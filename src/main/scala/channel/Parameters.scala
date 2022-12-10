package constellation.channel

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy.{ClockCrossingType, NoCrossing}
import constellation.routing.{ChannelRoutingInfo, FlowRoutingInfo}
import constellation.noc.{HasNoCParams}

// BEGIN: FlowParams
case class FlowParams(
  ingressId: Int,
  egressId: Int,
  vNetId: Int
)
// END: FlowParams

// BEGIN: ChannelParams
case class UserChannelParams(
  virtualChannelParams: Seq[UserVirtualChannelParams] =
    Seq(UserVirtualChannelParams()),
  channelGen: Parameters => ChannelOutwardNode => ChannelOutwardNode =
    p => u => u,
  crossingType: ClockCrossingType = NoCrossing,
  useOutputQueues: Boolean = true,
  srcSpeedup: Int = 1,
  destSpeedup: Int = 1
) {
  val nVirtualChannels = virtualChannelParams.size
}

case class UserVirtualChannelParams(
  bufferSize: Int = 1
)

// END: ChannelParams

/** Represents an ingress into the network
 *
 *  @param destId node identifier for the network node this ingress connects to
 *  @param vNetId virtual network id
 *  @param payloadBits width of the wire providing the input for the ingress
 */
case class UserIngressParams(
  destId: Int,
  payloadBits: Int = 64,
)

/** Represents an egress from the network
 *
 *  @param srcId node identifier for the network node this egress connects to
 *  @param vNetId virtual network id
 *  @param payloadBits width of the wire coming out of the egress
 */
case class UserEgressParams(
  srcId: Int,
  payloadBits: Int = 64,
)


// Internal-facing params
case class VirtualChannelParams(
  src: Int,
  dst: Int,
  vc: Int,
  bufferSize: Int,
  possibleFlows: Set[FlowRoutingInfo],
) {
  val traversable = possibleFlows.size > 0
  require(bufferSize > 0, s"Illegal $this")
}


trait BaseChannelParams {
  def srcId: Int
  def destId: Int
  def possibleFlows: Set[FlowRoutingInfo]
  def nVirtualChannels: Int
  def channelRoutingInfos: Seq[ChannelRoutingInfo]
  def payloadBits: Int
  def srcSpeedup: Int
  def destSpeedup: Int
  def traversable: Boolean = possibleFlows.size > 0
}

trait TerminalChannelParams extends BaseChannelParams {
  def nVirtualChannels = 1
  def srcSpeedup = 1
  def destSpeedup = 1
}


case class ChannelParams(
  srcId: Int,
  destId: Int,
  payloadBits: Int,
  virtualChannelParams: Seq[VirtualChannelParams],
  srcSpeedup: Int,
  destSpeedup: Int,
  channelGen: Parameters => ChannelOutwardNode => ChannelOutwardNode = { p => u => u },
  useOutputQueues: Boolean,
) extends BaseChannelParams {
  val nVirtualChannels = virtualChannelParams.size
  val maxBufferSize = virtualChannelParams.map(_.bufferSize).max

  val possibleFlows = virtualChannelParams.map(_.possibleFlows).reduce(_++_)

  val channelRoutingInfos = (0 until nVirtualChannels).map(i => ChannelRoutingInfo(
    src=srcId,
    vc=i,
    dst=destId,
    n_vc=nVirtualChannels))
}

object ChannelParams {
  def apply(
    srcId: Int,
    destId: Int,
    payloadBits: Int,
    user: UserChannelParams,
  ): ChannelParams = {
    ChannelParams(
      srcId = srcId,
      destId = destId,
      payloadBits = payloadBits,
      srcSpeedup = user.srcSpeedup,
      destSpeedup = user.destSpeedup,
      channelGen = user.channelGen,
      virtualChannelParams = user.virtualChannelParams.zipWithIndex.map { case (vP, vc) =>
        VirtualChannelParams(srcId, destId, vc, vP.bufferSize, Set[FlowRoutingInfo]())
      },
      useOutputQueues = user.useOutputQueues
    )
  }
}

case class IngressChannelParams(
  ingressId: Int,
  destId: Int,
  possibleFlows: Set[FlowRoutingInfo],
  vNetId: Int,
  payloadBits: Int
) extends TerminalChannelParams {
  val srcId = -1
  val channelRoutingInfos = Seq(ChannelRoutingInfo(src=(-1), vc=0, dst=destId, n_vc=1))
}

object IngressChannelParams {
  def apply(
    ingressId: Int,
    flows: Seq[FlowRoutingInfo],
    user: UserIngressParams
  ): IngressChannelParams = {
    val ourFlows = flows.filter(_.ingressId == ingressId)
    val vNetIds = ourFlows.map(_.vNetId).toSet
    require(vNetIds.size <= 1)
    IngressChannelParams(
      ingressId = ingressId,
      vNetId = vNetIds.headOption.getOrElse(0),
      destId = user.destId,
      possibleFlows = ourFlows.toSet,
      payloadBits = user.payloadBits
    )
  }
}



case class EgressChannelParams(
  egressId: Int,
  possibleFlows: Set[FlowRoutingInfo],
  srcId: Int,
  payloadBits: Int
) extends TerminalChannelParams {
  val destId = -1
  val channelRoutingInfos = Seq(ChannelRoutingInfo(src=srcId, vc=0, dst=(-1), n_vc=1))
}

object EgressChannelParams {
  def apply(
    egressId: Int,
    flows: Seq[FlowRoutingInfo],
    user: UserEgressParams): EgressChannelParams = {
    val ourFlows = flows.filter(_.egressId == egressId)
    val vNetIds = ourFlows.map(_.vNetId).toSet
    require(vNetIds.size <= 1)
    EgressChannelParams(
      egressId = egressId,
      possibleFlows = ourFlows.toSet,
      srcId = user.srcId,
      payloadBits = user.payloadBits
    )
  }
}

