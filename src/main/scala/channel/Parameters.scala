package constellation.channel

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy.{ClockCrossingType, NoCrossing}
import constellation.routing.{ChannelRoutingInfo, FlowRoutingInfo}
import constellation.noc.{HasNoCParams, NoCKey}


case class FlowParams(
  ingressId: Int,
  egressId: Int,
  vNetId: Int
)

// User-facing params, for adjusting config options
case class UserVirtualChannelParams(
  bufferSize: Int = 1
)

case class UserChannelParams(
  virtualChannelParams: Seq[UserVirtualChannelParams] = Seq(UserVirtualChannelParams()),
  channelGen: Parameters => ChannelOutwardNode => ChannelOutwardNode = { p => u => u },
  crossingType: ClockCrossingType = NoCrossing,
  srcMultiplier: Int = 1,
  destMultiplier: Int = 1
) {
  val nVirtualChannels = virtualChannelParams.size
}

/** Represents an ingress into the network
 *
 *  @param destId node identifier for the network node this ingress connects to
 *  @param vNetId virtual network id
 *  @param payloadBits width of the wire providing the input for the ingress
 */
case class UserIngressParams(
  destId: Int,
  vNetId: Int = 0,
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
  vNetId: Int = 0,
  payloadBits: Int = 64,
)


// Internal-facing params
case class VirtualChannelParams(
  src: Int,
  dst: Int,
  vc: Int,
  bufferSize: Int,
  possibleFlows: Set[FlowRoutingInfo],
  uniqueId: Int,
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
  def srcMultiplier: Int
  def destMultiplier: Int
  def traversable: Boolean = possibleFlows.size > 0
}

trait TerminalChannelParams extends BaseChannelParams {
  def nVirtualChannels = 1
  def srcMultiplier = 1
  def destMultiplier = 1
}


case class ChannelParams(
  srcId: Int,
  destId: Int,
  payloadBits: Int,
  virtualChannelParams: Seq[VirtualChannelParams],
  srcMultiplier: Int,
  destMultiplier: Int,
  channelGen: Parameters => ChannelOutwardNode => ChannelOutwardNode = { p => u => u }
) extends BaseChannelParams {
  val nVirtualChannels = virtualChannelParams.size
  val maxBufferSize = virtualChannelParams.map(_.bufferSize).max

  val possibleFlows = virtualChannelParams.map(_.possibleFlows).reduce(_++_)

  val channelRoutingInfos = (0 until nVirtualChannels).map(i => ChannelRoutingInfo(srcId, i, destId, nVirtualChannels))
}

object ChannelParams {
  def apply(
    srcId: Int,
    destId: Int,
    payloadBits: Int,
    user: UserChannelParams,
    uniqueId: Int
  ): ChannelParams = {
    ChannelParams(
      srcId = srcId,
      destId = destId,
      payloadBits = payloadBits,
      srcMultiplier = user.srcMultiplier,
      destMultiplier = user.destMultiplier,
      channelGen = user.channelGen,
      virtualChannelParams = user.virtualChannelParams.zipWithIndex.map { case (vP, vc) =>
        VirtualChannelParams(srcId, destId, vc, vP.bufferSize, Set[FlowRoutingInfo](), uniqueId)
      }
    )
  }
}

case class IngressChannelParams(
  ingressId: Int,
  uniqueId: Int,
  destId: Int,
  possibleFlows: Set[FlowRoutingInfo],
  vNetId: Int,
  payloadBits: Int
) extends TerminalChannelParams {
  val srcId = -1
  val channelRoutingInfos = Seq(ChannelRoutingInfo(-1, 0, destId, 1))
}

object IngressChannelParams {
  def apply(
    ingressId: Int,
    uniqueId: Int,
    flows: Seq[FlowParams],
    user: UserIngressParams,
    egresses: Seq[UserEgressParams]
  ): IngressChannelParams = {
    val ourFlows = flows.filter(_.ingressId == ingressId)
    ourFlows.foreach(f => require(f.vNetId == user.vNetId && f.vNetId == egresses(f.egressId).vNetId))
    IngressChannelParams(
      ingressId = ingressId,
      uniqueId = uniqueId,
      destId = user.destId,
      possibleFlows = ourFlows.map(f =>
        FlowRoutingInfo(f.ingressId, f.egressId, user.vNetId, egresses(f.egressId).srcId)
      ).toSet,
      vNetId = user.vNetId,
      payloadBits = user.payloadBits
    )
  }
}



case class EgressChannelParams(
  egressId: Int,
  uniqueId: Int,
  possibleFlows: Set[FlowRoutingInfo],
  srcId: Int,
  payloadBits: Int
) extends TerminalChannelParams {
  val destId = -1
  val channelRoutingInfos = Seq(ChannelRoutingInfo(srcId, 0, -1, 1))
}

object EgressChannelParams {
  def apply(
    egressId: Int,
    uniqueId: Int,
    flows: Seq[FlowParams],
    user: UserEgressParams): EgressChannelParams = {
    val ourFlows = flows.filter(_.egressId == egressId)
    val vNetIds = ourFlows.map(_.vNetId).toSet
    require(vNetIds.size <= 1)
    if (ourFlows.size > 0)
      require(vNetIds.head == user.vNetId)
    EgressChannelParams(
      egressId = egressId,
      uniqueId = uniqueId,
      possibleFlows = ourFlows.map(f =>
        FlowRoutingInfo(f.ingressId, egressId, vNetIds.head, user.srcId)
      ).toSet,
      srcId = user.srcId,
      payloadBits = user.payloadBits
    )
  }
}
