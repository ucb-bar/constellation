package constellation.channel

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy.{ClockCrossingType, NoCrossing}
import constellation.routing.{ChannelRoutingInfo, PacketRoutingInfo}
import constellation.{HasNoCParams, NoCKey}


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
 *  @param possibleEgresses set of all egresses this ingress may route to
 *  @param vNetId virtual network id
 *  @param payloadBits width of the wire providing the input for the ingress
 */
case class UserIngressParams(
  destId: Int,
  possibleEgresses: Set[Int] = Set[Int](),
  vNetId: Int = 0,
  payloadBits: Int = 64
)

case class UserEgressParams(
  srcId: Int,
  payloadBits: Int = 64
)


// Internal-facing params
case class VirtualChannelParams(
  src: Int,
  dst: Int,
  vc: Int,
  bufferSize: Int,
  possiblePackets: Set[PacketRoutingInfo],
  uniqueId: Int,
) {
  val traversable = possiblePackets.size > 0
}


trait BaseChannelParams {
  def srcId: Int
  def destId: Int
  def possiblePackets: Set[PacketRoutingInfo]
  def nVirtualChannels: Int
  def channelRoutingInfos: Seq[ChannelRoutingInfo]
  def payloadBits: Int
  def srcMultiplier: Int
  def destMultiplier: Int
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
  destMultiplier: Int
)(implicit p: Parameters) extends BaseChannelParams {
  def nVirtualChannels = virtualChannelParams.size
  val maxBufferSize = virtualChannelParams.map(_.bufferSize).max

  def possiblePackets = virtualChannelParams.map(_.possiblePackets).reduce(_++_)
  val traversable = virtualChannelParams.map(_.traversable).reduce(_||_)

  def channelRoutingInfos = (0 until nVirtualChannels).map(i => ChannelRoutingInfo(srcId, i, destId, nVirtualChannels))
}

object ChannelParams {
  def apply(
    srcId: Int,
    destId: Int,
    payloadBits: Int,
    user: UserChannelParams,
    uniqueId: Int
  )(implicit p: Parameters): ChannelParams = {
    ChannelParams(
      srcId = srcId,
      destId = destId,
      payloadBits = payloadBits,
      srcMultiplier = user.srcMultiplier,
      destMultiplier = user.destMultiplier,
      virtualChannelParams = user.virtualChannelParams.zipWithIndex.map { case (vP, vc) =>
        VirtualChannelParams(srcId, destId, vc, vP.bufferSize, Set[PacketRoutingInfo](), uniqueId)
      }
    )
  }
}

case class IngressChannelParams(
  ingressId: Int,
  uniqueId: Int,
  destId: Int,
  possibleEgresses: Set[Int],
  vNetId: Int,
  payloadBits: Int
)(implicit p: Parameters) extends TerminalChannelParams {
  def srcId = -1
  def possiblePackets = possibleEgresses.map { e => PacketRoutingInfo(e, vNetId) }
  def channelRoutingInfos = Seq(ChannelRoutingInfo(-1, 0, destId, 1))
}

object IngressChannelParams {
  def apply(
    ingressId: Int,
    uniqueId: Int,
    user: UserIngressParams)(implicit p: Parameters): IngressChannelParams = {
    require(user.destId < p(NoCKey).topology.nNodes)
    IngressChannelParams(
      ingressId = ingressId,
      uniqueId = uniqueId,
      destId = user.destId,
      possibleEgresses = user.possibleEgresses,
      vNetId = user.vNetId,
      payloadBits = user.payloadBits
    )
  }
}



case class EgressChannelParams(
  egressId: Int,
  uniqueId: Int,
  possiblePackets: Set[PacketRoutingInfo],
  srcId: Int,
  payloadBits: Int
)(implicit p: Parameters) extends TerminalChannelParams {
  def destId = -1
  def channelRoutingInfos = Seq(ChannelRoutingInfo(srcId, 0, -1, 1))
}

object EgressChannelParams {
  def apply(
    egressId: Int,
    uniqueId: Int,
    possiblePackets: Set[PacketRoutingInfo],
    user: UserEgressParams)(implicit p: Parameters): EgressChannelParams = {
    require(user.srcId < p(NoCKey).topology.nNodes)
    EgressChannelParams(
      egressId = egressId,
      uniqueId = uniqueId,
      possiblePackets = possiblePackets,
      srcId = user.srcId,
      payloadBits = user.payloadBits
    )
  }
}
