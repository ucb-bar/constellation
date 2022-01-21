package constellation

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy.{OutwardNodeHandle}
import constellation.routing.ChannelInfoForRouting


// User-facing params, for adjusting config options
case class UserVirtualChannelParams(
  bufferSize: Int = 1
)

case class UserChannelParams(
  virtualChannelParams: Seq[UserVirtualChannelParams] = Seq(UserVirtualChannelParams()),
  channel: Parameters => OutwardNodeHandle[ChannelParams, ChannelParams, ChannelEdgeParams, Channel] => OutwardNodeHandle[ChannelParams, ChannelParams, ChannelEdgeParams, Channel] = { p => u => u }
) {
  val nVirtualChannels = virtualChannelParams.size
}

case class UserIngressParams(
  destId: Int,
  possibleEgresses: Set[Int],
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
  possiblePackets: Set[PacketInfo],
  uniqueId: Int,
) {
  val traversable = possiblePackets.size > 0
  def asChannelInfoForRouting: ChannelInfoForRouting = ChannelInfoForRouting(src, vc, dst)
}

trait BaseChannelParams {
  def srcId: Int
  def destId: Int
  def possiblePackets: Set[PacketInfo]
  def nVirtualChannels: Int
  def channelInfosForRouting: Seq[ChannelInfoForRouting]
  def payloadBits: Int
}

trait TerminalChannelParams extends BaseChannelParams {
  def nVirtualChannels = 1
}


case class ChannelParams(
  srcId: Int,
  destId: Int,
  payloadBits: Int,
  virtualChannelParams: Seq[VirtualChannelParams],
) extends BaseChannelParams {
  def nVirtualChannels = virtualChannelParams.size
  val maxBufferSize = virtualChannelParams.map(_.bufferSize).max

  def possiblePackets = virtualChannelParams.map(_.possiblePackets).reduce(_++_)
  val traversable = virtualChannelParams.map(_.traversable).reduce(_||_)

  def channelInfosForRouting = virtualChannelParams.map(_.asChannelInfoForRouting)
}

case class IngressChannelParams(
  ingressId: Int,
  uniqueId: Int,
  destId: Int,
  possibleEgresses: Set[Int],
  vNetId: Int,
  payloadBits: Int
) extends TerminalChannelParams {
  def srcId = -1
  def possiblePackets = possibleEgresses.map { e => PacketInfo(e, vNetId) }
  def channelInfosForRouting = Seq(ChannelInfoForRouting(-1, 0, destId))
}

object IngressChannelParams {
  def apply(ingressId: Int, uniqueId: Int, user: UserIngressParams): IngressChannelParams =
    IngressChannelParams(
      ingressId = ingressId,
      uniqueId = uniqueId,
      destId = user.destId,
      possibleEgresses = user.possibleEgresses,
      vNetId = user.vNetId,
      payloadBits = user.payloadBits
    )
}



case class EgressChannelParams(
  egressId: Int,
  uniqueId: Int,
  possiblePackets: Set[PacketInfo],
  srcId: Int,
  payloadBits: Int
) extends TerminalChannelParams {
  def destId = -1
  def channelInfosForRouting = Seq(ChannelInfoForRouting(srcId, 0, -1))
}

object EgressChannelParams {
  def apply(
    egressId: Int, uniqueId: Int, possiblePackets: Set[PacketInfo], user: UserEgressParams): EgressChannelParams =
    EgressChannelParams(
      egressId = egressId,
      uniqueId = uniqueId,
      possiblePackets = possiblePackets,
      srcId = user.srcId,
      payloadBits = user.payloadBits
    )
}



trait HasChannelParams extends HasNoCParams {
  val cParam: BaseChannelParams

  val payloadBits = cParam.payloadBits
  val nVirtualChannels = cParam.nVirtualChannels
  val virtualChannelBits = log2Up(nVirtualChannels)
  def virtualChannelParams = cParam match {
    case c: ChannelParams         => c.virtualChannelParams
    case c: TerminalChannelParams => require(false); Nil;
  }
  def maxBufferSize = virtualChannelParams.map(_.bufferSize).max
}

class Channel(val cParam: ChannelParams)(implicit val p: Parameters) extends Bundle with HasChannelParams {
  val flit = Valid(new Flit(cParam))
  val credit_return = Input(Valid(UInt(virtualChannelBits.W)))
  val vc_free = Input(Valid(UInt(virtualChannelBits.W)))
}

class TerminalChannel(val cParam: BaseChannelParams)(implicit val p: Parameters) extends Bundle with HasChannelParams {
  require(cParam match {
    case c: TerminalChannelParams => true
    case _ => false
  })

  val flit = Decoupled(new IOFlit(cParam))
}

