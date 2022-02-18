package constellation.channel

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy.{ClockCrossingType, NoCrossing}
import constellation.routing.{ChannelRoutingInfo, PacketRoutingInfo}
import constellation.{HasNoCParams, NoCKey}

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
  val flit = Vec(cParam.srcMultiplier, Valid(new Flit(cParam)))
  val credit_return = Input(UInt(nVirtualChannels.W))
  val vc_free = Input(UInt(nVirtualChannels.W))
}

class TerminalChannel(val cParam: BaseChannelParams)(implicit val p: Parameters) extends Bundle with HasChannelParams {
  require(cParam match {
    case c: TerminalChannelParams => true
    case _ => false
  })

  val flit = Decoupled(new IOFlit(cParam))
}

