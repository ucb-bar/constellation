package constellation.channel

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy.{ClockCrossingType, NoCrossing}
import constellation.routing.{ChannelRoutingInfo}
import constellation.noc.{HasNoCParams}

class Channel(val cParam: ChannelParams)(implicit val p: Parameters) extends Bundle {
  val nVirtualChannels = cParam.nVirtualChannels
  val flit = Vec(cParam.srcSpeedup, Valid(new Flit(cParam.payloadBits)))
  val credit_return = Input(UInt(cParam.nVirtualChannels.W))
  val vc_free = Input(UInt(cParam.nVirtualChannels.W))
}

class IngressChannel(val cParam: BaseChannelParams)(implicit val p: Parameters) extends Bundle {
  val payloadBits = cParam.payloadBits
  require(cParam.isInstanceOf[IngressChannelParams])
  val flit = Irrevocable(new IngressFlit(cParam.payloadBits))
}

class EgressChannel(val cParam: BaseChannelParams)(implicit val p: Parameters) extends Bundle {
  val payloadBits = cParam.payloadBits
  require(cParam.isInstanceOf[EgressChannelParams])
  val flit = Irrevocable(new EgressFlit(cParam.payloadBits))
}
