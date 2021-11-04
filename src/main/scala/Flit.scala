package constellation

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}

class IOFlit(val cParam: ChannelParams)(implicit val p: Parameters) extends Bundle with HasChannelParams {
  val head = Bool()
  val tail = Bool()
  val egress_id = UInt(egressIdBits.W)
  val payload = UInt(flitPayloadBits.W)
}

class Flit(cParam: ChannelParams)(implicit p: Parameters) extends IOFlit(cParam)(p) {
  val vnet_id = UInt(vNetBits.W)
  val virt_channel_id = UInt(virtualChannelBits.W)
}
