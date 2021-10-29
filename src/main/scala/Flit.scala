package constellation

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}

class Flit(val cParam: ChannelParams)(implicit val p: Parameters) extends Bundle with HasChannelParams {
  val head = Bool()
  val tail = Bool()
  val vnet_id = UInt(vNetBits.W)
  val egress_id = UInt(egressIdBits.W)
  val virt_channel_id = UInt(virtualChannelBits.W)

  val payload = UInt(flitPayloadBits.W)
}
