package constellation

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}

class Flit(val cParam: ChannelParams)(implicit val p: Parameters) extends Bundle with HasChannelParams {
  val head = Bool()
  val tail = Bool()
  val prio = UInt(prioBits.W)
  val out_id = UInt(outputIdBits.W)
  val virt_channel_id = UInt(virtualChannelBits.W)

  val payload = UInt(flitPayloadBits.W)
}
