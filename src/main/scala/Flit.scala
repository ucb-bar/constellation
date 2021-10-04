package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}

class Flit(implicit val p: Parameters) extends Bundle with HasAstroNoCParams{
  val head = Bool()
  val tail = Bool()
  val dest_id = UInt(idBits.W)
  val virt_channel_id = UInt(virtChannelBits.W)

  val payload = UInt(flitPayloadWidth.W)
}
