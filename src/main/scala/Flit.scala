package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}

class Flit(implicit val p: Parameters) extends Bundle with HasAstroNoCParams {
  val head = Bool()
  val tail = Bool()
  val prio = UInt(prioBits.W)
  val out_id = UInt(outputIdBits.W)
  // virtual channel id when moving between nodes
  val virt_channel_id = UInt(virtChannelBits.W)

  val payload = UInt(flitPayloadBits.W)
}
