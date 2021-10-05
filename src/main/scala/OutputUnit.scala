package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}

class OutputUnitAlloc(nInputs: Int)(implicit val p: Parameters) extends Bundle with HasAstroNoCParams {
  val in_channel = UInt(log2Ceil(nInputs).W)
  val in_virt_channel = UInt(virtChannelBits.W)
  val out_virt_channel = UInt(virtChannelBits.W)
}

class OutputUnit(inParams: Seq[ChannelParams], outParam: ChannelParams)(implicit val p: Parameters) extends Module with HasAstroNoCParams {
  val nInputs = inParams.size
  val io = IO(new Bundle {
    val in = Flipped(Valid(new Flit))
    val alloc = Flipped(Valid(new OutputUnitAlloc(inParams.size)))
    val credit_alloc = Input(Bool())
    val credit_free = Input(UInt((1+log2Ceil(maxFlits)).W))
    val credits = Output(UInt((1+log2Ceil(outParam.bufferSize)).W))
    val out = Valid(new Flit)
  })

  val g_i :: g_a :: g_c :: Nil = Enum(3)

  val c = UInt((1+log2Ceil(outParam.bufferSize)).W)
  class OutputState extends Bundle {
    val g = UInt(2.W)
    val i_p = UInt(log2Ceil(nInputs).W)
    val i_c = UInt(virtChannelBits.W)
  }

  val states = Reg(Vec(outParam.virtualChannels, new OutputState))

  when (io.alloc.fire()) {
    val id = io.alloc.bits.out_virt_channel
    assert(states(id).g === g_i)

    states(id).g := g_a
    states(id).i_p := io.alloc.bits.in_channel
    states(id).i_c := io.alloc.bits.in_virt_channel
  }

  c := c +& io.credit_free - io.credit_alloc

  when (reset.asBool) {
    states.foreach(_.g := g_i)
    c := outParam.bufferSize.U
  }
}
