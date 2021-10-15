package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}

class OutputUnitAlloc(val inParams: Seq[ChannelParams], val terminalInParams: Seq[ChannelParams], val outParam: ChannelParams)(implicit val p: Parameters) extends Bundle with HasAstroNoCParams {
  val in_channel = UInt(log2Up(inParams.size + terminalInParams.size).W)
  val in_virt_channel = UInt(log2Up((inParams ++ terminalInParams).map(_.virtualChannelParams.size).max).W)
  val out_virt_channel = UInt(outParam.virtualChannelParams.size.W)
}

class AbstractOutputUnitIO(val inParams: Seq[ChannelParams], val terminalInParams: Seq[ChannelParams], val outParam: ChannelParams)(implicit val p: Parameters) extends Bundle {
  val nVirtualChannels = outParam.virtualChannelParams.size
  val in = Flipped(Valid(new Flit))
  val credit_available = Output(Vec(nVirtualChannels, Bool()))
  val channel_available = Output(Vec(nVirtualChannels, Bool()))
  val alloc = Flipped(Valid(new OutputUnitAlloc(inParams, terminalInParams, outParam)))
}

abstract class AbstractOutputUnit(inParams: Seq[ChannelParams], terminalInParams: Seq[ChannelParams], outParam: ChannelParams)(implicit val p: Parameters) extends Module with HasAstroNoCParams {
  def io: AbstractOutputUnitIO
}

class OutputUnit(inParams: Seq[ChannelParams], terminalInParams: Seq[ChannelParams], outParam: ChannelParams)(implicit p: Parameters) extends AbstractOutputUnit(inParams, terminalInParams, outParam)(p) {
  val nInputs = inParams.size
  val nVirtualChannels = outParam.virtualChannelParams.size
  val io = IO(new AbstractOutputUnitIO(inParams, terminalInParams, outParam) {
    val out = new Channel(outParam)
    val credit_alloc = Input(Valid(UInt(log2Up(nVirtualChannels).W)))
  })

  val g_i :: g_a :: g_c :: Nil = Enum(3)

  class OutputState(val bufferSize: Int) extends Bundle {
    val g = UInt(2.W)
    val i_p = UInt(log2Up(nInputs).W)
    val i_c = UInt(log2Up((inParams ++ terminalInParams).map(_.virtualChannelParams.size).max).W)
    val c = UInt(log2Up(1+bufferSize).W)
  }

  val states = Reg(MixedVec(outParam.virtualChannelParams.map { u => new OutputState(u.bufferSize) }))
  (states zip io.channel_available).map { case (s,a) => a := s.g === g_i }
  io.out.flit := io.in

  when (io.out.vc_free.fire()) {
    states.zipWithIndex.map { case (s,i) =>
      when (io.out.vc_free.bits === i.U) {
        assert(s.g =/= g_i)
        s.g := g_i
        io.channel_available(i) := true.B
      }
    }
  }

  when (io.alloc.fire()) {
    val id = io.alloc.bits.out_virt_channel
    states.zipWithIndex.map { case (s,i) =>
      when (id === i.U) {
        s.g := g_a
        s.i_p := io.alloc.bits.in_channel
        s.i_c := io.alloc.bits.in_virt_channel
      }
    }
  }

  (io.credit_available zip states).zipWithIndex.map { case ((c,s),i) =>
    c := s.c =/= 0.U || (io.out.credit_return.valid && io.out.credit_return.bits === i.U)
  }

  states.zipWithIndex.map { case (s,i) =>
    val free = (io.out.credit_return.valid && io.out.credit_return.bits === i.U)
    val alloc = (io.credit_alloc.valid && io.credit_alloc.bits === i.U)
    s.c := s.c +& free - alloc
  }



  when (reset.asBool) {
    states.foreach(_.g := g_i)
    states.foreach(s => s.c := s.bufferSize.U)
  }
}
