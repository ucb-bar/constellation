package constellation

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}

class AbstractOutputUnitIO(
  val inParams: Seq[ChannelParams],
  val terminalInParams: Seq[ChannelParams],
  val cParam: ChannelParams
)(implicit val p: Parameters) extends Bundle with HasRouterInputParams with HasChannelParams {
  val nodeId = cParam.srcId

  val in = Flipped(Valid(new Flit(cParam)))
  val credit_available = Output(Vec(nVirtualChannels, Bool()))
  val channel_available = Output(Vec(nVirtualChannels, Bool()))
  val allocs = Input(Vec(nVirtualChannels, Bool()))
}

abstract class AbstractOutputUnit(
  val inParams: Seq[ChannelParams],
  val terminalInParams: Seq[ChannelParams],
  val cParam: ChannelParams
)(implicit val p: Parameters) extends Module with HasRouterInputParams with HasChannelParams {
  val nodeId = cParam.srcId

  def io: AbstractOutputUnitIO
}

class OutputUnit(inParams: Seq[ChannelParams], terminalInParams: Seq[ChannelParams], cParam: ChannelParams)
  (implicit p: Parameters) extends AbstractOutputUnit(inParams, terminalInParams, cParam)(p) {

  val io = IO(new AbstractOutputUnitIO(inParams, terminalInParams, cParam) {
    val out = new Channel(cParam)
    val credit_alloc = Input(Valid(UInt(virtualChannelBits.W)))
  })

  val g_i :: g_a :: g_c :: Nil = Enum(3)

  class OutputState(val bufferSize: Int) extends Bundle {
    val g = UInt(2.W)
    val i_p = UInt(log2Up(nInputs).W)
    val i_c = UInt(log2Up(allInParams.map(_.virtualChannelParams.size).max).W)
    val c = UInt(log2Up(1+bufferSize).W)
  }

  val states = Reg(MixedVec(virtualChannelParams.map { u => new OutputState(u.bufferSize) }))
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

  (states zip io.allocs).map { case (s,a) =>
    when (a) { s.g := g_a }
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
