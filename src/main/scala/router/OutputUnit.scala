package constellation.router

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import constellation.channel._

class OutputCreditAlloc extends Bundle {
  val alloc = Bool()
  val tail = Bool()
}

class AbstractOutputUnitIO(
  val inParams: Seq[ChannelParams],
  val ingressParams: Seq[IngressChannelParams],
  val cParam: BaseChannelParams
)(implicit val p: Parameters) extends Bundle with HasRouterInputParams with HasChannelParams {
  val nodeId = cParam.srcId

  val in = Flipped(Vec(cParam.srcMultiplier, Valid(new Flit(cParam))))
  val credit_available = Output(Vec(nVirtualChannels, Bool()))
  val channel_available = Output(Vec(nVirtualChannels, Bool()))
  val allocs = Input(Vec(nVirtualChannels, Bool()))
  val credit_alloc = Input(Vec(nVirtualChannels, new OutputCreditAlloc))
}


abstract class AbstractOutputUnit(
  val inParams: Seq[ChannelParams],
  val ingressParams: Seq[IngressChannelParams],
  val cParam: BaseChannelParams
)(implicit val p: Parameters) extends Module with HasRouterInputParams with HasChannelParams {
  val nodeId = cParam.srcId

  def io: AbstractOutputUnitIO
}

class OutputUnit(inParams: Seq[ChannelParams], ingressParams: Seq[IngressChannelParams], cParam: ChannelParams)
  (implicit p: Parameters) extends AbstractOutputUnit(inParams, ingressParams, cParam)(p) {

  val io = IO(new AbstractOutputUnitIO(inParams, ingressParams, cParam) {
    val out = new Channel(cParam.asInstanceOf[ChannelParams])
  })

  val g_i :: g_a :: g_c :: Nil = Enum(3)

  class OutputState(val bufferSize: Int) extends Bundle {
    val g = UInt(2.W)
    val c = UInt(log2Up(1+bufferSize).W)
  }

  val states = Reg(MixedVec(virtualChannelParams.map { u => new OutputState(u.bufferSize) }))
  (states zip io.channel_available).map { case (s,a) => a := s.g === g_i }
  io.out.flit := io.in

  states.zipWithIndex.map { case (s,i) => if (virtualChannelParams(i).traversable) {
    when (io.out.vc_free(i)) {
      assert(s.g =/= g_i)
      s.g := g_i
      io.channel_available(i) := true.B
    }
  } }


  (states zip io.allocs).zipWithIndex.map { case ((s,a),i) => if (virtualChannelParams(i).traversable) {
    when (a) { s.g := g_a }
  } }

  (io.credit_available zip states).zipWithIndex.map { case ((c,s),i) =>
    c := s.c =/= 0.U //|| (io.out.credit_return.valid && io.out.credit_return.bits === i.U)
  }

  states.zipWithIndex.map { case (s,i) =>
    val free = io.out.credit_return(i)
    val alloc = io.credit_alloc(i).alloc
    if (virtualChannelParams(i).traversable) {
      s.c := s.c +& free - alloc
    }
  }



  when (reset.asBool) {
    states.foreach(_.g := g_i)
    states.foreach(s => s.c := s.bufferSize.U)
  }
}
