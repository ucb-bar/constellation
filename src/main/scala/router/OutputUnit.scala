package constellation.router

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import constellation.channel._
import constellation.routing.{FlowRoutingBundle}
import constellation.noc.{HasNoCParams}

class OutputCreditAlloc extends Bundle {
  val alloc = Bool()
  val tail = Bool()
}

class OutputChannelStatus(implicit val p: Parameters) extends Bundle with HasNoCParams {
  val occupied = Bool()
  def available = !occupied
  val flow = new FlowRoutingBundle
}

class OutputChannelAlloc(implicit val p: Parameters) extends Bundle with HasNoCParams {
  val alloc = Bool()
  val flow = new FlowRoutingBundle
}

class AbstractOutputUnitIO(
  val inParams: Seq[ChannelParams],
  val ingressParams: Seq[IngressChannelParams],
  val cParam: BaseChannelParams
)(implicit val p: Parameters) extends Bundle with HasRouterInputParams {
  val nodeId = cParam.srcId
  val nVirtualChannels = cParam.nVirtualChannels
  val in = Flipped(Vec(cParam.srcSpeedup, Valid(new Flit(cParam.payloadBits))))
  val credit_available = Output(Vec(nVirtualChannels, Bool()))
  val channel_status = Output(Vec(nVirtualChannels, new OutputChannelStatus))
  val allocs = Input(Vec(nVirtualChannels, new OutputChannelAlloc))
  val credit_alloc = Input(Vec(nVirtualChannels, new OutputCreditAlloc))
}


abstract class AbstractOutputUnit(
  val inParams: Seq[ChannelParams],
  val ingressParams: Seq[IngressChannelParams],
  val cParam: BaseChannelParams
)(implicit val p: Parameters) extends Module with HasRouterInputParams with HasNoCParams {
  val nodeId = cParam.srcId

  def io: AbstractOutputUnitIO
}

class OutputUnit(inParams: Seq[ChannelParams], ingressParams: Seq[IngressChannelParams], cParam: ChannelParams)
  (implicit p: Parameters) extends AbstractOutputUnit(inParams, ingressParams, cParam)(p) {

  class OutputUnitIO extends AbstractOutputUnitIO(inParams, ingressParams, cParam) {
    val out = new Channel(cParam.asInstanceOf[ChannelParams])
  }
  val io = IO(new OutputUnitIO)

  class OutputState(val bufferSize: Int) extends Bundle {
    val occupied = Bool()
    val c = UInt(log2Up(1+bufferSize).W)
    val flow = new FlowRoutingBundle
  }

  val states = Reg(MixedVec(cParam.virtualChannelParams.map { u => new OutputState(u.bufferSize) }))
  (states zip io.channel_status).map { case (s,a) =>
    a.occupied := s.occupied
    a.flow := s.flow
  }
  io.out.flit := io.in

  states.zipWithIndex.map { case (s,i) => if (cParam.virtualChannelParams(i).traversable) {
    when (io.out.vc_free(i)) {
      assert(s.occupied)
      s.occupied := false.B
      io.channel_status(i).occupied := false.B
    }
  } }


  (states zip io.allocs).zipWithIndex.map { case ((s,a),i) => if (cParam.virtualChannelParams(i).traversable) {
    when (a.alloc) {
      s.occupied := true.B
      s.flow := a.flow
    }
  } }

  (io.credit_available zip states).zipWithIndex.map { case ((c,s),i) =>
    c := s.c =/= 0.U //|| (io.out.credit_return.valid && io.out.credit_return.bits === i.U)
  }

  states.zipWithIndex.map { case (s,i) =>
    val free = io.out.credit_return(i)
    val alloc = io.credit_alloc(i).alloc
    if (cParam.virtualChannelParams(i).traversable) {
      s.c := s.c +& free - alloc
    }
  }



  when (reset.asBool) {
    states.foreach(_.occupied := false.B)
    states.foreach(s => s.c := s.bufferSize.U)
  }
}
