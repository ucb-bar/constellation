package constellation.router

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._
import freechips.rocketchip.rocket.{DecodeLogic}

import constellation._

class VCAllocReq(val cParam: BaseChannelParams, val outParams: Seq[ChannelParams], val egressParams: Seq[EgressChannelParams])
  (implicit val p: Parameters) extends Bundle with HasChannelParams with HasRouterOutputParams{
  val in_virt_channel = UInt(virtualChannelBits.W)
  val vc_sel = MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool()) })
}

class VCAllocResp(val cParam: BaseChannelParams, val outParams: Seq[ChannelParams], val egressParams: Seq[EgressChannelParams])(implicit val p: Parameters) extends Bundle with HasChannelParams with HasRouterOutputParams {
  val in_virt_channel = UInt(virtualChannelBits.W)
  val vc_sel = MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool()) })
}

class Allocator(d0: Int, d1: Int,
  revD0: Boolean = false, revD1: Boolean = false,
  rrD0: Boolean = false, rrD1: Boolean = false
)
    extends Module {

  class ValidReady extends Bundle {
    val valid = Input(Bool())
    val ready = Output(Bool())
    def fire() = valid && ready
  }

  val io = IO(new Bundle {
    val in = Vec(d0, Vec(d1, new ValidReady))
  })

  val in = Wire(Vec(d0, Vec(d1, new ValidReady)))
  (io.in zip (if (revD0) in.reverse else in)).map { case (io_row, in_row) =>
    (io_row zip (if (revD1) in_row.reverse else in_row)).map { case (io_e, in_e) =>
      io_e <> in_e
    }
  }

  val rank_1_arbs = Seq.fill(d0) { Module(new GrantHoldArbiter(Bool(), d1, (_: Bool) => true.B, rr = rrD0)) }
  val rank_2_arbs = Seq.fill(d1) { Module(new GrantHoldArbiter(Bool(), d0, (_: Bool) => true.B, rr = rrD1)) }

  Seq.tabulate(d0, d1) { case (y, x) =>
    rank_1_arbs(y).io.in(x).valid := in(y)(x).valid
    rank_1_arbs(y).io.in(x).bits := DontCare
    in(y)(x).ready := rank_1_arbs(y).io.in(x).ready

    rank_2_arbs(x).io.in(y).valid := (rank_1_arbs(y).io.out.valid &&
      rank_1_arbs(y).io.chosen === x.U)
    rank_2_arbs(x).io.in(y).bits := DontCare
  }
  rank_1_arbs.zipWithIndex.map { case (a,y) =>
    a.io.out.ready := rank_2_arbs.map(_.io.in(y).fire()).reduce(_||_)
  }
  rank_2_arbs.foreach(_.io.out.ready := true.B)

  assert((0 until d0).map { y => PopCount(
    (0 until d1).map { x => io.in(y)(x).fire() }) <= 1.U }.reduce(_&&_))
  assert((0 until d1).map { x => PopCount(
    (0 until d0).map { y => io.in(y)(x).fire() }) <= 1.U }.reduce(_&&_))
}

class VCAllocator(
  val routerParams: RouterParams,
  val inParams: Seq[ChannelParams],
  val outParams: Seq[ChannelParams],
  val ingressParams: Seq[IngressChannelParams],
  val egressParams: Seq[EgressChannelParams]
)(implicit val p: Parameters) extends Module
    with HasRouterParams {
  val io = IO(new Bundle {
    val req = MixedVec(allInParams.map { u =>
      Flipped(Decoupled(new VCAllocReq(u, outParams, egressParams))) })
    val resp = MixedVec(allInParams.map { u =>
      Valid(new VCAllocResp(u, outParams, egressParams)) })

    val channel_available = MixedVec(allOutParams.map { u =>
      Vec(u.nVirtualChannels, Input(Bool())) })
    val out_allocs = MixedVec(allOutParams.map { u =>
      Vec(u.nVirtualChannels, Output(Bool())) })
  })
  val nOutChannels = allOutParams.map(_.nVirtualChannels).sum

  io.resp.foreach(_.bits := DontCare)
  (io.resp zip io.req).map { case (o,i) => o.bits.in_virt_channel := i.bits.in_virt_channel }
  io.req.foreach { r => when (r.valid) { assert(r.bits.vc_sel.asUInt =/= 0.U) } }

  val allocator = Module(new Allocator(nOutChannels, nAllInputs))
  allocator.io.in.foreach(_.foreach(_.valid := false.B))

  def getIdx(outChannel: Int, outVirtChannel: Int): Int = {
    require(outChannel < nAllOutputs &&
      outVirtChannel < allOutParams(outChannel).nVirtualChannels)
    if (outChannel < nOutputs) {
      (0 until outVirtChannel).map(v =>
        outParams.count(_.nVirtualChannels > v)
      ).sum + outParams.take(outChannel).count(_.nVirtualChannels > outVirtChannel)
    } else {
      require(outVirtChannel == 0)
      outParams.map(_.nVirtualChannels).sum + outChannel - nOutputs
    }
  }
  def getOutChannelInfo(idx: Int): (Int, Int) = {
    for (outId <- 0 until nAllOutputs)
      for (outVirtId <- 0 until allOutParams(outId).nVirtualChannels)
        if (getIdx(outId, outVirtId) == idx) return (outId, outVirtId)
    require(false)
    return (-1, -1)
  }
  def getOutChannelInfo(idx: UInt): (UInt, UInt) = {
    val outId = MuxLookup(idx, 0.U(1.W), (0 until nOutChannels).map(i =>
      i.U -> getOutChannelInfo(i)._1.U
    ))
    val outVirtId = MuxLookup(idx, 0.U(1.W), (0 until nOutChannels).map(i =>
      i.U -> getOutChannelInfo(i)._2.U
    ))
    (outId, outVirtId)
  }

  for (outId <- 0 until nAllOutputs) {
    for (outVirtId <- 0 until allOutParams(outId).nVirtualChannels) {
      val idx = getIdx(outId, outVirtId)
      for (inId <- 0 until nAllInputs) {
        val r = io.req(inId)

        allocator.io.in(idx)(inId).valid := (r.valid &&
          r.bits.vc_sel(outId)(outVirtId) &&
          io.channel_available(outId)(outVirtId)
        )
      }
      io.out_allocs(outId)(outVirtId) := allocator.io.in(idx).map(_.fire()).reduce(_||_)
    }
  }

  (io.req zip io.resp).zipWithIndex.map { case ((req,resp),i) =>
    val fires = allocator.io.in.map(_(i).fire())
    val fire_id = OHToUInt(fires)
    val (out_id, out_virt_id) = getOutChannelInfo(fire_id)
    assert(PopCount(fires) <= 1.U)
    req.ready := fires.reduce(_||_)
    resp.valid := fires.reduce(_||_)
    resp.bits.in_virt_channel := req.bits.in_virt_channel
    resp.bits.vc_sel.foreach(_.foreach(_ := false.B))
    for (o <- 0 until nAllOutputs) {
      when (out_id === o.U) {
        resp.bits.vc_sel(o)(out_virt_id) := true.B
      }
    }
  }
}
