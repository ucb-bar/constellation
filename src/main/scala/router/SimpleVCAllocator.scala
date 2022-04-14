package constellation.router

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

import constellation.util.{GrantHoldArbiter, ArbiterPolicy}

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

  val rank_1_arbs = Seq.fill(d0) { Module(new GrantHoldArbiter(Bool(), d1, (_: Bool) => true.B,
    policy = if (rrD0) ArbiterPolicy.RoundRobin else ArbiterPolicy.LowestFirst)) }
  val rank_2_arbs = Seq.fill(d1) { Module(new GrantHoldArbiter(Bool(), d0, (_: Bool) => true.B,
    policy = if (rrD1) ArbiterPolicy.RoundRobin else ArbiterPolicy.LowestFirst)) }

  Seq.tabulate(d0, d1) { case (y, x) =>
    rank_1_arbs(y).io.in(x).valid := in(y)(x).valid
    rank_1_arbs(y).io.in(x).bits := DontCare
    in(y)(x).ready := rank_1_arbs(y).io.in(x).ready

    rank_2_arbs(x).io.in(y).valid := (rank_1_arbs(y).io.out(0).valid &&
      rank_1_arbs(y).io.chosen(0) === x.U)
    rank_2_arbs(x).io.in(y).bits := DontCare
  }
  rank_1_arbs.zipWithIndex.map { case (a,y) =>
    a.io.out(0).ready := rank_2_arbs.map(_.io.in(y).fire()).reduce(_||_)
  }
  rank_2_arbs.foreach(_.io.out(0).ready := true.B)

  assert((0 until d0).map { y => PopCount(
    (0 until d1).map { x => io.in(y)(x).fire() }) <= 1.U }.reduce(_&&_))
  assert((0 until d1).map { x => PopCount(
    (0 until d0).map { y => io.in(y)(x).fire() }) <= 1.U }.reduce(_&&_))
}

class SimpleVCAllocator(vP: VCAllocatorParams)(implicit p: Parameters) extends VCAllocator(vP)(p) {

  val per_input_qs = allInParams.map { u => Module(new GrantHoldArbiter(
    new VCAllocReq(inParams, ingressParams, outParams, egressParams), u.nVirtualChannels,
    (x: VCAllocReq) => true.B,
    policy = ArbiterPolicy.RoundRobin))
  }
  (per_input_qs zip io.req).zipWithIndex.map { case ((q,req),i) =>
    (q.io.in zip req).zipWithIndex.map { case ((l,r),v) =>
      l.bits.vc_sel := r.bits.vc_sel
      l.bits.ingress_id := r.bits.ingress_id
      l.bits.egress_id := r.bits.egress_id
      l.bits.in_virt_channel := v.U
      l.bits.in_id := i.U
      l.valid := r.valid
      r.ready := l.ready
    }
  }
  val reqs = per_input_qs.map(_.io.out(0))

  io.resp.foreach(_.bits := DontCare)
  (io.resp zip reqs).map { case (o,i) => o.bits.in_virt_channel := i.bits.in_virt_channel }
  reqs.foreach { r => when (r.valid) { assert(r.bits.vc_sel.asUInt =/= 0.U) } }

  val allocator = Module(new Allocator(nOutChannels, nAllInputs))
  allocator.io.in.foreach(_.foreach(_.valid := false.B))

  for (outId <- 0 until nAllOutputs) {
    for (outVirtId <- 0 until allOutParams(outId).nVirtualChannels) {
      val idx = getIdx(outId, outVirtId)
      for (inId <- 0 until nAllInputs) {
        val r = reqs(inId)

        allocator.io.in(idx)(inId).valid := (r.valid &&
          r.bits.vc_sel(outId)(outVirtId) &&
          io.channel_status(outId)(outVirtId).available
        )
      }
      io.out_allocs(outId)(outVirtId).alloc := allocator.io.in(idx).map(_.fire()).reduce(_||_)
      io.out_allocs(outId)(outVirtId).ingress_id := Mux1H(
        allocator.io.in(idx).map(_.fire()),
        reqs.map(_.bits.ingress_id)
      )
      io.out_allocs(outId)(outVirtId).ingress_id := Mux1H(
        allocator.io.in(idx).map(_.fire()),
        reqs.map(_.bits.egress_id)
      )
    }
  }

  (reqs zip io.resp).zipWithIndex.map { case ((req,resp),i) =>
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
