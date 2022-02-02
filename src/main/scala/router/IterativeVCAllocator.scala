package constellation.router

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

import constellation._

class IterativeVCAllocator(vP: VCAllocatorParams)(implicit p: Parameters) extends VCAllocator(vP)(p) {

  val arb = Module(new GrantHoldArbiter(Bool(), allInParams.size, (_: Bool) => true.B, rr=true))
  arb.io.in.foreach(_.bits := false.B)
  (arb.io.in zip io.req).foreach { case (l,r) =>
    l.valid := r.valid
    r.ready := l.ready
  }


  val sel = arb.io.chosen
  val sel_oh = UIntToOH(sel)
  val req_virt_channel = Mux1H(sel_oh, io.req.map(_.bits.in_virt_channel))
  val req_vc_sel = Mux1H(sel_oh, io.req.map(_.bits.vc_sel))

  val allocator = Module(new GrantHoldArbiter(Bool(), nOutChannels, (_: Bool) => true.B))
  allocator.io.in.foreach(_.bits := false.B)
  allocator.io.out.ready := true.B
  for (outId <- 0 until nAllOutputs) {
    for (outVirtId <- 0 until allOutParams(outId).nVirtualChannels) {
      val idx = getIdx(outId, outVirtId)
      allocator.io.in(idx).valid := (arb.io.out.valid &&
        req_vc_sel(outId)(outVirtId) &&
        io.channel_available(outId)(outVirtId))
      io.out_allocs(outId)(outVirtId) := allocator.io.in(idx).fire()
    }
  }

  arb.io.out.ready := allocator.io.out.valid

  (io.req zip io.resp).zipWithIndex.map { case ((req,resp),i) =>
    val fire_id = allocator.io.chosen
    val (out_id, out_virt_id) = getOutChannelInfo(fire_id)

    resp.valid := sel === i.U && allocator.io.out.valid
    resp.bits.in_virt_channel := req_virt_channel
    resp.bits.vc_sel.foreach(_.foreach(_ := false.B))
    for (o <- 0 until nAllOutputs) {
      when (out_id === o.U) {
        resp.bits.vc_sel(o)(out_virt_id) := true.B
      }
    }
  }
}
