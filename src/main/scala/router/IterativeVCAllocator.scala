package constellation.router

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

import constellation.util.{GrantHoldArbiter, ArbiterPolicy}

class IterativeVCAllocator(vP: VCAllocatorParams)(implicit p: Parameters) extends VCAllocator(vP)(p) {
  val arb = Module(new GrantHoldArbiter(
    new VCAllocReq(inParams, ingressParams, outParams, egressParams),
    allInParams.map(_.nVirtualChannels).sum,
    (_: VCAllocReq) => true.B,
    policy = ArbiterPolicy.RoundRobin
  ))

  var t = 0
  io.req.zipWithIndex.map { case (req,i) =>
    req.zipWithIndex.map { case (r,v) =>
      arb.io.in(t).valid := r.valid
      r.ready := arb.io.in(t).ready
      arb.io.in(t).bits.in_id := i.U
      arb.io.in(t).bits.in_virt_channel := v.U
      arb.io.in(t).bits.vc_sel := r.bits.vc_sel
      arb.io.in(t).bits.flow := r.bits.flow
      t += 1
    }
  }

  val allocator = Module(new GrantHoldArbiter(Bool(), nOutChannels, (_: Bool) => true.B,
    policy=ArbiterPolicy.RoundRobin
  ))
  allocator.io.in.foreach(_.bits := false.B)
  allocator.io.prios.foreach(_ := 0.U)
  allocator.io.out(0).ready := true.B
  for (outId <- 0 until nAllOutputs) {
    for (outVirtId <- 0 until allOutParams(outId).nVirtualChannels) {
      val idx = getIdx(outId, outVirtId)
      allocator.io.in(idx).valid := (arb.io.out(0).valid &&
        arb.io.out(0).bits.vc_sel(outId)(outVirtId) &&
        io.channel_status(outId)(outVirtId).available)
      io.out_allocs(outId)(outVirtId).alloc := allocator.io.in(idx).fire()
      io.out_allocs(outId)(outVirtId).flow := arb.io.out(0).bits.flow
    }
  }

  arb.io.out(0).ready := allocator.io.out(0).valid

  t = 0
  io.resp.zipWithIndex.map { case (resp,i) =>
    val fire_id = allocator.io.chosen(0)
    val (out_id, out_virt_id) = getOutChannelInfo(fire_id)

    resp.valid := allocator.io.out(0).valid && arb.io.out(0).bits.in_id === i.U
    resp.bits.in_virt_channel := arb.io.out(0).bits.in_virt_channel
    resp.bits.vc_sel.foreach(_.foreach(_ := false.B))
    for (o <- 0 until nAllOutputs) {
      when (out_id === o.U) {
        resp.bits.vc_sel(o)(out_virt_id) := true.B
      }
    }
    t += allInParams(i).nVirtualChannels
  }
}
