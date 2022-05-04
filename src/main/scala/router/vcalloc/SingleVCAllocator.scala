package constellation.router

import chisel3._
import chisel3.util._
import chisel3.util.random.{LFSR}

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

import constellation.util.{GrantHoldArbiter, ArbiterPolicy}
import constellation.channel._
import constellation.routing.{ChannelRoutingInfo, FlowRoutingBundle}

// Allocates 1 VC per cycle
abstract class SingleVCAllocator(vP: VCAllocatorParams)(implicit p: Parameters) extends VCAllocator(vP)(p) {
  // get single input
  val mask = RegInit(0.U(allInParams.size.W))
  val in_arb_reqs = Wire(Vec(allInParams.size, MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool()) })))
  val in_arb_vals = Wire(Vec(allInParams.size, Bool()))
  val in_arb_filter = PriorityEncoderOH(Cat(in_arb_vals.asUInt, in_arb_vals.asUInt & ~mask))
  val in_arb_sel = (in_arb_filter(allInParams.size-1,0) | (in_arb_filter >> allInParams.size))
  when (io.req.map(_.valid).orR) {
    mask := Mux1H(in_arb_sel, (0 until allInParams.size).map { w => ~(0.U((w+1).W)) })
  }

  for (i <- 0 until allInParams.size) {
    (0 until allOutParams.size).map { m =>
      (0 until allOutParams(m).nVirtualChannels).map { n =>
        in_arb_reqs(i)(m)(n) := io.req(i).bits.vc_sel(m)(n) && !io.channel_status(m)(n).occupied
      }
    }

    in_arb_vals(i) := io.req(i).valid && in_arb_reqs(i).map(_.orR).orR
  }

  // Input arbitration
  io.req.foreach(_.ready := false.B)
  val in_alloc = Wire(MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool()) }))
  val in_flow = Mux1H(in_arb_sel, io.req.map(_.bits.flow))
  val in_vc = Mux1H(in_arb_sel, io.req.map(_.bits.in_vc))
  val in_vc_sel = Mux1H(in_arb_sel, in_arb_reqs)
  in_alloc := Mux(in_arb_vals.orR,
    inputAllocPolicy(in_flow, in_vc_sel, OHToUInt(in_arb_sel), in_vc, io.req.map(_.fire).orR),
    0.U.asTypeOf(in_alloc))

  // send allocation to inputunits
  for (i <- 0 until allInParams.size) {
    io.resp(i).valid := in_arb_sel(i)
    io.resp(i).bits.in_vc := in_vc
    io.req(i).ready := io.resp(i).valid
    for (m <- 0 until allOutParams.size) {
      (0 until allOutParams(m).nVirtualChannels).map { n =>
        io.resp(i).bits.vc_sel(m)(n) := in_alloc(m)(n)
      }
    }
    assert(PopCount(io.resp(i).bits.vc_sel.asUInt) <= 1.U)
  }

  // send allocation to output units
  for (i <- 0 until allOutParams.size) {
    (0 until allOutParams(i).nVirtualChannels).map { j =>
      io.out_allocs(i)(j).alloc := in_alloc(i)(j)
      io.out_allocs(i)(j).flow := in_flow
    }
  }
}
