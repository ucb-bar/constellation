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
  val in_arb = Module(new RRArbiter(
    new VCAllocReq(outParams, egressParams),
    allInParams.map(_.nVirtualChannels).sum
  ))
  in_arb.io.out.ready := true.B

  var t = 0
  val in_sel = Wire(MixedVec(allInParams.map { u => Vec(u.nVirtualChannels, Bool()) }))
  for (i <- 0 until allInParams.size) {
    (0 until allInParams(i).nVirtualChannels).map { j =>
      in_arb.io.in(t).valid := io.req(i)(j).valid && in_arb.io.in(t).bits.vc_sel.asUInt =/= 0.U
      in_arb.io.in(t).bits := io.req(i)(j).bits
      for (m <- 0 until allOutParams.size) {
        (0 until allOutParams(m).nVirtualChannels).map { n =>
          when (!io.channel_status(m)(n).available) {
            in_arb.io.in(t).bits.vc_sel(m)(n) := false.B
          }
        }
      }
      in_sel(i)(j) := in_arb.io.chosen === t.U
      t += 1
    }
  }

  // Input arbitration
  io.req.foreach(_.foreach(_.ready := false.B))
  val in_alloc = Wire(MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool()) }))
  in_alloc := Mux(in_arb.io.out.valid,
    inputAllocPolicy(in_arb.io.out.bits, io.req.map(_.map(_.fire).orR).orR),
    0.U.asTypeOf(in_alloc))

  // send allocation to inputunits
  for (i <- 0 until allInParams.size) {
    (0 until allInParams(i).nVirtualChannels).map { j =>
      io.resp(i)(j).valid := in_sel(i)(j) && in_arb.io.out.valid
      io.req(i)(j).ready := io.resp(i)(j).valid
      for (m <- 0 until allOutParams.size) {
        (0 until allOutParams(m).nVirtualChannels).map { n =>
          io.resp(i)(j).bits.vc_sel(m)(n) := in_alloc(m)(n)
        }
      }
      assert(PopCount(io.resp(i)(j).bits.vc_sel.asUInt) <= 1.U)
    }
  }

  // send allocation to output units
  for (i <- 0 until allOutParams.size) {
    (0 until allOutParams(i).nVirtualChannels).map { j =>
      io.out_allocs(i)(j).alloc := in_alloc(i)(j)
      io.out_allocs(i)(j).flow := in_arb.io.out.bits.flow
    }
  }
}
