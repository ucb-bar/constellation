package constellation.router

import chisel3._
import chisel3.util._
import chisel3.util.random.{LFSR}

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

import constellation.util.{GrantHoldArbiter, ArbiterPolicy}
import constellation.channel._
import constellation.routing.{ChannelRoutingInfo, FlowRoutingBundle}

// Performs batch allocation
abstract class MultiVCAllocator(vP: VCAllocatorParams)(implicit p: Parameters) extends VCAllocator(vP)(p) {
  // Input arbitration
  io.req.foreach(_.foreach(_.ready := false.B))
  val in_allocs = allInParams.zipWithIndex.map { case (iP,i) =>
    (0 until iP.nVirtualChannels).map { j =>
      val a = Wire(MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool()) }))
      a := Mux(io.req(i)(j).valid, inputAllocPolicy(io.req(i)(j).bits, io.req(i)(j).fire()),
        0.U.asTypeOf(a))
      a
    }
  }

  // output arbitration
  val out_allocs = allOutParams.zipWithIndex.map { case (oP,i) =>
    (0 until oP.nVirtualChannels).map { j =>
      val a = Wire(MixedVec(allInParams.map { u => Vec(u.nVirtualChannels, Bool()) }))
      a := Mux(io.channel_status(i)(j).available,
        outputAllocPolicy(oP.channelRoutingInfos(j),
          io.req.map(_.map(_.bits.flow)),
          in_allocs.map(_.map(_(i)(j))),
          io.out_allocs(i)(j).alloc
        ),
        0.U.asTypeOf(a))
      a
    }
  }

  // send allocation to inputunits
  for (i <- 0 until allInParams.size) {
    (0 until allInParams(i).nVirtualChannels).map { j =>
      io.resp(i)(j).valid := out_allocs.map(_.map(_(i)(j)).orR).orR
      io.req(i)(j).ready := io.resp(i)(j).valid
      for (m <- 0 until allOutParams.size) {
        (0 until allOutParams(m).nVirtualChannels).map { n =>
          io.resp(i)(j).bits.vc_sel(m)(n) := out_allocs(m)(n)(i)(j)
        }
      }
      assert(PopCount(io.resp(i)(j).bits.vc_sel.asUInt) <= 1.U)
    }
  }

  // send allocation to output units
  for (i <- 0 until allOutParams.size) {
    (0 until allOutParams(i).nVirtualChannels).map { j =>
      io.out_allocs(i)(j).alloc := in_allocs.map(_.map(_(i)(j)).orR).orR && io.channel_status(i)(j).available
      io.out_allocs(i)(j).flow := Mux1H(out_allocs(i)(j).asUInt, io.req.map(_.map(_.bits.flow)).flatten)
    }
  }
}
