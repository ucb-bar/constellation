package constellation.router

import chisel3._
import chisel3.util._
import chisel3.util.random.{LFSR}

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

import constellation.channel._
import constellation.routing.{ChannelRoutingInfo, FlowRoutingBundle}

// Performs batch allocation
abstract class MultiVCAllocator(vP: VCAllocatorParams)(implicit p: Parameters) extends VCAllocator(vP)(p) {
  // Input arbitration
  io.req.foreach(_.ready := false.B)
  val in_allocs = allInParams.zipWithIndex.map { case (iP,i) =>
    val a = Wire(MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool()) }))
    a := Mux(io.req(i).valid,
      inputAllocPolicy(io.req(i).bits.flow, io.req(i).bits.vc_sel, i.U, io.req(i).bits.in_vc, io.req(i).fire()),
      0.U.asTypeOf(a))
    a
  }

  // output arbitration
  val out_allocs = allOutParams.zipWithIndex.map { case (oP,i) =>
    (0 until oP.nVirtualChannels).map { j =>
      Mux(io.channel_status(i)(j).available,
        outputAllocPolicy(
          oP.channelRoutingInfos(j),
          io.req.map(_.bits.flow).toSeq,
          in_allocs.map(_(i)(j)),
          io.out_allocs(i)(j).alloc
        ),
        0.U.asTypeOf(Vec(allInParams.size, Bool()))
      )
    }
  }

  // send allocation to inputunits
  for (i <- 0 until allInParams.size) {
    io.req(i).ready := out_allocs.map(_.map(_(i)).orR).orR
    for (m <- 0 until allOutParams.size) {
      (0 until allOutParams(m).nVirtualChannels).map { n =>
        io.resp(i).vc_sel(m)(n) := out_allocs(m)(n)(i)
      }
    }
    assert(PopCount(io.resp(i).vc_sel.asUInt) <= 1.U)
  }

  // send allocation to output units
  for (i <- 0 until allOutParams.size) {
    (0 until allOutParams(i).nVirtualChannels).map { j =>
      io.out_allocs(i)(j).alloc := in_allocs.map(_(i)(j)).orR && io.channel_status(i)(j).available
      io.out_allocs(i)(j).flow := Mux1H(out_allocs(i)(j).asUInt, io.req.map(_.bits.flow).toSeq)
    }
  }
}
