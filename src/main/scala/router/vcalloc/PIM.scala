package constellation.router

import chisel3._
import chisel3.util._
import chisel3.util.random.{LFSR}

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

import constellation.util.{GrantHoldArbiter, ArbiterPolicy}
import constellation.channel._
import constellation.routing.{ChannelRoutingInfo, FlowRoutingBundle}

trait PIM { this: VCAllocator =>
  def randOH(in: UInt): UInt = {
    val w = in.getWidth
    if (w > 1) {
      val mask = RegInit(0.U(w.W))
      val full = Cat(in, in & ~mask)
      val oh = PriorityEncoderOH(full)
      val rand = LFSR(8)(log2Ceil(w)-1,0)
      mask := MuxCase(0.U, (1 until w).map { i =>
        (rand === i.U) -> ~(0.U(i.W))
      })
      (oh(w-1,0) | (oh >> w))
    } else {
      in
    }
  }

  def inputAllocPolicy(req: VCAllocReq, fire: Bool) = {
    randOH(req.vc_sel.asUInt).asTypeOf(MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool())}))
  }
  def outputAllocPolicy(channel: ChannelRoutingInfo, flows: Seq[Seq[FlowRoutingBundle]], reqs: Seq[Seq[Bool]], fire: Bool) = {
    val in = Wire(MixedVec(allInParams.map { u => Vec(u.nVirtualChannels, Bool()) }))
    for (i <- 0 until allInParams.size) {
      (0 until allInParams(i).nVirtualChannels).map { j =>
        in(i)(j) := reqs(i)(j)
      }
    }
    randOH(in.asUInt).asTypeOf(new MixedVec(allInParams.map { u => Vec(u.nVirtualChannels, Bool()) }))
  }
}

class PIMMultiVCAllocator(vP: VCAllocatorParams)(implicit p: Parameters) extends MultiVCAllocator(vP)(p)
    with PIM
