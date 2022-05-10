package constellation.router

import chisel3._
import chisel3.util._
import chisel3.util.random.{LFSR}

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

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

  def inputAllocPolicy(flow: FlowRoutingBundle, vc_sel: MixedVec[Vec[Bool]], inId: UInt, inVId: UInt, fire: Bool) = {
    randOH(vc_sel.asUInt).asTypeOf(MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool())}))
  }
  def outputAllocPolicy(channel: ChannelRoutingInfo, flows: Seq[FlowRoutingBundle], reqs: Seq[Bool], fire: Bool) = {
    randOH(VecInit(reqs).asUInt).asTypeOf(Vec(allInParams.size, Bool()))
  }
}

class PIMMultiVCAllocator(vP: VCAllocatorParams)(implicit p: Parameters) extends MultiVCAllocator(vP)(p)
    with PIM
