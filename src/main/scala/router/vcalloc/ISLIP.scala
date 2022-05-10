package constellation.router

import chisel3._
import chisel3.util._
import chisel3.util.random.{LFSR}

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

import constellation.channel._
import constellation.routing.{ChannelRoutingInfo, FlowRoutingBundle}

trait ISLIP { this: VCAllocator =>
  def islip(in: UInt, fire: Bool): UInt = {
    val w = in.getWidth
    if (w > 1) {
      val mask = RegInit(0.U(w.W))
      val full = Cat(in, in & ~mask)
      val oh = PriorityEncoderOH(full)
      val sel = (oh(w-1,0) | (oh >> w))
      when (fire) {
        mask := MuxCase(0.U, (0 until w).map { i =>
          sel(i) -> ~(0.U((i+1).W))
        })
      }
      sel
    } else {
      in
    }
  }

  def inputAllocPolicy(flow: FlowRoutingBundle, vc_sel: MixedVec[Vec[Bool]], inId: UInt, inVId: UInt, fire: Bool) = {
    islip(vc_sel.asUInt, fire).asTypeOf(MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool())}))
  }
  def outputAllocPolicy(channel: ChannelRoutingInfo, flows: Seq[FlowRoutingBundle], reqs: Seq[Bool], fire: Bool) = {
    islip(VecInit(reqs).asUInt, fire).asTypeOf(Vec(allInParams.size, Bool()))
  }
}

class ISLIPMultiVCAllocator(vP: VCAllocatorParams)(implicit p: Parameters) extends MultiVCAllocator(vP)(p)
    with ISLIP

class RotatingSingleVCAllocator(vP: VCAllocatorParams)(implicit p: Parameters) extends SingleVCAllocator(vP)(p)
    with ISLIP
