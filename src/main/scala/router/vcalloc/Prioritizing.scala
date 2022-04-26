package constellation.router

import chisel3._
import chisel3.util._
import chisel3.util.random.{LFSR}

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.rocket.{DecodeLogic}
import freechips.rocketchip.util._

import constellation.util.{GrantHoldArbiter, ArbiterPolicy}
import constellation.channel._
import constellation.routing._

trait Prioritizing { this: VCAllocator =>
  def prioritizing(
    in: UInt,
    in_sel: MixedVec[Vec[Bool]],
    dests: Seq[ChannelRoutingInfo],
    flow: FlowRoutingBundle,
    fire: Bool): UInt = {
    val w = in.getWidth
    if (w > 1) {
      val nPrios = allInParams.map(_.channelRoutingInfos).flatten.map(c => routingRelation.getNPrios(c)).max

      case class PrioHelper(prio: Int, outId: Int, outVId: Int, inId: Int, inVId: Int, flow: FlowRoutingInfo)

      val prio_map = (0 until allOutParams.size).map { i => (0 until allOutParams(i).nVirtualChannels).map { j =>
        (0 until allInParams.size).map { m => (0 until allInParams(m).nVirtualChannels).map { n =>
          val flows = allInParams(m) match {
            case iP: ChannelParams => iP.virtualChannelParams(n).possibleFlows
            case iP: IngressChannelParams => iP.possibleFlows
          }
          flows.map { flow =>
            val prio = if (i >= outParams.size) {
              // egress. Fixed prio of 0
              0
            } else {
              routingRelation.getPrio(
                allInParams(m).channelRoutingInfos(n),
                allOutParams(i).channelRoutingInfos(j),
                flow)
            }
            require(prio < nPrios && prio >= 0, s"Invalid $prio not in [0, $nPrios) ${allInParams(m).channelRoutingInfos(n)} ${allOutParams(i).channelRoutingInfos(j)}")
            PrioHelper(prio, i, j, m, n, flow)
          }
        }}
      }}.flatten.flatten.flatten.flatten

      val in_split = in.asTypeOf(MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool())}))

      val in_prio = (0 until allOutParams.size).map { i => (0 until allOutParams(i).nVirtualChannels).map { j =>
        val addr = Cat(in_sel.asUInt, flow.asUInt)
        val lookup = prio_map.filter(t => t.outId == i && t.outVId == j).map { e =>
          val sel = BigInt(1) << (allInParams.take(e.inId).map(_.nVirtualChannels).sum + e.inVId + ingressIdBits + egressIdBits)
          val ingress = e.flow.ingressId << egressIdBits
          val egress = e.flow.egressId
          (BitPat((sel | ingress | egress).U), BitPat((1 << e.prio).U))
        }
        Mux(in_split(i)(j), DecodeLogic(addr, BitPat.dontCare(nPrios), lookup), 0.U(nPrios.W))
      }}

      val mask = RegInit(0.U(w.W))
      val prio_sels = (0 until nPrios).map { p =>
        val sel = Wire(MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool())}))
        (0 until allOutParams.size).map { i => (0 until allOutParams(i).nVirtualChannels).map { j =>
           sel(i)(j) := in_prio(i)(j)(p)
        }}
        val full = Cat(sel.asUInt, sel.asUInt & ~mask)
        val oh = PriorityEncoderOH(full)
        (oh(w-1,0) | (oh >> w))
      }
      val prio_oh = (0 until nPrios).map { p =>
        in_prio.map(_.map(_(p)).orR).orR
      }
      val lowest_prio = PriorityEncoderOH(prio_oh)
      val sel = Mux1H(lowest_prio, prio_sels)


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

  def inputAllocPolicy(req: VCAllocReq, sel: MixedVec[Vec[Bool]], fire: Bool) = {
    prioritizing(
      req.vc_sel.asUInt,
      sel,
      allOutParams.map(_.channelRoutingInfos).flatten,
      req.flow,
      fire).asTypeOf(MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool())}))
  }
  def outputAllocPolicy(out: ChannelRoutingInfo,
    flows: Seq[Seq[FlowRoutingBundle]], reqs: Seq[Seq[Bool]], fire: Bool): MixedVec[Vec[Bool]] = {
    val in = Wire(MixedVec(allInParams.map { u => Vec(u.nVirtualChannels, Bool()) }))
    require(false, "Not supported")
    in
  }
}

class PrioritizingSingleVCAllocator(vP: VCAllocatorParams)(implicit p: Parameters) extends SingleVCAllocator(vP)(p)
    with Prioritizing
