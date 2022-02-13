package constellation.router

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

import constellation.channel._
import constellation.routing.{PacketRoutingBundle}
import constellation.{NoCKey}
import constellation.util.{GrantHoldArbiter, WrapInc, ArbiterPolicy}

class AbstractInputUnitIO(
  val cParam: BaseChannelParams,
  val outParams: Seq[ChannelParams],
  val egressParams: Seq[EgressChannelParams],
)(implicit val p: Parameters) extends Bundle
    with HasRouterOutputParams with HasChannelParams {
  val nodeId = cParam.destId

  val router_req = Decoupled(new RouteComputerReq(cParam))
  val router_resp = Flipped(Valid(new RouteComputerResp(cParam, outParams, egressParams)))

  val vcalloc_req = Vec(nVirtualChannels, Decoupled(MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool()) })))
  val vcalloc_resp = Flipped(Valid(new VCAllocResp(cParam, outParams, egressParams)))

  val out_credit_available = Input(MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool()) }))

  val salloc_req = Vec(cParam.destMultiplier, Decoupled(new SwitchAllocReq(outParams, egressParams)))

  val out = Vec(cParam.destMultiplier, Valid(new SwitchBundle(outParams, egressParams)))
  val debug = Output(new Bundle {
    val va_stall = UInt(log2Ceil(nVirtualChannels).W)
    val sa_stall = UInt(log2Ceil(nVirtualChannels).W)
  })
}

abstract class AbstractInputUnit(
  val cParam: BaseChannelParams,
  val outParams: Seq[ChannelParams],
  val egressParams: Seq[EgressChannelParams]
)(implicit val p: Parameters) extends Module with HasRouterOutputParams with HasChannelParams {
  val nodeId = cParam.destId

  def io: AbstractInputUnitIO

  def filterVCSel(in: MixedVec[Vec[Bool]], srcV: Int): MixedVec[Vec[Bool]] = {
    val out = WireInit(in)
    if (virtualChannelParams(srcV).traversable) {
      outParams.zipWithIndex.map { case (oP, oI) =>
        (0 until oP.nVirtualChannels).map { oV =>
          val allow = virtualChannelParams(srcV).possiblePackets.map { pI =>
            p(NoCKey).routingRelation(
              nodeId,
              cParam.channelRoutingInfos(srcV),
              oP.channelRoutingInfos(oV),
              pI
            )
          }.reduce(_||_)
          if (!allow)
            out(oI)(oV) := false.B
        }
      }
    }
    out
  }

  def atDest(egress: UInt) = egressSrcIds.zipWithIndex.filter(_._1 == nodeId).map(_._2.U === egress).orR
}

class InputUnit(cParam: ChannelParams, outParams: Seq[ChannelParams],
  egressParams: Seq[EgressChannelParams],
  combineRCVA: Boolean, combineSAST: Boolean, earlyRC: Boolean
)
  (implicit p: Parameters) extends AbstractInputUnit(cParam, outParams, egressParams)(p) {

  val io = IO(new AbstractInputUnitIO(cParam, outParams, egressParams) {
    val in = Flipped(new Channel(cParam.asInstanceOf[ChannelParams]))
  })

  val g_i :: g_r :: g_r_stall :: g_v :: g_v_stall :: g_a :: g_c :: Nil = Enum(7)

  class InputState extends Bundle {
    val g = UInt(3.W)
    val vc_sel = MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool()) })
    val r = new PacketRoutingBundle
    val tail_seen = Bool()
  }
  val qs = virtualChannelParams.map { vP => Module(new Queue(new Flit(cParam), vP.bufferSize)) }
  qs.zipWithIndex.foreach { case (q,i) =>
    val sel = io.in.flit.map(f => f.valid && f.bits.virt_channel_id === i.U)
    q.io.enq.valid := sel.reduce(_||_)
    q.io.enq.bits := Mux1H(sel, io.in.flit.map(_.bits))
    assert(!(q.io.enq.valid && !q.io.enq.ready))
    q.io.deq.ready := false.B
  }

  val route_arbiter = Module(new GrantHoldArbiter(
    new RouteComputerReq(cParam), nVirtualChannels,
    (t: RouteComputerReq) => true.B,
    policy = ArbiterPolicy.RoundRobin))
  val early_route_arbiter = Module(new Arbiter(
    new RouteComputerReq(cParam), 1 + cParam.srcMultiplier))
  early_route_arbiter.io.in.foreach(_.valid := false.B)
  early_route_arbiter.io.in.foreach(_.bits := DontCare)
  early_route_arbiter.io.in(0) <> route_arbiter.io.out(0)
  io.router_req <> early_route_arbiter.io.out

  val states = Reg(Vec(nVirtualChannels, new InputState))

  for (i <- 0 until cParam.srcMultiplier) {
    when (io.in.flit(i).fire() && io.in.flit(i).bits.head) {
      val id = io.in.flit(i).bits.virt_channel_id
      assert(id < nVirtualChannels.U)
      assert(states(id).g === g_i)
      val at_dest = atDest(io.in.flit(i).bits.egress_id)
      states(id).g := Mux(at_dest, g_v, g_r)
      states(id).vc_sel.foreach(_.foreach(_ := false.B))
      for (o <- 0 until nEgress) {
        when (egressParams(o).egressId.U === io.in.flit(i).bits.egress_id) {
          states(id).vc_sel(o+nOutputs)(0) := true.B
        }
      }
      states(id).r := io.in.flit(i).bits.route_info
      states(id).tail_seen := io.in.flit(i).bits.tail
      if (earlyRC) {
        val rreq = early_route_arbiter.io.in(i+1)
        when (!at_dest) {
          rreq.valid := true.B
          rreq.bits.route_info := io.in.flit(i).bits.route_info
          rreq.bits.src_virt_id := io.in.flit(i).bits.virt_channel_id
          states(id).g := Mux(rreq.ready, g_r_stall, g_r)
        }
      }
    } .elsewhen (io.in.flit(i).fire()) {
      val id = io.in.flit(i).bits.virt_channel_id
      when (io.in.flit(i).bits.tail) {
        states(id).tail_seen := true.B
      }
    }
  }

  (route_arbiter.io.in zip states).zipWithIndex.map { case ((i,s),idx) =>
    if (virtualChannelParams(idx).traversable) {
      i.valid := s.g === g_r
      i.bits.route_info := s.r
      i.bits.src_virt_id := idx.U
      when (i.fire()) { s.g := g_r_stall }
    } else {
      i.valid := false.B
      i.bits := DontCare
    }
  }

  when (io.router_resp.fire()) {
    val id = io.router_resp.bits.src_virt_id
    assert(states(id).g.isOneOf(g_r, g_r_stall) || (
      earlyRC.B && io.in.flit.map(f => f.valid && f.bits.head && f.bits.virt_channel_id === id).reduce(_||_)
    ))
    states(id).g := g_v
    for (i <- 0 until nVirtualChannels) {
      when (i.U === id) {
        states(i).vc_sel := filterVCSel(io.router_resp.bits.vc_sel, i)
      }
    }
  }

  (io.vcalloc_req zip states).zipWithIndex.map { case ((i,s),idx) =>
    if (virtualChannelParams(idx).traversable) {
      i.valid := s.g === g_v
      i.bits := s.vc_sel
      when (i.fire()) { s.g := g_v_stall }
      if (combineRCVA) {
        when (io.router_resp.valid && io.router_resp.bits.src_virt_id === idx.U) {
          i.valid := true.B
          i.bits := io.router_resp.bits.vc_sel
        }
      }
    } else {
      i.valid := false.B
      i.bits := DontCare
    }
  }

  io.debug.va_stall := PopCount(io.vcalloc_req.map { i => i.valid && !i.ready })

  when (io.vcalloc_resp.fire()) {
    val id = io.vcalloc_resp.bits.in_virt_channel
    for (i <- 0 until nVirtualChannels) {
      when (i.U === id) {
        states(i).vc_sel := filterVCSel(io.vcalloc_resp.bits.vc_sel, i)
      }
    }
    states(id).g := g_a
  }
  val salloc_arb = Module(new GrantHoldArbiter(
    new SwitchAllocReq(outParams, egressParams),
    nVirtualChannels,
    (d: SwitchAllocReq) => d.tail,
    policy = ArbiterPolicy.RoundRobin,
    nOut = cParam.destMultiplier
  ))

  (states zip salloc_arb.io.in).zipWithIndex.map { case ((s,r),i) =>
    if (virtualChannelParams(i).traversable) {
      val credit_available = (s.vc_sel.asUInt & io.out_credit_available.asUInt) =/= 0.U
      r.valid := s.g === g_a && credit_available && qs(i).io.deq.valid
      r.bits.vc_sel := s.vc_sel
      r.bits.tail := qs(i).io.deq.bits.tail
      when (r.fire() && qs(i).io.deq.bits.tail) {
        s.g := g_i
      }
      qs(i).io.deq.ready := r.ready
    } else {
      r.valid := false.B
      r.bits := DontCare
    }
  }
  io.debug.sa_stall := PopCount(salloc_arb.io.in.map(r => r.valid && !r.ready))
  io.salloc_req <> salloc_arb.io.out

  class OutBundle extends Bundle {
    val valid = Bool()
    val vid = UInt(virtualChannelBits.W)
    val out_vid = UInt(log2Up(allOutParams.map(_.nVirtualChannels).max).W)
    val flit = new Flit(cParam)
  }

  val salloc_outs = if (combineSAST) {
    Wire(Vec(cParam.destMultiplier, new OutBundle))
  } else {
    Reg(Vec(cParam.destMultiplier, new OutBundle))
  }


  io.in.credit_return := salloc_arb.io.out.zipWithIndex.map { case (o, i) =>
    Mux(o.fire(), salloc_arb.io.chosen_oh(i), 0.U)
  }.reduce(_|_)
  io.in.vc_free := salloc_arb.io.out.zipWithIndex.map { case (o, i) =>
    Mux(o.fire() && Mux1H(salloc_arb.io.chosen_oh(i), qs.map(_.io.deq.bits.tail)), salloc_arb.io.chosen_oh(i), 0.U)
  }.reduce(_|_)

  for (i <- 0 until cParam.destMultiplier) {
    val salloc_out = salloc_outs(i)
    salloc_out.valid := salloc_arb.io.out(i).fire()
    salloc_out.vid := salloc_arb.io.chosen(i)
    val vc_sel = Mux1H(salloc_arb.io.chosen_oh(i), states.map(_.vc_sel))
    val channel_oh = vc_sel.map(_.reduce(_||_))
    val virt_channel = Mux1H(channel_oh, vc_sel.map(v => OHToUInt(v)))
    salloc_out.out_vid := virt_channel
    salloc_out.flit := Mux1H(salloc_arb.io.chosen_oh(i), qs.map(_.io.deq.bits))

    io.out(i).valid := salloc_out.valid
    io.out(i).bits.flit := salloc_out.flit
    io.out(i).bits.out_virt_channel := salloc_out.out_vid
  }

  (0 until nVirtualChannels).map { i =>
    if (!virtualChannelParams(i).traversable) states(i) := DontCare
  }

  when (reset.asBool) {
    states.foreach(_.g := g_i)
  }
}
