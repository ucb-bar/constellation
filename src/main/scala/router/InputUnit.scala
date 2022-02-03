package constellation.router

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

import constellation.channel._
import constellation.routing.{PacketRoutingBundle}
import constellation.{NoCKey}
import constellation.util.{GrantHoldArbiter, WrapInc}

class AbstractInputUnitIO(
  val cParam: BaseChannelParams,
  val outParams: Seq[ChannelParams],
  val egressParams: Seq[EgressChannelParams],
)(implicit val p: Parameters) extends Bundle
    with HasRouterOutputParams with HasChannelParams {
  val nodeId = cParam.destId

  val router_req = Decoupled(new RouteComputerReq(cParam))
  val router_resp = Flipped(Valid(new RouteComputerResp(cParam, outParams, egressParams)))

  val vcalloc_req = Decoupled(new VCAllocReq(cParam, outParams, egressParams))
  val vcalloc_resp = Flipped(Valid(new VCAllocResp(cParam, outParams, egressParams)))

  val out_credit_available = Input(MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool()) }))

  val salloc_req = Vec(nVirtualChannels, Decoupled(new SwitchAllocReq(outParams, egressParams)))

  val out = Valid(new SwitchBundle(outParams, egressParams))
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
              virtualChannelParams(srcV).asChannelRoutingInfo,
              oP.virtualChannelParams(oV).asChannelRoutingInfo,
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
    val ptr = UInt(log2Up(maxBufferSize).W)
    val c = UInt(log2Up(1+maxBufferSize).W)
    val r = new PacketRoutingBundle
    val tail_seen = Bool()
  }

  val route_arbiter = Module(new GrantHoldArbiter(
    new RouteComputerReq(cParam), nVirtualChannels,
    (t: RouteComputerReq) => true.B, rr = true))
  val early_route_arbiter = Module(new Arbiter(
    new RouteComputerReq(cParam), 2))
  early_route_arbiter.io.in(0) <> route_arbiter.io.out
  early_route_arbiter.io.in(1).valid := false.B
  early_route_arbiter.io.in(1).bits := DontCare
  io.router_req <> early_route_arbiter.io.out

  val states = Reg(Vec(nVirtualChannels, new InputState))
  val buffer = Module(new InputBuffer(cParam))
  buffer.io.in := io.in.flit
  (0 until nVirtualChannels).map { i => if (virtualChannelParams(i).traversable) {
    when (!io.salloc_req(i).fire() &&
      (io.in.flit.fire() && io.in.flit.bits.virt_channel_id === i.U)) {

      states(i).c := states(i).c + 1.U
    } .elsewhen (io.salloc_req(i).fire() &&
      !(io.in.flit.fire() && io.in.flit.bits.virt_channel_id === i.U)) {

      states(i).c := states(i).c - 1.U
    }
  } }

  when (io.in.flit.fire() && io.in.flit.bits.head) {
    val id = io.in.flit.bits.virt_channel_id
    assert(id < nVirtualChannels.U)
    assert(states(id).g === g_i)
    val at_dest = atDest(io.in.flit.bits.egress_id)
    states(id).g := Mux(at_dest, g_v, g_r)
    states(id).vc_sel.foreach(_.foreach(_ := false.B))
    for (o <- 0 until nEgress) {
      when (egressParams(o).egressId.U === io.in.flit.bits.egress_id) {
        states(id).vc_sel(o+nOutputs)(0) := true.B
      }
    }
    states(id).ptr := buffer.io.head
    states(id).r := io.in.flit.bits.route_info
    states(id).tail_seen := io.in.flit.bits.tail
    if (earlyRC) {
      val rreq = early_route_arbiter.io.in(1)
      when (!at_dest) {
        rreq.valid := true.B
        rreq.bits.route_info := io.in.flit.bits.route_info
        rreq.bits.src_virt_id := io.in.flit.bits.virt_channel_id
        states(id).g := Mux(rreq.ready, g_r_stall, g_r)
      }
    }
  } .elsewhen (io.in.flit.fire()) {
    val id = io.in.flit.bits.virt_channel_id
    when (io.in.flit.bits.tail) {
      states(id).tail_seen := true.B
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
      earlyRC.B && io.in.flit.valid && io.in.flit.bits.head && io.in.flit.bits.virt_channel_id === id)
    )
    states(id).g := g_v
    for (i <- 0 until nVirtualChannels) {
      when (i.U === id) {
        states(i).vc_sel := filterVCSel(io.router_resp.bits.vc_sel, i)
      }
    }
  }

  val vcalloc_arbiter = Module(new GrantHoldArbiter(
    new VCAllocReq(cParam, outParams, egressParams), nVirtualChannels,
    (x: VCAllocReq) => true.B, rr = true))
  val early_vcalloc_arbiter = Module(new Arbiter(
    new VCAllocReq(cParam, outParams, egressParams), 2))
  early_vcalloc_arbiter.io.in(0) <> vcalloc_arbiter.io.out
  early_vcalloc_arbiter.io.in(1).valid := false.B
  early_vcalloc_arbiter.io.in(1).bits := DontCare
  io.vcalloc_req <> early_vcalloc_arbiter.io.out
  (vcalloc_arbiter.io.in zip states).zipWithIndex.map { case ((i,s),idx) =>
    if (virtualChannelParams(idx).traversable) {
      i.valid := s.g === g_v
      i.bits.in_virt_channel := idx.U
      i.bits.vc_sel := s.vc_sel
      when (i.fire()) { s.g := g_v_stall }
    } else {
      i.valid := false.B
      i.bits := DontCare
    }
  }

  if (combineRCVA) {
    when (io.router_resp.fire()) {
      val vcreq = early_vcalloc_arbiter.io.in(1)
      vcreq.valid := true.B
      vcreq.bits.in_virt_channel := io.router_resp.bits.src_virt_id
      vcreq.bits.vc_sel := io.router_resp.bits.vc_sel
      when (vcreq.fire()) {
        states(io.router_resp.bits.src_virt_id).g := g_v_stall
      }
    }
  }

  io.debug.va_stall := PopCount(vcalloc_arbiter.io.in.map { i => i.valid && !i.ready })

  when (io.vcalloc_resp.fire()) {
    val id = io.vcalloc_resp.bits.in_virt_channel
    for (i <- 0 until nVirtualChannels) {
      when (i.U === id) {
        states(i).vc_sel := filterVCSel(io.vcalloc_resp.bits.vc_sel, i)
      }
    }
    states(id).g := g_a
  }

  (states zip io.salloc_req).zipWithIndex.map { case ((s,r),i) =>
    if (virtualChannelParams(i).traversable) {
      val credit_available = (s.vc_sel.asUInt & io.out_credit_available.asUInt) =/= 0.U
      val bypass_input = if (combineSAST) {
        false.B
      } else {
        io.in.flit.valid && io.in.flit.bits.virt_channel_id === i.U
      }
      r.valid := s.g === g_a && credit_available && (s.c =/= 0.U || bypass_input)
      r.bits.vc_sel := s.vc_sel
      buffer.io.tail_read_req(i) := s.ptr
      r.bits.tail := buffer.io.tail_read_resp(i)
      when (r.fire()) {
        s.ptr := WrapInc(s.ptr, virtualChannelParams(i).bufferSize)
      }
      when (r.fire() && buffer.io.tail_read_resp(i)) {
        s.g := g_i
      }
    } else {
      r.valid := false.B
      r.bits := DontCare
      buffer.io.tail_read_req(i) := 0.U
    }
  }
  io.debug.sa_stall := PopCount(io.salloc_req.map(r => r.valid && !r.ready))

  val salloc_fires = io.salloc_req.map(_.fire())
  val salloc_fire_id = OHToUInt(salloc_fires)
  val salloc_fire = salloc_fires.reduce(_||_)
  assert(PopCount(salloc_fires) <= 1.U)

  io.in.credit_return.valid := salloc_fire
  io.in.credit_return.bits := salloc_fire_id
  io.in.vc_free.valid := salloc_fire && buffer.io.tail_read_resp(salloc_fire_id)
  io.in.vc_free.bits := salloc_fire_id


  class OutBundle extends Bundle {
    val valid = Bool()
    val ptr = UInt(log2Up(maxBufferSize).W)
    val vid = UInt(virtualChannelBits.W)
    val out_vid = UInt(log2Up(allOutParams.map(_.nVirtualChannels).max).W)
    val out_id = Vec(nAllOutputs, Bool())
  }

  val salloc_out = if (combineSAST) {
    Wire(new OutBundle)
  } else {
    Reg(new OutBundle)
  }
  salloc_out.valid := salloc_fire
  salloc_out.ptr := Mux1H(salloc_fires, states.map(_.ptr))
  salloc_out.vid := salloc_fire_id
  val vc_sel = Mux1H(salloc_fires, states.map(_.vc_sel))
  val channel_oh = vc_sel.map(_.reduce(_||_))
  val virt_channel = Mux1H(channel_oh, vc_sel.map(v => OHToUInt(v)))
  salloc_out.out_vid := virt_channel
  salloc_out.out_id := VecInit(channel_oh)

  buffer.io.read_req.valid := salloc_out.valid
  buffer.io.read_req.bits.addr := salloc_out.ptr
  buffer.io.read_req.bits.channel := salloc_out.vid

  io.out.valid := buffer.io.read_req.valid
  io.out.bits.flit := buffer.io.read_resp
  io.out.bits.out_virt_channel := salloc_out.out_vid
  io.out.bits.out_channel_oh := salloc_out.out_id

  (0 until nVirtualChannels).map { i =>
    if (!virtualChannelParams(i).traversable) states(i) := DontCare
  }

  when (reset.asBool) {
    states.foreach(_.g := g_i)
    states.foreach(_.c := 0.U)
  }
}
