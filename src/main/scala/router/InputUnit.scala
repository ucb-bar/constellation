package constellation.router

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

import constellation._

class AbstractInputUnitIO(
  val cParam: ChannelParams,
  val outParams: Seq[ChannelParams],
  val terminalOutParams: Seq[ChannelParams]
)(implicit val p: Parameters) extends Bundle with HasRouterOutputParams with HasChannelParams {
  val nodeId = cParam.destId

  val router_req = Decoupled(new RouteComputerReq(cParam))
  val router_resp = Flipped(Valid(new RouteComputerResp(cParam, outParams, terminalOutParams)))

  val vcalloc_req = Decoupled(new VCAllocReq(cParam, outParams, terminalOutParams))
  val vcalloc_resp = Flipped(Valid(new VCAllocResp(cParam, outParams, terminalOutParams)))

  val out_credit_available = Input(MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool()) }))

  val salloc_req = Vec(nVirtualChannels, Decoupled(new SwitchAllocReq(outParams, terminalOutParams)))

  val out = Valid(new SwitchBundle(outParams, terminalOutParams))
}

abstract class AbstractInputUnit(
  val cParam: ChannelParams,
  val outParams: Seq[ChannelParams],
  val terminalOutParams: Seq[ChannelParams]
)(implicit val p: Parameters) extends Module with HasRouterOutputParams with HasChannelParams {
  val nodeId = cParam.destId

  def io: AbstractInputUnitIO
}

class InputUnit(cParam: ChannelParams, outParams: Seq[ChannelParams], terminalOutParams: Seq[ChannelParams])
  (implicit p: Parameters) extends AbstractInputUnit(cParam, outParams, terminalOutParams)(p) {
  require(!isTerminalInputChannel)

  val io = IO(new AbstractInputUnitIO(cParam, outParams, terminalOutParams) {
    val in = Flipped(new Channel(cParam))
  })
  val g_i :: g_r :: g_r_stall :: g_v :: g_v_stall :: g_a :: g_c :: Nil = Enum(7)

  class InputState extends Bundle {
    val g = UInt(3.W)
    val ro = MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool()) })
    val p = UInt(log2Up(maxBufferSize).W)
    val c = UInt(log2Up(1+maxBufferSize).W)
    val vnet_id = UInt(vNetBits.W)
    val tail_seen = Bool()
    val dest_id = UInt(nodeIdBits.W)
  }
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

    val dest_id = outIdToDestId(io.in.flit.bits.out_id)
    states(id).g := Mux(dest_id === nodeId.U, g_v, g_r)
    states(id).dest_id := dest_id
    states(id).ro.foreach(_.foreach(_ := false.B))
    val term_id = outIdToDestChannelId(io.in.flit.bits.out_id)
    for (o <- 0 until nTerminalOutputs) {
      when (term_id === o.U) {
        states(id).ro(o+nOutputs)(0) := true.B
      }
    }
    states(id).p := buffer.io.head
    states(id).tail_seen := io.in.flit.bits.tail
    states(id).vnet_id := io.in.flit.bits.vnet_id

  } .elsewhen (io.in.flit.fire()) {
    val id = io.in.flit.bits.virt_channel_id
    when (io.in.flit.bits.tail) {
      states(id).tail_seen := true.B
    }
  }

  val route_arbiter = Module(new GrantHoldArbiter(
    new RouteComputerReq(cParam), nVirtualChannels,
    (t: RouteComputerReq) => true.B, rr = true))
  (route_arbiter.io.in zip states).zipWithIndex.map { case ((i,s),idx) =>
    if (virtualChannelParams(idx).traversable) {
      i.valid := s.g === g_r
      i.bits.dest_id := s.dest_id
      i.bits.src_virt_id := idx.U
      i.bits.src_vnet_id := s.vnet_id
      when (i.fire()) { s.g := g_r_stall }
    } else {
      i.valid := false.B
      i.bits := DontCare
    }
  }
  io.router_req <> route_arbiter.io.out

  when (io.router_resp.fire()) {
    val id = io.router_resp.bits.src_virt_id
    assert(states(id).g.isOneOf(g_r, g_r_stall))
    states(id).g := g_v
    states(id).ro := io.router_resp.bits.vc_sel
  }

  val vcalloc_arbiter = Module(new GrantHoldArbiter(
    new VCAllocReq(cParam, outParams, terminalOutParams), nVirtualChannels,
    (x: VCAllocReq) => true.B, rr = true))
  (vcalloc_arbiter.io.in zip states).zipWithIndex.map { case ((i,s),idx) =>
    if (virtualChannelParams(idx).traversable) {
      i.valid := s.g === g_v

      i.bits.in_virt_channel := idx.U
      i.bits.vc_sel := s.ro
      when (i.fire()) { s.g := g_v_stall }
    } else {
      i.valid := false.B
      i.bits := DontCare
    }
  }
  io.vcalloc_req <> vcalloc_arbiter.io.out

  when (io.vcalloc_resp.fire()) {
    val id = io.vcalloc_resp.bits.in_virt_channel
    assert(states(id).g.isOneOf(g_v, g_v_stall))


    states(id).ro := io.vcalloc_resp.bits.vc_sel
    states(id).g := g_a
  }

  (states zip io.salloc_req).zipWithIndex.map { case ((s,r),i) =>
    if (virtualChannelParams(i).traversable) {
      val credit_available = (s.ro.asUInt & io.out_credit_available.asUInt) =/= 0.U

      r.valid := s.g === g_a && credit_available && (s.c =/= 0.U ||
        (io.in.flit.valid && io.in.flit.bits.virt_channel_id === i.U))
      r.bits.vc_sel := s.ro
      buffer.io.tail_read_req(i) := s.p
      r.bits.tail := buffer.io.tail_read_resp(i)
      when (r.fire()) {
        s.p := WrapInc(s.p, virtualChannelParams(i).bufferSize)
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
  val salloc_fires = io.salloc_req.map(_.fire())
  val salloc_fire_id = OHToUInt(salloc_fires)
  val salloc_fire = salloc_fires.reduce(_||_)
  assert(PopCount(salloc_fires) <= 1.U)
  buffer.io.read_req.valid := salloc_fire
  buffer.io.read_req.bits.addr := Mux1H(salloc_fires, states.map(_.p))
  buffer.io.read_req.bits.channel := salloc_fire_id

  io.in.credit_return.valid := salloc_fire
  io.in.credit_return.bits := salloc_fire_id
  io.in.vc_free.valid := salloc_fire && buffer.io.tail_read_resp(salloc_fire_id)
  io.in.vc_free.bits := salloc_fire_id

  io.out.valid := RegNext(buffer.io.read_req.valid)
  io.out.bits.flit := buffer.io.read_resp
  val ro = Mux1H(salloc_fires, states.map(_.ro))
  val channel_oh = ro.map(_.reduce(_||_))
  val virt_channel = Mux1H(channel_oh, ro.map(v => OHToUInt(v)))
  io.out.bits.out_virt_channel := RegNext(virt_channel)
  io.out.bits.out_channel := RegNext(OHToUInt(channel_oh))

  when (reset.asBool) {
    states.foreach(_.g := g_i)
    states.foreach(_.c := 0.U)
  }
}
