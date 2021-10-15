package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

class AbstractInputUnitIO(val inParam: ChannelParams, val outParams: Seq[ChannelParams], val terminalOutParams: Seq[ChannelParams])(implicit val p: Parameters) extends Bundle {
  val router_req = Decoupled(new RouteComputerReq(inParam))
  val router_resp = Flipped(Valid(new RouteComputerResp(inParam, outParams.size)))

  val vcalloc_req = Decoupled(new VCAllocReq(inParam, outParams.size, terminalOutParams.size))
  val vcalloc_resp = Flipped(Valid(new VCAllocResp(inParam, outParams ++ terminalOutParams)))

  val out_credit_available = Input(MixedVec((outParams ++ terminalOutParams).map { u => Vec(u.virtualChannelParams.size, Bool()) }))

  val salloc_req = Vec(inParam.nVirtualChannels, Decoupled(new SwitchAllocReq(outParams ++ terminalOutParams)))

  val out = Valid(new SwitchBundle(outParams.size + terminalOutParams.size))
}

abstract class AbstractInputUnit(implicit val p: Parameters) extends Module with HasAstroNoCParams {
  def io: AbstractInputUnitIO
}

class InputUnit(inParam: ChannelParams, outParams: Seq[ChannelParams], terminalOutParams: Seq[ChannelParams])
  (implicit p: Parameters) extends AbstractInputUnit()(p) {
  val nOutputs = outParams.size
  val nTerminalOutputs = terminalOutParams.size
  val nVirtualChannels = inParam.virtualChannelParams.size
  require(nVirtualChannels <= (1 << virtChannelBits))

  val io = IO(new AbstractInputUnitIO(inParam, outParams, terminalOutParams) {
    val in = Flipped(new Channel(inParam))
  })
  val g_i :: g_r :: g_r_stall :: g_v :: g_v_stall :: g_a :: g_c :: Nil = Enum(7)

  class InputState extends Bundle {
    val g = UInt(3.W)
    val r = UInt((nTerminalOutputs + nOutputs).W)
    val o = UInt(log2Up((outParams ++ terminalOutParams).map(_.virtualChannelParams.size).max).W)
    val p = UInt(log2Up(inParam.virtualChannelParams.map(_.bufferSize).max).W)
    val c = UInt(log2Up(1+inParam.virtualChannelParams.map(_.bufferSize).max).W)
    val prio = UInt(prioBits.W)
    val tail_seen = Bool()
    val dest_id = UInt(idBits.W)
  }
  val states = Reg(Vec(nVirtualChannels, new InputState))
  val buffer = Module(new InputBuffer(inParam))
  buffer.io.in := io.in.flit
  (0 until nVirtualChannels).map { i =>
    when (!io.salloc_req(i).fire() &&
      (io.in.flit.fire() && io.in.flit.bits.virt_channel_id === i.U)) {

      states(i).c := states(i).c + 1.U
    } .elsewhen (io.salloc_req(i).fire() &&
      !(io.in.flit.fire() && io.in.flit.bits.virt_channel_id === i.U)) {

      states(i).c := states(i).c - 1.U
    }
  }

  when (io.in.flit.fire() && io.in.flit.bits.head) {
    val id = io.in.flit.bits.virt_channel_id
    assert(id < nVirtualChannels.U)
    assert(states(id).g === g_i)

    val dest_id = outIdToDestId(io.in.flit.bits.out_id)
    states(id).g := Mux(dest_id === inParam.destId.U, g_v, g_r)
    states(id).dest_id := dest_id
    states(id).r := UIntToOH(outIdToDestChannelId(io.in.flit.bits.out_id)) << outParams.size
    states(id).p := buffer.io.head
    states(id).tail_seen := io.in.flit.bits.tail
    states(id).prio := io.in.flit.bits.prio

  } .elsewhen (io.in.flit.fire()) {
    val id = io.in.flit.bits.virt_channel_id
    when (io.in.flit.bits.tail) {
      states(id).tail_seen := true.B
    }
  }

  val route_arbiter = Module(new RRArbiter(new RouteComputerReq(inParam), nVirtualChannels))
  (route_arbiter.io.in zip states).zipWithIndex.map { case ((i,s),idx) =>
    i.valid := s.g === g_r
    i.bits.dest_id := s.dest_id
    i.bits.src_virt_id := idx.U
    i.bits.src_prio := s.prio
    when (i.fire()) { s.g := g_r_stall }
  }
  io.router_req <> route_arbiter.io.out

  when (io.router_resp.fire()) {
    val id = io.router_resp.bits.src_virt_id
    assert(states(id).g.isOneOf(g_r, g_r_stall))
    states(id).g := g_v
    states(id).r := io.router_resp.bits.out_channels
  }

  val vcalloc_arbiter = Module(new RRArbiter(
    new VCAllocReq(inParam, outParams.size, terminalOutParams.size), nVirtualChannels))
  (vcalloc_arbiter.io.in zip states).zipWithIndex.map { case ((i,s),idx) =>
    i.valid := s.g === g_v
    val bits = Wire(new VCAllocReq(inParam, outParams.size, terminalOutParams.size))
    bits.in_virt_channel := idx.U
    bits.out_channels := s.r
    bits.in_prio := s.prio
    bits.dummy := idx.U
    i.bits <> bits
    when (i.fire()) { s.g := g_v_stall }
  }
  io.vcalloc_req <> vcalloc_arbiter.io.out

  when (io.vcalloc_resp.fire()) {
    val id = io.vcalloc_resp.bits.in_virt_channel
    assert(states(id).g.isOneOf(g_v, g_v_stall))


    states(id).r := UIntToOH(io.vcalloc_resp.bits.out_channel)
    states(id).o := io.vcalloc_resp.bits.out_virt_channel
    states(id).g := g_a
  }

  (states zip io.salloc_req).zipWithIndex.map { case ((s,r),i) =>
    val credit_available = (UIntToOH(s.o) & Mux1H(s.r, io.out_credit_available.map(_.asUInt))) =/= 0.U
    r.valid := s.g === g_a && credit_available && s.c =/= 0.U
    r.bits.out_channel := OHToUInt(s.r)
    r.bits.out_virt_channel := s.o
    buffer.io.tail_read_req(i) := s.p
    r.bits.tail := buffer.io.tail_read_resp(i)
    when (r.fire()) {
      s.p := WrapInc(s.p, inParam.virtualChannelParams(i).bufferSize)
    }
    when (r.fire() && buffer.io.tail_read_resp(i)) {
      s.g := g_i
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
  val virt_channel = Mux1H(salloc_fires, states.map(_.o))
  io.out.bits.flit.virt_channel_id := RegNext(virt_channel)
  val channel_oh = Mux1H(salloc_fires, states.map(_.r))
  io.out.bits.out_channel := RegNext(OHToUInt(channel_oh))

  when (reset.asBool) {
    states.foreach(_.g := g_i)
    states.foreach(_.c := 0.U)
  }
}
