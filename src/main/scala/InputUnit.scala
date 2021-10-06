package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

class InputUnit(inParam: ChannelParams, outParams: Seq[ChannelParams])
  (implicit val p: Parameters) extends Module with HasAstroNoCParams {
  val nOutputs = outParams.size
  val nVirtualChannels = inParam.virtualChannelParams.size
  require(nVirtualChannels <= (1 << virtChannelBits))

  val io = IO(new Bundle {
    val in = Flipped(Valid(new Flit))

    val router_req = Decoupled(new RouteComputerReq(inParam))
    val router_resp = Flipped(Valid(new RouteComputerResp(inParam, nOutputs)))

    val vcalloc_req = Decoupled(new VCAllocReq(inParam, nOutputs))
    val vcalloc_resp = Flipped(Valid(new VCAllocResp(inParam, outParams)))

    val out_credit_available = Input(MixedVec(outParams.map { u => Vec(u.virtualChannelParams.size, Bool()) }))

    val salloc_req = Vec(nVirtualChannels, Decoupled(new SwitchAllocReq(outParams.size)))
    
    val out = Valid(new SwitchBundle(nOutputs))
    val credit_free = Output(Valid(UInt(log2Ceil(nVirtualChannels).W)))

    val vc_free = Output(Valid(UInt(log2Ceil(nVirtualChannels).W)))
  })
  val g_i :: g_r :: g_r_stall :: g_v :: g_v_stall :: g_a :: g_c :: Nil = Enum(7)

  class InputState extends Bundle {
    val g = UInt(3.W)
    val r = UInt(nOutputs.W)
    val o = UInt(log2Ceil(outParams.map(_.virtualChannelParams.size).max).W)
    val p = UInt(log2Ceil(inParam.virtualChannelParams.map(_.bufferSize).max).W)
    val flits_arrived = UInt((1+log2Ceil(maxFlits)).W)
    val flits_sent = UInt((1+log2Ceil(maxFlits)).W)
    val tail_seen = Bool()
    val dest_id = UInt(idBits.W)
  }
  val states = Reg(Vec(nVirtualChannels, new InputState))
  val buffer = Module(new InputBuffer(inParam))
  buffer.io.in := io.in

  when (io.in.fire() && io.in.bits.head) {
    val id = io.in.bits.virt_channel_id
    assert(id < nVirtualChannels.U)
    assert(states(id).g === g_i)

    states(id).g := g_r
    states(id).dest_id := io.in.bits.dest_id
    states(id).p := buffer.io.head
    states(id).flits_sent := 0.U
    states(id).flits_arrived := 1.U
    states(id).tail_seen := io.in.bits.tail

  } .elsewhen (io.in.fire()) {
    val id = io.in.bits.virt_channel_id
    states(id).flits_arrived := states(id).flits_arrived + 1.U
    when (io.in.bits.tail) {
      states(id).tail_seen := true.B
    }
  }

  val route_arbiter = Module(new RRArbiter(new RouteComputerReq(inParam), nVirtualChannels))
  (route_arbiter.io.in zip states).zipWithIndex.map { case ((i,s),idx) =>
    i.valid := s.g === g_r
    i.bits.dest_id := s.dest_id
    i.bits.src_virt_id := idx.U
    when (i.fire()) { s.g := g_r_stall }
  }
  io.router_req <> route_arbiter.io.out

  when (io.router_resp.fire()) {
    val id = io.router_resp.bits.src_virt_id
    assert(states(id).g.isOneOf(g_r, g_r_stall))
    states(id).g := g_v
    states(id).r := io.router_resp.bits.out_channels
  }

  val vcalloc_arbiter = Module(new RRArbiter(new VCAllocReq(inParam, outParams.size), nVirtualChannels))
  (vcalloc_arbiter.io.in zip states).zipWithIndex.map { case ((i,s),idx) =>
    i.valid := s.g === g_v
    i.bits.in_virt_channel := idx.U
    i.bits.out_channels := s.o
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

  val tail_fired = WireInit(false.B)
  ((states zip io.salloc_req) zip inParam.virtualChannelParams).map { case ((s,r),u) =>
    val c = Mux1H(UIntToOH(s.o), Mux1H(s.r, io.out_credit_available))
    r.valid := s.g === g_a && c && s.flits_sent =/= s.flits_arrived
    r.bits.out_channel := OHToUInt(s.r)
    when (r.fire()) {
      s.p := WrapInc(s.p + 1.U, u.bufferSize)
      s.flits_sent := s.flits_sent + 1.U
    }
    when (s.flits_sent === s.flits_arrived && s.tail_seen) {
      s.g := g_i
      tail_fired := true.B
    }
  }
  val salloc_fires = io.salloc_req.map(_.fire())
  val salloc_fire = salloc_fires.reduce(_||_)
  assert(PopCount(salloc_fires) <= 1.U)
  buffer.io.read_req.valid := salloc_fire
  buffer.io.read_req.bits.addr := Mux1H(salloc_fires, states.map(_.p))
  buffer.io.read_req.bits.channel := OHToUInt(salloc_fires)

  io.credit_free.valid := RegNext(salloc_fire)
  io.credit_free.bits := RegNext(OHToUInt(salloc_fires))
  io.vc_free.valid := RegNext(salloc_fire && tail_fired)
  io.vc_free.bits := RegNext(OHToUInt(salloc_fires))


  io.out.valid := RegNext(buffer.io.read_req.valid)
  io.out.bits.flit := buffer.io.read_resp
  val virt_channel_oh = Mux1H(salloc_fires, states.map(_.o))
  io.out.bits.flit.virt_channel_id := RegNext(OHToUInt(virt_channel_oh))
  val channel_oh = Mux1H(salloc_fires, states.map(_.r))
  io.out.bits.out_channel := RegNext(OHToUInt(channel_oh))

  when (reset.asBool) { states.foreach(_.g := g_i) }



}
