package constellation.router

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

import constellation.channel._
import constellation.routing.{FlowRoutingBundle}
import constellation.noc.{HasNoCParams}

class AbstractInputUnitIO(
  val cParam: BaseChannelParams,
  val outParams: Seq[ChannelParams],
  val egressParams: Seq[EgressChannelParams],
)(implicit val p: Parameters) extends Bundle with HasRouterOutputParams {
  val nodeId = cParam.destId

  val router_req = Decoupled(new RouteComputerReq)
  val router_resp = Input(new RouteComputerResp(outParams, egressParams))

  val vcalloc_req = Decoupled(new VCAllocReq(cParam, outParams, egressParams))
  val vcalloc_resp = Input(new VCAllocResp(outParams, egressParams))

  val out_credit_available = Input(MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool()) }))

  val salloc_req = Vec(cParam.destSpeedup, Decoupled(new SwitchAllocReq(outParams, egressParams)))

  val out = Vec(cParam.destSpeedup, Valid(new SwitchBundle(outParams, egressParams)))
  val debug = Output(new Bundle {
    val va_stall = UInt(log2Ceil(cParam.nVirtualChannels).W)
    val sa_stall = UInt(log2Ceil(cParam.nVirtualChannels).W)
  })
  val block = Input(Bool())
}

abstract class AbstractInputUnit(
  val cParam: BaseChannelParams,
  val outParams: Seq[ChannelParams],
  val egressParams: Seq[EgressChannelParams]
)(implicit val p: Parameters) extends Module with HasRouterOutputParams with HasNoCParams {
  val nodeId = cParam.destId

  def io: AbstractInputUnitIO

}

class InputBuffer(cParam: ChannelParams)(implicit p: Parameters) extends Module {
  val nVirtualChannels = cParam.nVirtualChannels
  val io = IO(new Bundle {
    val enq = Flipped(Vec(cParam.srcSpeedup, Valid(new Flit(cParam.payloadBits))))
    val deq = Vec(cParam.nVirtualChannels, Decoupled(new BaseFlit(cParam.payloadBits)))
  })

  val useOutputQueues = cParam.useOutputQueues
  val delims = if (useOutputQueues) {
    cParam.virtualChannelParams.map(u => if (u.traversable) u.bufferSize else 0).scanLeft(0)(_+_)
  } else {
    // If no queuing, have to add an additional slot since head == tail implies empty
    // TODO this should be fixed, should use all slots available
    cParam.virtualChannelParams.map(u => if (u.traversable) u.bufferSize + 1 else 0).scanLeft(0)(_+_)
  }
  val starts = delims.dropRight(1).zipWithIndex.map { case (s,i) =>
    if (cParam.virtualChannelParams(i).traversable) s else 0
  }
  val ends = delims.tail.zipWithIndex.map { case (s,i) =>
    if (cParam.virtualChannelParams(i).traversable) s else 0
  }
  val fullSize = delims.last

  // Ugly case. Use multiple queues
  if (cParam.srcSpeedup > 1 || cParam.destSpeedup > 1 || fullSize <= 1) {
    require(useOutputQueues)
    val qs = cParam.virtualChannelParams.map(v => Module(new Queue(new BaseFlit(cParam.payloadBits), v.bufferSize)))
    qs.zipWithIndex.foreach { case (q,i) =>
      val sel = io.enq.map(f => f.valid && f.bits.virt_channel_id === i.U)
      q.io.enq.valid := sel.orR
      q.io.enq.bits.head := Mux1H(sel, io.enq.map(_.bits.head))
      q.io.enq.bits.tail := Mux1H(sel, io.enq.map(_.bits.tail))
      q.io.enq.bits.payload := Mux1H(sel, io.enq.map(_.bits.payload))
      io.deq(i) <> q.io.deq
    }
  } else {
    val mem = Mem(fullSize, new BaseFlit(cParam.payloadBits))
    val heads = RegInit(VecInit(starts.map(_.U(log2Ceil(fullSize).W))))
    val tails = RegInit(VecInit(starts.map(_.U(log2Ceil(fullSize).W))))
    val empty = (heads zip tails).map(t => t._1 === t._2)

    val qs = Seq.fill(nVirtualChannels) { Module(new Queue(new BaseFlit(cParam.payloadBits), 1, pipe=true)) }
    qs.foreach(_.io.enq.valid := false.B)
    qs.foreach(_.io.enq.bits := DontCare)

    val vc_sel = UIntToOH(io.enq(0).bits.virt_channel_id)
    val flit = Wire(new BaseFlit(cParam.payloadBits))
    val direct_to_q = (Mux1H(vc_sel, qs.map(_.io.enq.ready)) && Mux1H(vc_sel, empty)) && useOutputQueues.B
    flit.head := io.enq(0).bits.head
    flit.tail := io.enq(0).bits.tail
    flit.payload := io.enq(0).bits.payload
    when (io.enq(0).valid && !direct_to_q) {
      val tail = tails(io.enq(0).bits.virt_channel_id)
      mem.write(tail, flit)
      tails(io.enq(0).bits.virt_channel_id) := Mux(
        tail === Mux1H(vc_sel, ends.map(_ - 1).map(_ max 0).map(_.U)),
        Mux1H(vc_sel, starts.map(_.U)),
        tail + 1.U)
    } .elsewhen (io.enq(0).valid && direct_to_q) {
      for (i <- 0 until nVirtualChannels) {
        when (io.enq(0).bits.virt_channel_id === i.U) {
          qs(i).io.enq.valid := true.B
          qs(i).io.enq.bits := flit
        }
      }
    }

    if (useOutputQueues) {
      val can_to_q = (0 until nVirtualChannels).map { i => !empty(i) && qs(i).io.enq.ready }
      val to_q_oh = PriorityEncoderOH(can_to_q)
      val to_q = OHToUInt(to_q_oh)
      when (can_to_q.orR) {
        val head = Mux1H(to_q_oh, heads)
        heads(to_q) := Mux(
          head === Mux1H(to_q_oh, ends.map(_ - 1).map(_ max 0).map(_.U)),
          Mux1H(to_q_oh, starts.map(_.U)),
          head + 1.U)
        for (i <- 0 until nVirtualChannels) {
          when (to_q_oh(i)) {
            qs(i).io.enq.valid := true.B
            qs(i).io.enq.bits := mem.read(head)
          }
        }
      }
      for (i <- 0 until nVirtualChannels) {
        io.deq(i) <> qs(i).io.deq
      }
    } else {
      qs.map(_.io.deq.ready := false.B)
      val ready_sel = io.deq.map(_.ready)
      val fire = io.deq.map(_.fire())
      assert(PopCount(fire) <= 1.U)
      val head = Mux1H(fire, heads)
      when (fire.orR) {
        val fire_idx = OHToUInt(fire)
        heads(fire_idx) := Mux(
          head === Mux1H(fire, ends.map(_ - 1).map(_ max 0).map(_.U)),
          Mux1H(fire, starts.map(_.U)),
          head + 1.U)
      }
      val read_flit = mem.read(head)
      for (i <- 0 until nVirtualChannels) {
        io.deq(i).valid := !empty(i)
        io.deq(i).bits := read_flit
      }
    }
  }
}

class InputUnit(cParam: ChannelParams, outParams: Seq[ChannelParams],
  egressParams: Seq[EgressChannelParams],
  combineRCVA: Boolean, combineSAST: Boolean
)
  (implicit p: Parameters) extends AbstractInputUnit(cParam, outParams, egressParams)(p) {

  val nVirtualChannels = cParam.nVirtualChannels
  val virtualChannelParams = cParam.virtualChannelParams

  class InputUnitIO extends AbstractInputUnitIO(cParam, outParams, egressParams) {
    val in = Flipped(new Channel(cParam.asInstanceOf[ChannelParams]))
  }
  val io = IO(new InputUnitIO)

  val g_i :: g_r :: g_v :: g_a :: g_c :: Nil = Enum(5)

  class InputState extends Bundle {
    val g = UInt(3.W)
    val vc_sel = MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool()) })
    val flow = new FlowRoutingBundle
  }

  val input_buffer = Module(new InputBuffer(cParam))
  for (i <- 0 until cParam.srcSpeedup) {
    input_buffer.io.enq(i) := io.in.flit(i)
  }
  input_buffer.io.deq.foreach(_.ready := false.B)

  val route_arbiter = Module(new Arbiter(
    new RouteComputerReq, nVirtualChannels
  ))
  io.router_req <> route_arbiter.io.out

  val states = Reg(Vec(nVirtualChannels, new InputState))

  for (i <- 0 until cParam.srcSpeedup) {
    when (io.in.flit(i).fire() && io.in.flit(i).bits.head) {
      val id = io.in.flit(i).bits.virt_channel_id
      assert(id < nVirtualChannels.U)
      assert(states(id).g === g_i)
      val at_dest = io.in.flit(i).bits.flow.egress_node === nodeId.U
      states(id).g := Mux(at_dest, g_v, g_r)
      states(id).vc_sel.foreach(_.foreach(_ := false.B))
      for (o <- 0 until nEgress) {
        when (o.U === io.in.flit(i).bits.flow.egress_node_id) {
          states(id).vc_sel(o+nOutputs)(0) := true.B
        }
      }
      states(id).flow := io.in.flit(i).bits.flow
    }
  }

  (route_arbiter.io.in zip states).zipWithIndex.map { case ((i,s),idx) =>
    if (virtualChannelParams(idx).traversable) {
      i.valid := s.g === g_r
      i.bits.flow := s.flow
      i.bits.src_virt_id := idx.U
      when (i.fire()) { s.g := g_v }
    } else {
      i.valid := false.B
      i.bits := DontCare
    }
  }

  when (io.router_req.fire()) {
    val id = io.router_req.bits.src_virt_id
    assert(states(id).g === g_r)
    states(id).g := g_v
    for (i <- 0 until nVirtualChannels) {
      when (i.U === id) {
        states(i).vc_sel := io.router_resp.vc_sel
      }
    }
  }

  val mask = RegInit(0.U(nVirtualChannels.W))
  val vcalloc_reqs = Wire(Vec(nVirtualChannels, new VCAllocReq(cParam, outParams, egressParams)))
  val vcalloc_vals = Wire(Vec(nVirtualChannels, Bool()))
  val vcalloc_filter = PriorityEncoderOH(Cat(vcalloc_vals.asUInt, vcalloc_vals.asUInt & ~mask))
  val vcalloc_sel = vcalloc_filter(nVirtualChannels-1,0) | (vcalloc_filter >> nVirtualChannels)
  // Prioritize incoming packetes
  when (io.router_req.fire()) {
    mask := (1.U << io.router_req.bits.src_virt_id) - 1.U
  } .elsewhen (vcalloc_vals.orR) {
    mask := Mux1H(vcalloc_sel, (0 until nVirtualChannels).map { w => ~(0.U((w+1).W)) })
  }
  io.vcalloc_req.valid := vcalloc_vals.orR
  io.vcalloc_req.bits := Mux1H(vcalloc_sel, vcalloc_reqs)

  states.zipWithIndex.map { case (s,idx) =>
    if (virtualChannelParams(idx).traversable) {
      vcalloc_vals(idx) := s.g === g_v
      vcalloc_reqs(idx).in_vc := idx.U
      vcalloc_reqs(idx).vc_sel := s.vc_sel
      vcalloc_reqs(idx).flow := s.flow
      when (vcalloc_vals(idx) && vcalloc_sel(idx) && io.vcalloc_req.ready) { s.g := g_a }
      if (combineRCVA) {
        when (route_arbiter.io.in(idx).fire()) {
          vcalloc_vals(idx) := true.B
          vcalloc_reqs(idx).vc_sel := io.router_resp.vc_sel
        }
      }
    } else {
      vcalloc_vals(idx) := false.B
      vcalloc_reqs(idx) := DontCare
    }
  }

  io.debug.va_stall := PopCount(vcalloc_vals) - io.vcalloc_req.ready

  when (io.vcalloc_req.fire()) {
    for (i <- 0 until nVirtualChannels) {
      when (vcalloc_sel(i)) {
        states(i).vc_sel := io.vcalloc_resp.vc_sel
        states(i).g := g_a
        if (!combineRCVA) {
          assert(states(i).g === g_v)
        }
      }
    }
  }
  val salloc_arb = Module(new SwitchArbiter(
    nVirtualChannels,
    cParam.destSpeedup,
    outParams, egressParams
  ))

  (states zip salloc_arb.io.in).zipWithIndex.map { case ((s,r),i) =>
    if (virtualChannelParams(i).traversable) {
      val credit_available = (s.vc_sel.asUInt & io.out_credit_available.asUInt) =/= 0.U
      r.valid := s.g === g_a && credit_available && input_buffer.io.deq(i).valid
      r.bits.vc_sel := s.vc_sel
      val deq_tail = input_buffer.io.deq(i).bits.tail
      r.bits.tail := deq_tail
      when (r.fire() && deq_tail) {
        s.g := g_i
      }
      input_buffer.io.deq(i).ready := r.ready
    } else {
      r.valid := false.B
      r.bits := DontCare
    }
  }
  io.debug.sa_stall := PopCount(salloc_arb.io.in.map(r => r.valid && !r.ready))
  io.salloc_req <> salloc_arb.io.out
  when (io.block) {
    salloc_arb.io.out.foreach(_.ready := false.B)
    io.salloc_req.foreach(_.valid := false.B)
  }

  class OutBundle extends Bundle {
    val valid = Bool()
    val vid = UInt(virtualChannelBits.W)
    val out_vid = UInt(log2Up(allOutParams.map(_.nVirtualChannels).max).W)
    val flit = new Flit(cParam.payloadBits)
  }

  val salloc_outs = if (combineSAST) {
    Wire(Vec(cParam.destSpeedup, new OutBundle))
  } else {
    Reg(Vec(cParam.destSpeedup, new OutBundle))
  }

  io.in.credit_return := salloc_arb.io.out.zipWithIndex.map { case (o, i) =>
    Mux(o.fire(), salloc_arb.io.chosen_oh(i), 0.U)
  }.reduce(_|_)
  io.in.vc_free := salloc_arb.io.out.zipWithIndex.map { case (o, i) =>
    Mux(o.fire() && Mux1H(salloc_arb.io.chosen_oh(i), input_buffer.io.deq.map(_.bits.tail)),
      salloc_arb.io.chosen_oh(i), 0.U)
  }.reduce(_|_)

  for (i <- 0 until cParam.destSpeedup) {
    val salloc_out = salloc_outs(i)
    salloc_out.valid := salloc_arb.io.out(i).fire()
    salloc_out.vid := OHToUInt(salloc_arb.io.chosen_oh(i))
    val vc_sel = Mux1H(salloc_arb.io.chosen_oh(i), states.map(_.vc_sel))
    val channel_oh = vc_sel.map(_.reduce(_||_)).toSeq
    val virt_channel = Mux1H(channel_oh, vc_sel.map(v => OHToUInt(v)).toSeq)
    when (salloc_arb.io.out(i).fire()) {
      salloc_out.out_vid := virt_channel
      salloc_out.flit.payload := Mux1H(salloc_arb.io.chosen_oh(i), input_buffer.io.deq.map(_.bits.payload))
      salloc_out.flit.head    := Mux1H(salloc_arb.io.chosen_oh(i), input_buffer.io.deq.map(_.bits.head))
      salloc_out.flit.tail    := Mux1H(salloc_arb.io.chosen_oh(i), input_buffer.io.deq.map(_.bits.tail))
      salloc_out.flit.flow    := Mux1H(salloc_arb.io.chosen_oh(i), states.map(_.flow))
    } .otherwise {
      salloc_out.out_vid := DontCare
      salloc_out.flit := DontCare
    }

    salloc_out.flit.virt_channel_id := DontCare // this gets set in the switch

    io.out(i).valid := salloc_out.valid
    io.out(i).bits.flit := salloc_out.flit
    io.out(i).bits.out_virt_channel := salloc_out.out_vid
  }

  def filterVCSel(sel: MixedVec[Vec[Bool]], srcV: Int) = {
    if (virtualChannelParams(srcV).traversable) {
      outParams.zipWithIndex.map { case (oP, oI) =>
        (0 until oP.nVirtualChannels).map { oV =>
          var allow = false
          virtualChannelParams(srcV).possibleFlows.foreach { pI =>
            allow = allow || routingRelation(
              cParam.channelRoutingInfos(srcV),
              oP.channelRoutingInfos(oV),
              pI
            )
          }
          if (!allow)
            sel(oI)(oV) := false.B
        }
      }
    }
  }

  (0 until nVirtualChannels).map { i =>
    if (!virtualChannelParams(i).traversable) states(i) := DontCare
    filterVCSel(states(i).vc_sel, i)
  }
  when (reset.asBool) {
    states.foreach(_.g := g_i)
  }
}
