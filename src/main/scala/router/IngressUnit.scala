package constellation.router

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

import constellation.channel._

class IngressUnit(
  ingressNodeId: Int,
  cParam: IngressChannelParams,
  outParams: Seq[ChannelParams],
  egressParams: Seq[EgressChannelParams],
  combineRCVA: Boolean,
  combineSAST: Boolean,
)
  (implicit p: Parameters) extends AbstractInputUnit(cParam, outParams, egressParams)(p) {

  class IngressUnitIO extends AbstractInputUnitIO(cParam, outParams, egressParams) {
    val in = Flipped(Decoupled(new IngressFlit(cParam.payloadBits)))
  }
  val io = IO(new IngressUnitIO)

  val route_buffer = Module(new Queue(new Flit(cParam.payloadBits), 2))
  val route_q = Module(new Queue(new RouteComputerResp(outParams, egressParams), 2,
    flow=combineRCVA))

  assert(!(io.in.valid && !cParam.possibleFlows.toSeq.map(_.egressId.U === io.in.bits.egress_id).orR))

  route_buffer.io.enq.bits.head := io.in.bits.head
  route_buffer.io.enq.bits.tail := io.in.bits.tail
  val flows = cParam.possibleFlows.toSeq
  if (flows.size == 0) {
    route_buffer.io.enq.bits.flow := DontCare
  } else {
    route_buffer.io.enq.bits.flow.ingress_node    := cParam.destId.U
    route_buffer.io.enq.bits.flow.ingress_node_id := ingressNodeId.U
    route_buffer.io.enq.bits.flow.vnet_id         := cParam.vNetId.U
    route_buffer.io.enq.bits.flow.egress_node    := Mux1H(
      flows.map(_.egressId.U === io.in.bits.egress_id),
      flows.map(_.egressNode.U)
    )
    route_buffer.io.enq.bits.flow.egress_node_id := Mux1H(
      flows.map(_.egressId.U === io.in.bits.egress_id),
      flows.map(_.egressNodeId.U)
    )
  }
  route_buffer.io.enq.bits.payload := io.in.bits.payload
  route_buffer.io.enq.bits.virt_channel_id := DontCare
  io.router_req.bits.src_virt_id := 0.U
  io.router_req.bits.flow := route_buffer.io.enq.bits.flow

  val at_dest = route_buffer.io.enq.bits.flow.egress_node === nodeId.U
  route_buffer.io.enq.valid := io.in.valid && (
    io.router_req.ready || !io.in.bits.head || at_dest)
  io.router_req.valid := io.in.valid && route_buffer.io.enq.ready && io.in.bits.head && !at_dest
  io.in.ready := route_buffer.io.enq.ready && (
    io.router_req.ready || !io.in.bits.head || at_dest)

  route_q.io.enq.valid := io.router_req.fire()
  route_q.io.enq.bits := io.router_resp
  when (io.in.fire() && io.in.bits.head && at_dest) {
    route_q.io.enq.valid := true.B
    route_q.io.enq.bits.vc_sel.foreach(_.foreach(_ := false.B))
    for (o <- 0 until nEgress) {
      when (egressParams(o).egressId.U === io.in.bits.egress_id) {
        route_q.io.enq.bits.vc_sel(o+nOutputs)(0) := true.B
      }
    }
  }
  assert(!(route_q.io.enq.valid && !route_q.io.enq.ready))

  val vcalloc_buffer = Module(new Queue(new Flit(cParam.payloadBits), 2))
  val vcalloc_q = Module(new Queue(new VCAllocResp(outParams, egressParams),
    1, pipe=true))

  vcalloc_buffer.io.enq.bits := route_buffer.io.deq.bits

  io.vcalloc_req.bits.vc_sel := route_q.io.deq.bits.vc_sel
  io.vcalloc_req.bits.flow := route_buffer.io.deq.bits.flow
  io.vcalloc_req.bits.in_vc := 0.U

  val head = route_buffer.io.deq.bits.head
  val tail = route_buffer.io.deq.bits.tail
  vcalloc_buffer.io.enq.valid := (route_buffer.io.deq.valid &&
    (route_q.io.deq.valid || !head) &&
    (io.vcalloc_req.ready || !head)
  )
  io.vcalloc_req.valid := (route_buffer.io.deq.valid && route_q.io.deq.valid &&
    head && vcalloc_buffer.io.enq.ready && vcalloc_q.io.enq.ready)
  route_buffer.io.deq.ready := (vcalloc_buffer.io.enq.ready &&
    (route_q.io.deq.valid || !head) &&
    (io.vcalloc_req.ready || !head) &&
    (vcalloc_q.io.enq.ready || !head))
  route_q.io.deq.ready := (route_buffer.io.deq.fire() && tail)


  vcalloc_q.io.enq.valid := io.vcalloc_req.fire()
  vcalloc_q.io.enq.bits := io.vcalloc_resp
  assert(!(vcalloc_q.io.enq.valid && !vcalloc_q.io.enq.ready))

  io.salloc_req(0).bits.vc_sel := vcalloc_q.io.deq.bits.vc_sel
  io.salloc_req(0).bits.tail := vcalloc_buffer.io.deq.bits.tail

  val c = (vcalloc_q.io.deq.bits.vc_sel.asUInt & io.out_credit_available.asUInt) =/= 0.U
  val vcalloc_tail = vcalloc_buffer.io.deq.bits.tail
  io.salloc_req(0).valid := vcalloc_buffer.io.deq.valid && vcalloc_q.io.deq.valid && c && !io.block
  vcalloc_buffer.io.deq.ready := io.salloc_req(0).ready && vcalloc_q.io.deq.valid && c && !io.block
  vcalloc_q.io.deq.ready := vcalloc_tail && vcalloc_buffer.io.deq.fire()

  val out_bundle = if (combineSAST) {
    Wire(Valid(new SwitchBundle(outParams, egressParams)))
  } else {
    Reg(Valid(new SwitchBundle(outParams, egressParams)))
  }
  io.out(0) := out_bundle

  out_bundle.valid := vcalloc_buffer.io.deq.fire()
  out_bundle.bits.flit := vcalloc_buffer.io.deq.bits
  out_bundle.bits.flit.virt_channel_id := 0.U
  val out_channel_oh = vcalloc_q.io.deq.bits.vc_sel.map(_.reduce(_||_)).toSeq
  out_bundle.bits.out_virt_channel := Mux1H(out_channel_oh,
    vcalloc_q.io.deq.bits.vc_sel.map(v => OHToUInt(v)).toSeq)

  io.debug.va_stall := io.vcalloc_req.valid && !io.vcalloc_req.ready
  io.debug.sa_stall := io.salloc_req(0).valid && !io.salloc_req(0).ready

  // TODO: We should not generate input/ingress/output/egress units for untraversable channels
  if (!cParam.traversable) {
    io.in.ready := false.B
    io.router_req.valid := false.B
    io.router_req.bits := DontCare
    io.vcalloc_req.valid := false.B
    io.vcalloc_req.bits := DontCare
    io.salloc_req.foreach(_.valid := false.B)
    io.salloc_req.foreach(_.bits := DontCare)
    io.out.foreach(_.valid := false.B)
    io.out.foreach(_.bits := DontCare)
  }

}
