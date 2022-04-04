package constellation.router

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

import constellation.channel._

class IngressUnit(
  cParam: IngressChannelParams,
  outParams: Seq[ChannelParams],
  egressParams: Seq[EgressChannelParams],
  combineRCVA: Boolean,
  combineSAST: Boolean,
)
  (implicit p: Parameters) extends AbstractInputUnit(cParam, outParams, egressParams)(p) {

  val io = IO(new AbstractInputUnitIO(cParam, outParams, egressParams) {
    val in = Flipped(Decoupled(new IOFlit(cParam)))
  })

  val route_buffer = Module(new Queue(new IOFlit(cParam), 2))
  val route_q = Module(new Queue(new RouteComputerResp(cParam, outParams, egressParams), 2,
    flow=combineRCVA))

  route_buffer.io.enq.bits := io.in.bits
  io.router_req.bits.src_virt_id := 0.U
  io.router_req.bits.route_info.vnet := cParam.vNetId.U
  io.router_req.bits.route_info.egress := io.in.bits.egress_id

  val at_dest = atDest(io.in.bits.egress_id)
  route_buffer.io.enq.valid := io.in.valid && (
    io.router_req.ready || !io.in.bits.head || (at_dest && !io.router_resp.valid))
  io.router_req.valid := io.in.valid && route_buffer.io.enq.ready && io.in.bits.head && !at_dest
  io.in.ready := route_buffer.io.enq.ready && (
    io.router_req.ready || !io.in.bits.head || (at_dest && !io.router_resp.valid))

  route_q.io.enq.valid := io.router_resp.valid
  route_q.io.enq.bits := io.router_resp.bits
  when (io.in.fire() && io.in.bits.head && at_dest) {
    route_q.io.enq.valid := true.B
    route_q.io.enq.bits.src_virt_id := 0.U
    route_q.io.enq.bits.vc_sel.foreach(_.foreach(_ := false.B))
    for (o <- 0 until nEgress) {
      when (egressParams(o).egressId.U === io.in.bits.egress_id) {
        route_q.io.enq.bits.vc_sel(o+nOutputs)(0) := true.B
      }
    }
  }
  assert(!(route_q.io.enq.valid && !route_q.io.enq.ready))

  val vcalloc_buffer = Module(new Queue(new IOFlit(cParam), 2))
  val vcalloc_q = Module(new Queue(new VCAllocResp(cParam, outParams, egressParams),
    1, pipe=true))

  vcalloc_buffer.io.enq.bits := route_buffer.io.deq.bits

  io.vcalloc_req(0).bits := route_q.io.deq.bits.vc_sel

  val head = route_buffer.io.deq.bits.head
  val tail = route_buffer.io.deq.bits.tail
  vcalloc_buffer.io.enq.valid := (route_buffer.io.deq.valid &&
    (route_q.io.deq.valid || !head) &&
    (io.vcalloc_req(0).ready || !head)
  )
  io.vcalloc_req(0).valid := (route_buffer.io.deq.valid && route_q.io.deq.valid &&
    head && vcalloc_buffer.io.enq.ready && vcalloc_q.io.enq.ready)
  route_buffer.io.deq.ready := (vcalloc_buffer.io.enq.ready &&
    (route_q.io.deq.valid || !head) &&
    (io.vcalloc_req(0).ready || !head) &&
    (vcalloc_q.io.enq.ready || !head))
  route_q.io.deq.ready := (route_buffer.io.deq.fire() && tail)


  vcalloc_q.io.enq.valid := io.vcalloc_resp.valid
  vcalloc_q.io.enq.bits := io.vcalloc_resp.bits
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
  out_bundle.bits.flit.head := vcalloc_buffer.io.deq.bits.head
  out_bundle.bits.flit.tail := vcalloc_buffer.io.deq.bits.tail
  out_bundle.bits.flit.egress_id := vcalloc_buffer.io.deq.bits.egress_id
  out_bundle.bits.flit.payload := vcalloc_buffer.io.deq.bits.payload
  out_bundle.bits.flit.vnet_id := cParam.vNetId.U
  out_bundle.bits.flit.virt_channel_id := 0.U
  val out_channel_oh = vcalloc_q.io.deq.bits.vc_sel.map(_.reduce(_||_))
  out_bundle.bits.out_virt_channel := Mux1H(out_channel_oh, vcalloc_q.io.deq.bits.vc_sel.map(v => OHToUInt(v)))

  io.debug.va_stall := io.vcalloc_req(0).valid && !io.vcalloc_req(0).ready
  io.debug.sa_stall := io.salloc_req(0).valid && !io.salloc_req(0).ready
}
