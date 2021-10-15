package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

class TerminalInputUnit(inParam: ChannelParams, outParams: Seq[ChannelParams], terminalOutParams: Seq[ChannelParams])
  (implicit p: Parameters) extends AbstractInputUnit()(p) {
  val nOutputs = outParams.size
  require(inParam.isInput && !inParam.isOutput)
  require(inParam.nVirtualChannels == 1)

  val io = IO(new AbstractInputUnitIO(inParam, outParams, terminalOutParams) {
    val in = Flipped(Decoupled(new Flit))
  })
  assert(!(io.in.fire() && outIdToDestId(io.in.bits.out_id) === inParam.destId.U))

  val route_buffer = Module(new Queue(new Flit, 2))
  val route_q = Module(new Queue(new RouteComputerResp(inParam, nOutputs), 4))

  route_buffer.io.enq.bits := io.in.bits
  io.router_req.bits.src_virt_id := 0.U
  io.router_req.bits.src_prio := io.in.bits.prio
  io.router_req.bits.dest_id := outIdToDestId(io.in.bits.out_id)

  route_buffer.io.enq.valid := io.in.valid && (io.router_req.ready || !io.in.bits.head)
  io.router_req.valid := io.in.valid && route_buffer.io.enq.ready && io.in.bits.head
  io.in.ready := route_buffer.io.enq.ready && (io.router_req.ready || !io.in.bits.head)

  route_q.io.enq.valid := io.router_resp.valid
  route_q.io.enq.bits := io.router_resp.bits
  assert(!(route_q.io.enq.valid && !route_q.io.enq.ready))

  val vcalloc_buffer = Module(new Queue(new Flit, 2))
  val vcalloc_q = Module(new Queue(new VCAllocResp(inParam, outParams), 2))

  vcalloc_buffer.io.enq.bits := route_buffer.io.deq.bits

  io.vcalloc_req.bits.in_virt_channel := 0.U
  io.vcalloc_req.bits.in_prio := route_buffer.io.deq.bits.prio
  io.vcalloc_req.bits.out_channels := route_q.io.deq.bits.out_channels
  io.vcalloc_req.bits.dummy := 0.U

  val head = route_buffer.io.deq.bits.head
  val tail = route_buffer.io.deq.bits.tail
  vcalloc_buffer.io.enq.valid := (route_buffer.io.deq.valid &&
    (route_q.io.deq.valid || !head) &&
    (io.vcalloc_req.ready || !head)
  )
  io.vcalloc_req.valid := (route_buffer.io.deq.valid && route_q.io.deq.valid &&
    head && vcalloc_buffer.io.enq.ready)
  route_buffer.io.deq.ready := (vcalloc_buffer.io.enq.ready &&
    (route_q.io.deq.valid || !head) &&
    (io.vcalloc_req.ready || !head))
  route_q.io.deq.ready := (route_buffer.io.deq.fire() && tail)


  vcalloc_q.io.enq.valid := io.vcalloc_resp.valid
  vcalloc_q.io.enq.bits := io.vcalloc_resp.bits
  assert(!(vcalloc_q.io.enq.valid && !vcalloc_q.io.enq.ready))

  io.salloc_req(0).bits.out_channel := vcalloc_q.io.deq.bits.out_channel
  io.salloc_req(0).bits.out_virt_channel := vcalloc_q.io.deq.bits.out_virt_channel
  io.salloc_req(0).bits.tail := vcalloc_buffer.io.deq.bits.tail

  val c = Mux1H(UIntToOH(vcalloc_q.io.deq.bits.out_virt_channel),
    Mux1H(vcalloc_q.io.deq.bits.out_channel, io.out_credit_available))
  val vcalloc_tail = vcalloc_buffer.io.deq.bits.tail
  io.salloc_req(0).valid := vcalloc_buffer.io.deq.valid && vcalloc_q.io.deq.valid && route_q.io.deq.valid && c
  vcalloc_buffer.io.deq.ready := io.salloc_req(0).ready && vcalloc_q.io.deq.valid && route_q.io.deq.valid && c
  vcalloc_q.io.deq.ready := vcalloc_tail && vcalloc_buffer.io.deq.fire()
  route_q.io.deq.ready := vcalloc_tail && vcalloc_buffer.io.deq.fire()

  io.out.valid := RegNext(vcalloc_buffer.io.deq.fire())
  io.out.bits.flit := RegNext(vcalloc_buffer.io.deq.bits)
  io.out.bits.flit.virt_channel_id := RegNext(vcalloc_q.io.deq.bits.out_virt_channel)
  io.out.bits.out_channel := RegNext(vcalloc_q.io.deq.bits.out_channel)

}
