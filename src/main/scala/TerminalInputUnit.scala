package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

class TerminalInputUnit(inParam: ChannelParams, outParams: Seq[ChannelParams], terminalOutParams: Seq[ChannelParams])
  (implicit p: Parameters) extends AbstractInputUnit()(p) {
  val nOutputs = outParams.size
  require(inParam.isInput && !inParam.isOutput)

  val io = IO(new AbstractInputUnitIO(inParam, outParams, terminalOutParams) {
    val in = Flipped(Vec(inParam.nVirtualChannels, Decoupled(new Flit)))
  })
  io.in.foreach { i => assert(!(i.fire() && outIdToDestId(i.bits.out_id) === inParam.destId.U)) }

  val route_buffers = Seq.fill(inParam.nVirtualChannels) { Module(new Queue(new Flit, 2)) }
  val route_qs = Seq.fill(inParam.nVirtualChannels) { Module(new Queue(new RouteComputerResp(inParam, nOutputs), 4)) }
  val route_arbiter = Module(new Arbiter(new RouteComputerReq(inParam), inParam.nVirtualChannels))
  io.router_req <> route_arbiter.io.out
  (0 until inParam.nVirtualChannels).foreach { i =>
    route_buffers(i).io.enq.bits := io.in(i).bits
    route_arbiter.io.in(i).bits.src_virt_id := i.U
    route_arbiter.io.in(i).bits.src_prio := io.in(i).bits.prio
    route_arbiter.io.in(i).bits.dest_id := outIdToDestId(io.in(i).bits.out_id)

    route_buffers(i).io.enq.valid := io.in(i).valid && (route_arbiter.io.in(i).ready || !io.in(i).bits.head)
    route_arbiter.io.in(i).valid := io.in(i).valid && route_buffers(i).io.enq.ready && io.in(i).bits.head
    io.in(i).ready := route_buffers(i).io.enq.ready && (route_arbiter.io.in(i).ready || !io.in(i).bits.head)
  }

  route_qs.zipWithIndex.foreach { case (q,i) =>
    q.io.enq.valid := io.router_resp.valid && io.router_resp.bits.src_virt_id === i.U
    q.io.enq.bits := io.router_resp.bits
    assert(!(q.io.enq.valid && !q.io.enq.ready))
  }

  val vcalloc_buffers = Seq.fill(inParam.nVirtualChannels) { Module(new Queue(new Flit, 2)) }
  val vcalloc_qs = Seq.fill(inParam.nVirtualChannels) { Module(new Queue(new VCAllocResp(inParam, outParams), 2)) }
  val vcalloc_arbiter = Module(new Arbiter(new VCAllocReq(inParam, outParams.size, terminalOutParams.size), inParam.nVirtualChannels))
  io.vcalloc_req <> vcalloc_arbiter.io.out
  (0 until inParam.nVirtualChannels).foreach { i =>
    vcalloc_buffers(i).io.enq.bits := route_buffers(i).io.deq.bits

    vcalloc_arbiter.io.in(i).bits.in_virt_channel := i.U
    vcalloc_arbiter.io.in(i).bits.in_prio := route_buffers(i).io.deq.bits.prio
    vcalloc_arbiter.io.in(i).bits.out_channels := route_qs(i).io.deq.bits.out_channels
    vcalloc_arbiter.io.in(i).bits.dummy := i.U

    val head = route_buffers(i).io.deq.bits.head
    val tail = route_buffers(i).io.deq.bits.tail
    vcalloc_buffers(i).io.enq.valid := (route_buffers(i).io.deq.valid &&
      (route_qs(i).io.deq.valid || !head) &&
      (vcalloc_arbiter.io.in(i).ready || !head)
    )
    vcalloc_arbiter.io.in(i).valid := (route_buffers(i).io.deq.valid && route_qs(i).io.deq.valid &&
      head && vcalloc_buffers(i).io.enq.ready)
    route_buffers(i).io.deq.ready := (vcalloc_buffers(i).io.enq.ready &&
      (route_qs(i).io.deq.valid || !head) &&
      (vcalloc_arbiter.io.in(i).ready || !head)
    )
    route_qs(i).io.deq.ready := (route_buffers(i).io.deq.fire() && tail)
  }

  vcalloc_qs.zipWithIndex.foreach { case (q,i) =>
    q.io.enq.valid := io.vcalloc_resp.valid && io.vcalloc_resp.bits.in_virt_channel === i.U
    q.io.enq.bits := io.vcalloc_resp.bits
    assert(!(q.io.enq.valid && !q.io.enq.ready))
  }

  val out_oh = Wire(Vec(inParam.nVirtualChannels, Bool()))
  (0 until inParam.nVirtualChannels).foreach { i =>
    io.salloc_req(i).bits.out_channel := vcalloc_qs(i).io.deq.bits.out_channel
    io.salloc_req(i).bits.out_virt_channel := vcalloc_qs(i).io.deq.bits.out_virt_channel


    val c = Mux1H(UIntToOH(vcalloc_qs(i).io.deq.bits.out_virt_channel),
      Mux1H(vcalloc_qs(i).io.deq.bits.out_channel, io.out_credit_available))
    val tail = vcalloc_buffers(i).io.deq.bits.tail
    io.salloc_req(i).valid := vcalloc_buffers(i).io.deq.valid && vcalloc_qs(i).io.deq.valid && route_qs(i).io.deq.valid && c
    vcalloc_buffers(i).io.deq.ready := io.salloc_req(i).ready && vcalloc_qs(i).io.deq.valid && route_qs(i).io.deq.valid && c
    vcalloc_qs(i).io.deq.ready := tail && vcalloc_buffers(i).io.deq.fire()
    route_qs(i).io.deq.ready := tail && vcalloc_buffers(i).io.deq.fire()
    out_oh(i) := vcalloc_buffers(i).io.deq.fire()
  }
  assert(PopCount(out_oh) <= 1.U)

  io.out.valid := RegNext(out_oh.reduce(_||_))
  io.out.bits.flit := RegNext(Mux1H(out_oh, vcalloc_buffers.map(_.io.deq.bits)))
  io.out.bits.flit.virt_channel_id := RegNext(Mux1H(out_oh, vcalloc_qs.map(_.io.deq.bits.out_virt_channel)))
  io.out.bits.out_channel := RegNext(Mux1H(out_oh, vcalloc_qs.map(_.io.deq.bits.out_channel)))

}
