package constellation.router

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

import constellation.channel._
import constellation.routing.{FlowRoutingBundle}

class EgressUnit(coupleSAVA: Boolean, inParams: Seq[ChannelParams], ingressParams: Seq[IngressChannelParams], cParam: EgressChannelParams)
  (implicit p: Parameters) extends AbstractOutputUnit(inParams, ingressParams, cParam)(p) {

  class EgressUnitIO extends AbstractOutputUnitIO(inParams, ingressParams, cParam) {
    val out = Decoupled(new EgressFlit(cParam.payloadBits))
  }
  val io = IO(new EgressUnitIO)

  val channel_empty = RegInit(true.B)
  val flow = Reg(new FlowRoutingBundle)
  val q = Module(new Queue(new EgressFlit(cParam.payloadBits), 3, flow=true))
  q.io.enq.valid := io.in(0).valid
  q.io.enq.bits.head := io.in(0).bits.head
  q.io.enq.bits.tail := io.in(0).bits.tail
  val flows = cParam.possibleFlows.toSeq
  if (flows.size == 0) {
    q.io.enq.bits.ingress_id := 0.U(1.W)
  } else {
    q.io.enq.bits.ingress_id := Mux1H(
      flows.map(f => (f.ingressNode.U === io.in(0).bits.flow.ingress_node &&
        f.ingressNodeId.U === io.in(0).bits.flow.ingress_node_id)),
      flows.map(f => f.ingressId.U(ingressIdBits.W))
    )
  }
  q.io.enq.bits.payload := io.in(0).bits.payload
  io.out <> q.io.deq
  assert(!(q.io.enq.valid && !q.io.enq.ready))

  io.credit_available(0) := q.io.count === 0.U
  io.channel_status(0).occupied := !channel_empty
  io.channel_status(0).flow := flow

  when (io.credit_alloc(0).alloc && io.credit_alloc(0).tail) {
    channel_empty := true.B
    if (coupleSAVA) io.channel_status(0).occupied := false.B
  }

  when (io.allocs(0).alloc) {
    channel_empty := false.B
    flow := io.allocs(0).flow
  }

}
