package constellation.router

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

import constellation.channel._

class EgressUnit(coupleSAVA: Boolean, inParams: Seq[ChannelParams], ingressParams: Seq[IngressChannelParams], cParam: EgressChannelParams)
  (implicit p: Parameters) extends AbstractOutputUnit(inParams, ingressParams, cParam)(p) {

  require(nVirtualChannels == 1)

  val io = IO(new AbstractOutputUnitIO(inParams, ingressParams, cParam) {
    val out = Decoupled(new IOFlit(cParam))
  })

  val channel_empty = RegInit(true.B)
  val q = Module(new Queue(new IOFlit(cParam), 3, flow=true))
  q.io.enq.valid := io.in(0).valid
  q.io.enq.bits.head := io.in(0).bits.head
  q.io.enq.bits.tail := io.in(0).bits.tail
  q.io.enq.bits.egress_id := io.in(0).bits.egress_id
  q.io.enq.bits.payload := io.in(0).bits.payload
  io.out <> q.io.deq
  assert(!(q.io.enq.valid && !q.io.enq.ready))

  io.credit_available(0) := q.io.count === 0.U
  io.channel_available(0) := channel_empty

  when (io.credit_alloc(0).alloc && io.credit_alloc(0).tail) {
    channel_empty := true.B
    if (coupleSAVA) io.channel_available(0) := true.B
  }

  when (io.allocs(0)) {
    channel_empty := false.B
  }

}
