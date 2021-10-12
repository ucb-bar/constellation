package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._


class TerminalOutputUnit(inParams: Seq[ChannelParams], terminalInParams: Seq[ChannelParams], outParam: ChannelParams)(implicit p: Parameters) extends AbstractOutputUnit(inParams, terminalInParams, outParam)(p) {
  val nInputs = inParams.size
  require(outParam.nVirtualChannels == nPrios)
  val io = IO(new AbstractOutputUnitIO(inParams, terminalInParams, outParam) {
    val out = Vec(nPrios, Decoupled(new Flit))
  })

  val channel_empty = Reg(Vec(nPrios, Bool()))
  val qs = Seq.fill(nPrios) { Module(new Queue(new Flit, 3)) }
  qs.zipWithIndex.map { case (q,i) =>
    q.io.enq.valid := io.in.valid && io.in.bits.virt_channel_id === i.U
    q.io.enq.bits := io.in.bits
    assert(!(q.io.enq.valid && !q.io.enq.ready))
  }
  (qs zip io.credit_available).map { case (q,a) => a := q.io.count <= 1.U }
  io.channel_available := channel_empty
  when (io.alloc.valid) {
    channel_empty(io.alloc.bits.out_virt_channel) := false.B
  }
  when (io.in.fire() && io.in.bits.tail) {
    channel_empty(io.in.bits.virt_channel_id) := true.B
  }

  when (reset.asBool) { channel_empty.foreach(_ := true.B) }
}
