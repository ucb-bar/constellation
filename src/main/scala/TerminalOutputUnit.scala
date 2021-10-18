package constellation

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

class TerminalOutputUnit(inParams: Seq[ChannelParams], terminalInParams: Seq[ChannelParams], cParam: ChannelParams)
  (implicit p: Parameters) extends AbstractOutputUnit(inParams, terminalInParams, cParam)(p) {

  require(nVirtualChannels == 1)

  val io = IO(new AbstractOutputUnitIO(inParams, terminalInParams, cParam) {
    val out = Decoupled(new Flit(cParam))
  })

  val channel_empty = RegInit(true.B)
  val q = Module(new Queue(new Flit(cParam), 3))
  q.io.enq.valid := io.in.valid
  q.io.enq.bits := io.in.bits
  io.out <> q.io.deq
  assert(!(q.io.enq.valid && !q.io.enq.ready))

  io.credit_available(0) := q.io.count <= 1.U
  io.channel_available(0) := channel_empty
  when (io.allocs(0)) {
    channel_empty := false.B
  }
  when (io.in.fire() && io.in.bits.tail) {
    channel_empty := true.B
    io.channel_available(0) := true.B
  }
}
