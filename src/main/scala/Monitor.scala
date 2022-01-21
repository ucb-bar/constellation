package constellation

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._

class NoCMonitor(edge: ChannelEdgeParams) extends Module {
  val io = IO(new Bundle {
    val in = Input(new Channel(edge.cp)(edge.p))
  })
  println("instantiating monitor")
  val in_flight = RegInit(false.B)
  when (io.in.flit.valid) {
    when (io.in.flit.bits.head) { in_flight := true.B }
    when (io.in.flit.bits.tail) { in_flight := false.B }
    assert (io.in.flit.bits.head ^ in_flight, "Flit head/tail sequencing is messed up")
  }

}
