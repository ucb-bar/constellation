package astronoc

import chisel3._
import chisel3.util._

object WrapInc {
  // "n" is the number of increments, so we wrap at n-1.
  def apply(value: UInt, n: Int): UInt = {
    if (isPow2(n)) {
      (value + 1.U)(log2Ceil(n)-1,0)
    } else {
      val wrap = (value === (n-1).U)
      Mux(wrap, 0.U, value + 1.U)
    }
  }

  def apply(value: UInt, n: UInt): UInt = {
    val wrap = value === n - 1.U
    Mux(wrap, 0.U, value + 1.U)
  }
}


class Pipeline[T <: Data](gen: T, entries: Int) extends Module {
  require(entries > 0)
  val io = IO(new QueueIO(gen, entries))

  val regs = Seq.fill(entries) { Module(new Queue(gen, 1, pipe=true)) }

  io.count := regs.map(_.io.count).reduce(_+&_)
  regs(0).io.enq <> io.enq
  io.deq <> regs(entries-1).io.deq

  (regs.drop(1) zip regs.dropRight(1)).map { case (r,l) => l.io.enq <> r.io.deq }
}
