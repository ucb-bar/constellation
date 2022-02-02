package constellation.util

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

// Grants a lock to a requestor, then preserves the grant until the requestor
// unlocks, or deasserts valid
class GrantHoldArbiter[T <: Data](
      typ: T, arbN: Int,
      canUnlock: T => Bool,
      needsLock: Option[T => Bool] = None,
      rr: Boolean = false)
    extends Module {

  val io = IO(new ArbiterIO(typ, arbN))

  def rotateLeft[T <: Data](norm: Vec[T], rot: UInt): Seq[T] = {
    val n = norm.size
    Seq.tabulate(n) { i =>
      Mux(rot < (n - i).U, norm(i.U + rot), norm(rot - (n - i).U))
    }
  }

  val lockIdx = RegInit(0.U(log2Up(arbN).W))
  val locked = RegInit(false.B)

  val choice = if (rr) {
    PriorityMux(
      rotateLeft(VecInit(io.in.map(_.valid)), lockIdx + 1.U),
      rotateLeft(VecInit((0 until arbN).map(_.U)), lockIdx + 1.U))
  } else {
    PriorityEncoder(io.in.map(_.valid))
  }

  val chosen = Mux(locked && io.in(lockIdx).valid, lockIdx, choice)
  io.chosen := chosen

  for (i <- 0 until arbN) {
    io.in(i).ready := io.out.ready && chosen === i.U
  }

  io.out.valid := io.in(chosen).valid
  io.out.bits := io.in(chosen).bits

  def realNeedsLock(data: T): Bool =
    needsLock.map(_(data)).getOrElse(true.B)

  when (io.out.fire() && realNeedsLock(io.out.bits)) {
    lockIdx := choice
    locked := true.B
  } .elsewhen (!locked) {
    lockIdx := WrapInc(lockIdx, arbN)
  }

  // the unlock statement takes precedent
  when (io.out.fire() && canUnlock(io.out.bits)) {
    locked := false.B
  }

}
