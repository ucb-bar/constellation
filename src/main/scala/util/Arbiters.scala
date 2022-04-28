package constellation.util

import chisel3._
import chisel3.util._
import chisel3.util.random.LFSR

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

// Grants a lock to a requestor, then preserves the grant until the requestor
// unlocks, or deasserts valid
class GrantHoldArbiter[T <: Data](
      typ: T, arbN: Int,
      canUnlock: T => Bool,
      needsLock: Option[T => Bool] = None,
      policy: ArbiterPolicy.ArbiterPolicy = ArbiterPolicy.LowestFirst,
      nOut: Int = 1
) extends Module {

  val io = IO(new Bundle {
    val in = Flipped(Vec(arbN, Decoupled(typ)))
    val out = Vec(nOut, Decoupled(typ))
    val chosen = Vec(nOut, Output(UInt(log2Ceil(arbN).W)))
    val chosen_oh = Vec(nOut, Output(UInt(arbN.W)))
  })

  def rotateLeft[T <: Data](norm: Vec[T], rot: UInt): Seq[T] = {
    val n = norm.size
    Seq.tabulate(n) { i =>
      Mux(rot < (n - i).U, norm(i.U + rot), norm(rot - (n - i).U))
    }
  }

  val lockIdx = Seq.fill(nOut) { RegInit(0.U(log2Up(arbN).W)) }
  val locked = Seq.fill(nOut) { RegInit(false.B) }
  val valids = (0 until arbN).map(i => io.in(i).valid && !(0 until nOut).map(j => locked(j) && lockIdx(j) === i.U).orR)

  val choices = Wire(Vec(nOut, UInt(log2Ceil(arbN).W)))
  choices(0) := (if (policy == ArbiterPolicy.RoundRobin) {
    PriorityMux(
      rotateLeft(VecInit(valids), lockIdx(0) + 1.U),
      rotateLeft(VecInit((0 until arbN).map(_.U)), lockIdx(0) + 1.U))
  } else if (policy == ArbiterPolicy.Random) {
    val rand = LFSR(16)(log2Up(arbN)-1,0)
    PriorityMux(
      rotateLeft(VecInit(valids), rand + 1.U),
      rotateLeft(VecInit((0 until arbN).map(_.U)), rand + 1.U))
  } else {
    PriorityEncoder(valids)
  })

  var next_valids = valids.asUInt & ~Mux(locked(0), 0.U(arbN.W), UIntToOH(choices(0)))
  for (i <- 1 until nOut) {
    val sel = PriorityEncoderOH(next_valids)
    choices(i) := OHToUInt(sel)
    next_valids = next_valids & ~Mux(locked(i), 0.U(arbN.W), sel)
  }

  def realNeedsLock(data: T): Bool =
    needsLock.map(_(data)).getOrElse(true.B)


  io.in.foreach(_.ready := false.B)
  var chosens = 0.U(arbN.W)
  for (i <- 0 until nOut) {
    val in_valids = (0 until arbN).map { j => io.in(j).valid && !chosens(j) }
    val chosen = Mux(locked(i) && in_valids(lockIdx(i)), lockIdx(i), choices(i))
    io.chosen(i) := chosen
    io.chosen_oh(i) := UIntToOH(chosen)
    io.out(i).valid := in_valids(chosen)
    io.out(i).bits := io.in(chosen).bits

    for (j <- 0 until arbN) {
      when (chosen === j.U && io.out(i).ready) {
        io.in(j).ready := true.B
      }
    }
    chosens = chosens | (1.U << chosen)

    when (io.out(i).fire() && realNeedsLock(io.out(i).bits)) {
      lockIdx(i) := chosen
      locked(i) := true.B
    } .elsewhen (!locked(i)) {
      lockIdx(i) := WrapInc(lockIdx(i), arbN)
    }

    // the unlock statement takes precedence
    when (io.out(i).fire && canUnlock(io.out(i).bits)) {
      locked(i) := false.B
    }
  }

}
