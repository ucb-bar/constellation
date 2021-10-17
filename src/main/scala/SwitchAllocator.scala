package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

class CustomLockingArbiter[T <: Data](
      typ: T, arbN: Int,
      canUnlock: T => Bool,
      needsLock: Option[T => Bool] = None,
      rr: Boolean = false)
    extends Module {

  val io = IO(new Bundle {
    val in = Vec(arbN, Flipped(Decoupled(typ)))
    val out = Decoupled(typ)
  })

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

  for (i <- 0 until arbN) {
    io.in(i).ready := io.out.ready && chosen === i.U
  }

  io.out.valid := io.in(chosen).valid
  io.out.bits := io.in(chosen).bits

  def realNeedsLock(data: T): Bool =
    needsLock.map(_(data)).getOrElse(true.B)

  when (io.out.fire()) {
    when (realNeedsLock(io.out.bits)) {
      lockIdx := choice
      locked := true.B
    }
    // the unlock statement takes precedent
    when (canUnlock(io.out.bits)) {
      locked := false.B
    }
  }
}

class SwitchAllocReq(val outParams: Seq[ChannelParams], val terminalOutParams: Seq[ChannelParams])(implicit val p: Parameters) extends Bundle with HasRouterOutputParams {
  val out_channel = UInt(log2Up(nAllOutputs).W)
  val out_virt_channel = UInt(log2Up(allOutParams.map(_.virtualChannelParams.size).max).W)
  val tail = Bool()
}

class SwitchAllocator(val rParams: RouterParams)(implicit val p: Parameters) extends Module with HasRouterParams {
  val io = IO(new Bundle {
    val req = MixedVec((inParams ++ terminalInParams).map(u =>
      Vec(u.virtualChannelParams.size, Flipped(Decoupled(new SwitchAllocReq(outParams, terminalOutParams))))))
    val credit_alloc = MixedVec(outParams.map { u => Valid(UInt(log2Up(u.virtualChannelParams.size).W)) })
  })
  val nInputChannels = allInParams.map(_.virtualChannelParams.size).sum

  val in_arbs = io.req.map { r =>
    val arb = Module(new CustomLockingArbiter(
      new SwitchAllocReq(outParams, terminalOutParams),
      r.size,
      (d: SwitchAllocReq) => d.tail,
      rr = true
    ))
    arb.io.in <> r
    arb
  }
  val arbs = Seq.fill(nOutputs + nTerminalOutputs) { Module(new CustomLockingArbiter(
    new SwitchAllocReq(outParams, terminalOutParams),
    nInputs + nTerminalInputs,
    (d: SwitchAllocReq) => d.tail,
    rr = true
  )) }
  arbs.foreach(_.io.out.ready := true.B)

  in_arbs.zipWithIndex.foreach { case (o,j) =>
    val fires = Wire(Vec(arbs.size, Bool()))
    arbs.zipWithIndex.foreach { case (a,i) =>
      if (possibleTransition(allInParams(j), allOutParams(i))) {
        a.io.in(j).valid := o.io.out.valid && o.io.out.bits.out_channel === i.U
        a.io.in(j).bits := o.io.out.bits
        fires(i) := a.io.in(j).fire()
      } else {
        a.io.in(j).valid := false.B
        a.io.in(j).bits := DontCare
        fires(i) := false.B
      }
    }
    o.io.out.ready := fires.reduce(_||_)
  }

  (arbs.take(nOutputs) zip io.credit_alloc).map { case (a,i) =>
    i.valid := a.io.out.fire()
    i.bits := a.io.out.bits.out_virt_channel
  }
}
