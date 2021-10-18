package constellation

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

class SwitchAllocReq(val outParams: Seq[ChannelParams], val terminalOutParams: Seq[ChannelParams])(implicit val p: Parameters) extends Bundle with HasRouterOutputParams {
  val out_channel = UInt(log2Up(nAllOutputs).W)
  val out_virt_channel = UInt(log2Up(allOutParams.map(_.virtualChannelParams.size).max).W)
  val tail = Bool()
}

class SwitchAllocator(val rParams: RouterParams)(implicit val p: Parameters) extends Module with HasRouterParams {
  val io = IO(new Bundle {
    val req = MixedVec(allInParams.map(u =>
      Vec(u.nVirtualChannels, Flipped(Decoupled(new SwitchAllocReq(outParams, terminalOutParams))))))
    val credit_alloc = MixedVec(outParams.map { u => Valid(UInt(log2Up(u.nVirtualChannels).W)) })
  })
  val nInputChannels = allInParams.map(_.nVirtualChannels).sum

  val in_arbs = io.req.map { r =>
    val arb = Module(new GrantHoldArbiter(
      new SwitchAllocReq(outParams, terminalOutParams),
      r.size,
      (d: SwitchAllocReq) => d.tail,
      rr = true
    ))
    arb.io.in <> r
    arb
  }
  val arbs = Seq.fill(nAllOutputs) { Module(new GrantHoldArbiter(
    new SwitchAllocReq(outParams, terminalOutParams),
    nAllInputs,
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
