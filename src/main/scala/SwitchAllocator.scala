package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._


class SwitchAllocReq(val allOutParams: Seq[ChannelParams])(implicit val p: Parameters) extends Bundle with HasAstroNoCParams {
  val out_channel = UInt(log2Up(allOutParams.size).W)
  val out_virt_channel = UInt(log2Up(allOutParams.map(_.virtualChannelParams.size).max).W)
}

class SwitchAllocator(val rParams: RouterParams)(implicit val p: Parameters) extends Module with HasRouterParams {
  val io = IO(new Bundle {
    val req = MixedVec((inParams ++ terminalInParams).map(u =>
      Vec(u.virtualChannelParams.size, Flipped(Decoupled(new SwitchAllocReq(allOutParams))))))
    val credit_alloc = MixedVec(outParams.map { u => Valid(UInt(log2Up(u.virtualChannelParams.size).W)) })
  })
  val nInputChannels = allInParams.map(_.virtualChannelParams.size).sum

  val in_arbs = io.req.map { r =>
    val arb = Module(new Arbiter(new SwitchAllocReq(allOutParams), r.size))
    arb.io.in <> r
    arb
  }
  val arbs = allOutParams.map { u => Module(new RRArbiter(UInt(log2Up(u.virtualChannelParams.size).W), nInputs + nTerminalInputs)) }
  arbs.foreach(_.io.out.ready := true.B)

  in_arbs.zipWithIndex.foreach { case (o,j) =>
    arbs.zipWithIndex.foreach { case (a,i) =>
      a.io.in(j).valid := o.io.out.valid && o.io.out.bits.out_channel === i.U
      a.io.in(j).bits := o.io.out.bits.out_virt_channel
    }
    o.io.out.ready := arbs.map(_.io.in(j).ready).reduce(_||_)
  }

  (arbs.take(nOutputs) zip io.credit_alloc).map { case (a,i) =>
    i.valid := a.io.out.fire()
    i.bits := a.io.out.bits
  }
}
