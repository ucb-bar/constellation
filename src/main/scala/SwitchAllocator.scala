package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._


class SwitchAllocReq(val outParams: Seq[ChannelParams])(implicit val p: Parameters) extends Bundle with HasAstroNoCParams {
  val out_channel = UInt(log2Up(outParams.size).W)
  val out_virt_channel = UInt(log2Up(outParams.map(_.virtualChannelParams.size).max).W)
}

class SwitchAllocator(val rParams: RouterParams)(implicit val p: Parameters) extends Module with HasRouterParams {
  val io = IO(new Bundle {
    val req = MixedVec((inParams ++ terminalInParams).map(u =>
      Vec(u.virtualChannelParams.size, Flipped(Decoupled(new SwitchAllocReq(outParams))))))
    val credit_alloc = MixedVec(outParams.map { u => Valid(UInt(log2Up(u.virtualChannelParams.size).W)) })
  })
  val nInputChannels = allInParams.map(_.virtualChannelParams.size).sum

  val arbs = outParams.map { u => Module(new RRArbiter(UInt(log2Up(u.virtualChannelParams.size).W), nInputChannels)) }
  arbs.foreach(_.io.out.ready := true.B)

  var idx = 0
  for (j <- 0 until allInParams.map(_.virtualChannelParams.size).max) {
    for (i <- 0 until allInParams.size) {
      if (j < allInParams(i).virtualChannelParams.size) {
        arbs.zipWithIndex.map { case (a,k) =>
          a.io.in(idx).valid := io.req(i)(j).valid && io.req(i)(j).bits.out_channel === k.U
          a.io.in(idx).bits := io.req(i)(j).bits.out_virt_channel
        }
        io.req(i)(j).ready := arbs.map(_.io.in(idx).ready).reduce(_||_)
        idx += 1
      }
    }
  }
  require(idx == nInputChannels)

  (arbs zip io.credit_alloc).map { case (a,i) =>
    i.valid := a.io.out.fire()
    i.bits := a.io.out.bits
  }
}
