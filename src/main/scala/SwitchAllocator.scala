package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._


class SwitchAllocReq(nOutputs: Int)(implicit val p: Parameters) extends Bundle with HasAstroNoCParams {
  val out_channel = UInt(log2Ceil(nOutputs).W)
}

class SwitchAllocator(inParams: Seq[ChannelParams], outParams: Seq[ChannelParams])(implicit val p: Parameters) extends Module with HasAstroNoCParams {
  val io = IO(new Bundle {
    val req = MixedVec(inParams.map(u => Vec(u.virtualChannels, Flipped(Decoupled(new SwitchAllocReq(outParams.size))))))
    val credit_alloc = Vec(outParams.size, Output(Bool()))
  })
}
