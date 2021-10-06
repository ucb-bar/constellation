package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

class VCAllocReq(val inParam: ChannelParams, val nOutputs: Int)(implicit val p: Parameters) extends Bundle with HasAstroNoCParams {
  val in_virt_channel = UInt(log2Ceil(inParam.virtualChannelParams.size).W)
  val out_channels = UInt(nOutputs.W)
}

class VCAllocResp(val inParam: ChannelParams, val outParams: Seq[ChannelParams])(implicit val p: Parameters) extends Bundle with HasAstroNoCParams {
  val in_virt_channel = UInt(log2Ceil(inParam.virtualChannelParams.size).W)
  val out_virt_channel = UInt(log2Ceil(outParams.map(_.virtualChannelParams.size).max).W)
  val out_channel = UInt(log2Ceil(outParams.size).W)
}

class VCAllocator(inParams: Seq[ChannelParams], outParams: Seq[ChannelParams])(implicit val p: Parameters) extends Module with HasAstroNoCParams {
  val io = IO(new Bundle {
    val req = MixedVec(inParams.map { u => Flipped(Decoupled(new VCAllocReq(u, outParams.size))) })
    val resp = MixedVec(inParams.map { u => Valid(new VCAllocResp(u, outParams)) })
    val out_alloc = MixedVec(outParams.map { u => Valid(new OutputUnitAlloc(inParams ,u)) })
  })
}
