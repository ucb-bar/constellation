package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

class VCAllocReq(nOutputs: Int)(implicit val p: Parameters) extends Bundle with HasAstroNoCParams {
  val in_virt_channel = UInt(virtChannelBits.W)
  val out_channels = UInt(nOutputs.W)
}

class VCAllocResp(nOutputs: Int)(implicit val p: Parameters) extends Bundle with HasAstroNoCParams {
  val in_virt_channel = UInt(virtChannelBits.W)
  val out_virt_channel = UInt(virtChannelBits.W)
  val out_channel = UInt(log2Ceil(nOutputs).W)
}

class VCAllocator(inParams: Seq[ChannelParams], outParams: Seq[ChannelParams])(implicit val p: Parameters) extends Module with HasAstroNoCParams {
  val io = IO(new Bundle {
    val req = Vec(inParams.size, Flipped(Decoupled(new VCAllocReq(outParams.size))))
    val resp = Vec(inParams.size, Valid(new VCAllocResp(outParams.size)))
    val out_alloc = Vec(outParams.size, Valid(new OutputUnitAlloc(inParams.size)))
  })
}
