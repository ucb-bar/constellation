package constellation.router

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._
import freechips.rocketchip.rocket.{DecodeLogic}

import constellation.channel._
import constellation.noc.{HasNoCParams}

class VCAllocReqPerInputVC(
  val outParams: Seq[ChannelParams],
  val egressParams: Seq[EgressChannelParams])
  (implicit val p: Parameters) extends Bundle with HasRouterOutputParams with HasNoCParams {
  val ingress_id = UInt(ingressIdBits.W)
  val egress_id = UInt(egressIdBits.W)
  val vc_sel = MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool()) })
}


class VCAllocReq(
  val inParams: Seq[ChannelParams],
  val ingressParams: Seq[IngressChannelParams],
  val outParams: Seq[ChannelParams],
  val egressParams: Seq[EgressChannelParams])
  (implicit val p: Parameters) extends Bundle with HasRouterOutputParams with HasRouterInputParams with HasNoCParams {
  val ingress_id = UInt(ingressIdBits.W)
  val egress_id = UInt(egressIdBits.W)
  val in_id = UInt(log2Ceil(allInParams.size).W)
  val in_virt_channel = UInt(log2Ceil(allInParams.map(_.nVirtualChannels).max).W)
  val vc_sel = MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool()) })
}

class VCAllocResp(val cParam: BaseChannelParams, val outParams: Seq[ChannelParams], val egressParams: Seq[EgressChannelParams])(implicit val p: Parameters) extends Bundle with HasChannelParams with HasRouterOutputParams {
  val in_virt_channel = UInt(virtualChannelBits.W)
  val vc_sel = MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool()) })
}

case class VCAllocatorParams(
  routerParams: RouterParams,
  inParams: Seq[ChannelParams],
  outParams: Seq[ChannelParams],
  ingressParams: Seq[IngressChannelParams],
  egressParams: Seq[EgressChannelParams])

abstract class VCAllocator(val vP: VCAllocatorParams)(implicit val p: Parameters) extends Module
    with HasRouterParams {

  val routerParams = vP.routerParams
  val inParams = vP.inParams
  val outParams = vP.outParams
  val ingressParams = vP.ingressParams
  val egressParams = vP.egressParams

  val io = IO(new Bundle {
    val req = MixedVec(allInParams.map { u =>
      Flipped(Vec(u.nVirtualChannels, Decoupled(new VCAllocReqPerInputVC(outParams, egressParams))))
    })
    val resp = MixedVec(allInParams.map { u =>
      Valid(new VCAllocResp(u, outParams, egressParams)) })

    val channel_status = MixedVec(allOutParams.map { u =>
      Vec(u.nVirtualChannels, Input(new OutputChannelStatus)) })
    val out_allocs = MixedVec(allOutParams.map { u =>
      Vec(u.nVirtualChannels, Output(new OutputChannelAlloc)) })
  })
  val nOutChannels = allOutParams.map(_.nVirtualChannels).sum

  // Remaps the channels/virt channels such that lower-indexed virt channels are
  // first. Kludgy solution
  def getIdx(outChannel: Int, outVirtChannel: Int): Int = {
    require(outChannel < nAllOutputs &&
      outVirtChannel < allOutParams(outChannel).nVirtualChannels)
    if (outChannel < nOutputs) {
      (0 until outVirtChannel).map(v =>
        outParams.count(_.nVirtualChannels > v)
      ).sum + outParams.take(outChannel).count(_.nVirtualChannels > outVirtChannel)
    } else {
      require(outVirtChannel == 0)
      outParams.map(_.nVirtualChannels).sum + outChannel - nOutputs
    }
  }
  def getOutChannelInfo(idx: Int): (Int, Int) = {
    for (outId <- 0 until nAllOutputs)
      for (outVirtId <- 0 until allOutParams(outId).nVirtualChannels)
        if (getIdx(outId, outVirtId) == idx) return (outId, outVirtId)
    require(false)
    return (-1, -1)
  }
  def getOutChannelInfo(idx: UInt): (UInt, UInt) = {
    val outId = MuxLookup(idx, 0.U(1.W), (0 until nOutChannels).map(i =>
      i.U -> getOutChannelInfo(i)._1.U
    ))
    val outVirtId = MuxLookup(idx, 0.U(1.W), (0 until nOutChannels).map(i =>
      i.U -> getOutChannelInfo(i)._2.U
    ))
    (outId, outVirtId)
  }

}
