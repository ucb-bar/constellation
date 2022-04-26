package constellation.router

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._
import freechips.rocketchip.rocket.{DecodeLogic}

import constellation.channel._
import constellation.noc.{HasNoCParams}
import constellation.routing.{FlowRoutingBundle, FlowRoutingInfo, ChannelRoutingInfo}

class VCAllocReq(
  val outParams: Seq[ChannelParams],
  val egressParams: Seq[EgressChannelParams])
  (implicit val p: Parameters) extends Bundle
    with HasRouterOutputParams
    with HasNoCParams {
  val flow = new FlowRoutingBundle
  val vc_sel = MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool()) })
}

class VCAllocResp(val cParam: BaseChannelParams, val outParams: Seq[ChannelParams], val egressParams: Seq[EgressChannelParams])(implicit val p: Parameters) extends Bundle
    with HasChannelParams
    with HasRouterOutputParams {
  val vc_sel = MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool()) })
}

case class VCAllocatorParams(
  routerParams: RouterParams,
  inParams: Seq[ChannelParams],
  outParams: Seq[ChannelParams],
  ingressParams: Seq[IngressChannelParams],
  egressParams: Seq[EgressChannelParams])

abstract class VCAllocator(val vP: VCAllocatorParams)(implicit val p: Parameters) extends Module
    with HasRouterParams
    with HasRouterInputParams
    with HasRouterOutputParams
    with HasNoCParams {

  val routerParams = vP.routerParams
  val inParams = vP.inParams
  val outParams = vP.outParams
  val ingressParams = vP.ingressParams
  val egressParams = vP.egressParams

  val io = IO(new Bundle {
    val req = MixedVec(allInParams.map { u =>
      Flipped(Vec(u.nVirtualChannels, Decoupled(new VCAllocReq(outParams, egressParams))))
    })
    val resp = MixedVec(allInParams.map { u =>
      Vec(u.nVirtualChannels, Valid(new VCAllocResp(u, outParams, egressParams)))
    })

    val channel_status = MixedVec(allOutParams.map { u =>
      Vec(u.nVirtualChannels, Input(new OutputChannelStatus)) })
    val out_allocs = MixedVec(allOutParams.map { u =>
      Vec(u.nVirtualChannels, Output(new OutputChannelAlloc)) })
  })
  val nOutChannels = allOutParams.map(_.nVirtualChannels).sum

  def inputAllocPolicy(req: VCAllocReq, srcSel: MixedVec[Vec[Bool]], fire: Bool): MixedVec[Vec[Bool]]
  def outputAllocPolicy(out: ChannelRoutingInfo,
    flows: Seq[Seq[FlowRoutingBundle]], reqs: Seq[Seq[Bool]], fire: Bool): MixedVec[Vec[Bool]]

}
