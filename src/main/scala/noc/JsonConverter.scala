package constellation.noc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, BundleBridgeSink, InModuleBody}
import constellation.router._
import constellation.channel._
import constellation.routing._
import constellation.topology.{PhysicalTopology, UnidirectionalLine}

import play.api.libs.json._

object JSONConverters {
  implicit val jsonUserVirtualChannelParams = new Writes[UserVirtualChannelParams] {
    def writes(uvcp: UserVirtualChannelParams) = Json.obj(
      "bufferSize"      -> uvcp.bufferSize
    )
  }

  implicit val jsonUserChannelParams = new Writes[UserChannelParams] {
    def writes(ucp: UserChannelParams) = Json.obj(
      "virtualChannelParams"  -> ucp.virtualChannelParams,
      "srcSpeedup"            -> ucp.srcSpeedup,
      "dstSpeedup"            -> ucp.destSpeedup
    )
  }

  implicit val jsonVirtualChannelParams = new Writes[VirtualChannelParams] {
    def writes(vcp: VirtualChannelParams) = Json.obj(
      "src"         -> vcp.src,
      "dst"         -> vcp.dst,
      "vc"          -> vcp.vc,
      "bufferSize"  -> vcp.bufferSize
    )
  }

  implicit val jsonChannelParams = new Writes[ChannelParams] {
    def writes(cp: ChannelParams) = Json.obj(
      "srcId"                 -> cp.srcId,
      "destId"                -> cp.destId,
      "payloadBits"           -> cp.payloadBits,
      "virtualChannelParams"  -> Json.arr(cp.virtualChannelParams),
      "srcSpeedup"            -> cp.srcSpeedup,
      "destSpeeup"            -> cp.destSpeedup
    )
  }

  implicit val jsonUserIngressParams = new Writes[UserIngressParams] {
    def writes(uip: UserIngressParams) = Json.obj(
      "destId"       -> uip.destId,
      "payloadBits"  -> uip.payloadBits
    )
  }

  implicit val jsonUserEgressParams = new Writes[UserEgressParams] {
    def writes(uep: UserEgressParams) = Json.obj(
      "srcId"       -> uep.srcId,
      "payloadBits"  -> uep.payloadBits
    )
  }

  implicit val jsonFlowParams = new Writes[FlowParams] {
    def writes(fp: FlowParams) = Json.obj(
      "ingressId"    -> fp.ingressId,
      "egressId"     -> fp.egressId,
      "vNetId"       -> fp.vNetId
    )
  }

  implicit val jsonNoCParams = new Writes[NoCParams] {
    def writes(np: NoCParams) = Json.obj(
      "topology"     -> np.topology.getClass().getName(),
      // channelParamGen is a function
      "ingresses"    -> np.ingresses,
      "egresses"     -> np.egresses,
      // routerParams is a function
      // vNetBlocking is a function
      "flows"        -> np.flows,
      // routing relation omitted
      "nocName"      -> np.nocName
    )
  }

  implicit val jsonUserRouterParams = new Writes[UserRouterParams] {
    def writes(urp: UserRouterParams) = Json.obj(
      "payloadBits"    -> urp.payloadBits,
      "combineSAST"    -> urp.combineSAST,
      "combineRCVA"    -> urp.combineRCVA,
      "coupleSAVA"     -> urp.coupleSAVA
    )
  }

  implicit val jsonIngressChannelParams = new Writes[IngressChannelParams] {
    def writes(icp: IngressChannelParams) = Json.obj(
      "ingressId"   -> icp.ingressId,
      "destId"      -> icp.destId,
      // possibleFlows omitted
      "vNetId"      -> icp.vNetId,
      "payloadBits" -> icp.payloadBits

    )
  }

  implicit val jsonEgressChannelParams = new Writes[EgressChannelParams] {
    def writes(ecp: EgressChannelParams) = Json.obj(
      "egressId"   -> ecp.egressId,
      "srcId"      -> ecp.srcId,
      // possibleFlows omitted
      "payloadBits" -> ecp.payloadBits
    )
  }

  implicit val routerParams = new Writes[RouterParams] {
    def writes(rp: RouterParams) = Json.obj(
      "nodeId"   -> rp.nodeId,
      "nIngress" -> rp.nIngress,
      "nEgress"  -> rp.nEgress,
      "user"     -> rp.user
    )
  }

  implicit val jsonInternalNoCParams = new Writes[InternalNoCParams] {
    def writes(inp: InternalNoCParams) = Json.obj(
      "userParams"       -> inp.userParams,
      "nVirtualNetworks" -> inp.nVirtualNetworks,
      // routing relation omitted
      "channelParams"    -> inp.channelParams,
      "ingressParams"    -> inp.ingressParams,
      "egressParams"     -> inp.egressParams,
      "routerParams"     -> inp.routerParams
    )
  }

  def printAsJson(inp: InternalNoCParams): JsValue = {
    Json.toJson(inp)
  }

  def printAsJson(np: NoCParams): JsValue = {
    Json.toJson(np)
  }

}