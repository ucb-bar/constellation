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
      // excluding channelGen
      "crossingType"          -> ucp.getClass.getName(),
      "srcSpeedup"            -> ucp.srcSpeedup,
      "dstSpeedup"            -> ucp.destSpeedup
    )
  }

  case class UserChannelParamsWrapper(
    srcId: Int,
    destId: Int,
    userChannelParams: UserChannelParams
  )

  implicit val jsonUserChannelParamsWrapper = new Writes[UserChannelParamsWrapper] {
    def writes(ucpw: UserChannelParamsWrapper) = Json.obj(
        "srcId" -> ucpw.srcId,
        "destId" -> ucpw.destId,
        "userChannelParams" -> ucpw.userChannelParams
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

  implicit val jsonUserRouterParams = new Writes[UserRouterParams] {
    def writes(urp: UserRouterParams) = Json.obj(
      "payloadBits"    -> urp.payloadBits,
      "combineSAST"    -> urp.combineSAST,
      "combineRCVA"    -> urp.combineRCVA,
      "coupleSAVA"     -> urp.coupleSAVA
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
    def writes(nocParams: NoCParams) = {
      val nNodes = nocParams.topology.nNodes
      val channelParams = Seq.tabulate(nNodes, nNodes) { case (i,j) =>
        if (nocParams.topology.topo(i, j)) {
          val cP = nocParams.channelParamGen(i, j)
          Some(UserChannelParamsWrapper(
            srcId = i,
            destId = j,
            userChannelParams = cP,
          ))
        } else {
          None
        }
      }.flatten.flatten

      val routerParams = (0 until nNodes).map { i =>
        nocParams.routerParams(i)
      }

      Json.obj(
      "topology"     -> nocParams.topology.getClass().getName(),
      "channelParams" -> channelParams,
      "ingresses"    -> nocParams.ingresses,
      "egresses"     -> nocParams.egresses,
      "routerParams" -> routerParams,
      // vNetBlocking is a function
      "flows"        -> nocParams.flows,
      "routingRelation" -> routingRelation.getClass().getName(),
      "nocName"      -> nocParams.nocName
      )
    }
  }

  def printAsJson(np: NoCParams): JsValue = {
    Json.toJson(np)
  }

}