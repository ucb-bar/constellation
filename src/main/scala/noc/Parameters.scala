package constellation.noc

import chisel3._
import chisel3.util._


import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, BundleBridgeSink, InModuleBody}
import constellation.router._
import constellation.channel._
import constellation.routing.{RoutingRelation, PacketRoutingInfo, ChannelRoutingInfo}
import constellation.topology.{PhysicalTopology, UnidirectionalLine}


case class NoCParams(
  nVirtualNetworks: Int = 1,

  topology: PhysicalTopology = new UnidirectionalLine(1),
  channelParamGen: (Int, Int) => UserChannelParams = (a: Int, b: Int) => UserChannelParams(),
  ingresses: Seq[UserIngressParams] = Nil,
  egresses: Seq[UserEgressParams] = Nil,
  routingRelation: RoutingRelation = RoutingRelation.allLegal,
  routerParams: Int => UserRouterParams = (i: Int) => UserRouterParams(),
  // (blocker, blockee) => bool
  // If true, then blocker must be able to proceed when blockee is blocked
  vNetBlocking: (Int, Int) => Boolean = (_: Int, _: Int) => true,
  nocName: String = "test",
  skipValidationChecks: Boolean = false
)
case object NoCKey extends Field[NoCParams](NoCParams())

trait HasNoCParams {
  implicit val p: Parameters
  val nocParams = p(NoCKey)

  val nNodes = nocParams.topology.nNodes
  val nVirtualNetworks = nocParams.nVirtualNetworks
  val nocName = nocParams.nocName
  val skipValidationChecks = nocParams.skipValidationChecks

  val nodeIdBits = log2Ceil(nNodes)
  val vNetBits = log2Up(nocParams.nVirtualNetworks)
  val nEgresses = nocParams.egresses.size
  val egressIdBits = log2Up(nocParams.egresses.size)
  val egressSrcIds = nocParams.egresses.map(_.srcId)
}
