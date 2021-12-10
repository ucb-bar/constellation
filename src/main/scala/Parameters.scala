package constellation

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}

import constellation.routing._
import constellation.router._

case class NoCConfig(
  nNodes: Int = 3,
  flitPayloadBits: Int = 64,
  maxFlits: Int = 8,
  nVirtualNetworks: Int = 1,

  // srcNodeId, destNodeId => virtualChannelParams
  topology: (Int, Int) => Option[UserChannelParams] = (a: Int, b: Int) => None,
  ingresses: Seq[UserIngressParams] = Nil,
  egresses: Seq[UserEgressParams] = Nil,
  routingRelation: RoutingRelation = RoutingRelations.allLegal,
  routerParams: Int => UserRouterParams = (i: Int) => UserRouterParams(),
  // blocker, blocked => bool
  vNetBlocking: (Int, Int) => Boolean = (_: Int, _: Int) => false,
)
case object NoCKey extends Field[NoCConfig](NoCConfig())

trait HasNoCParams {
  implicit val p: Parameters
  private val params = p(NoCKey)

  val nNodes = params.nNodes
  val maxFlits = params.maxFlits
  val nVirtualNetworks = params.nVirtualNetworks

  val flitPayloadBits = params.flitPayloadBits
  val nodeIdBits = log2Ceil(params.nNodes)
  val flitIdBits = log2Up(params.maxFlits+1)
  val vNetBits = log2Up(params.nVirtualNetworks)
  val nEgresses = params.egresses.size
  val egressIdBits = log2Up(params.egresses.size)
  val egressSrcIds = params.egresses.map(_.srcId)

  val topologyFunction = params.topology
  val routerParams = params.routerParams
}

