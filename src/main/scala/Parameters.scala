package constellation

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}

import constellation.topology._
import constellation.router._

case class NoCConfig(
  nNodes: Int = 3,
  flitPayloadBits: Int = 64,
  maxFlits: Int = 8,
  nVirtualNetworks: Int = 1,

  // srcNodeId, destNodeId => virtualChannelParams
  topology: (Int, Int) => Option[ChannelParams] = (a: Int, b: Int) => None,
  ingresses: Seq[IngressChannelParams] = Nil,
  egresses: Seq[EgressChannelParams] = Nil,
  masterAllocTable: MasterAllocTable = MasterAllocTables.allLegal,
  routerParams: Int => RouterParams =
    (i: Int) => RouterParams(i, Nil, Nil, Nil, Nil, (_,_,_,_,_,_) => false, false, false),
  // blocker, blocked => bool
  vNetBlocking: (Int, Int) => Boolean = (_: Int, _: Int) => false,
)
case object NoCKey extends Field[NoCConfig](NoCConfig())

trait HasNoCParams {
  implicit val p: Parameters
  val params = p(NoCKey)

  val globalIngressParams = params.ingresses.zipWithIndex.map { case (u,i) => u.copy(ingressId=i) }
  val globalEgressParams = params.egresses.zipWithIndex.map { case (u,e) => u.copy(egressId=e,
    possiblePackets=globalIngressParams.map { i =>
      (i.possibleEgresses.contains(e), PacketRoutingInfo(e, i.vNetId))
    }.filter(_._1).map(_._2).toSet
  ) }

  globalIngressParams.foreach(_.possibleEgresses.foreach(e => require(e < globalEgressParams.size)))

  val nNodes = params.nNodes
  val maxFlits = params.maxFlits
  val nVirtualNetworks = params.nVirtualNetworks

  val flitPayloadBits = params.flitPayloadBits
  val nodeIdBits = log2Ceil(params.nNodes)
  val flitIdBits = log2Up(params.maxFlits+1)
  val vNetBits = log2Up(params.nVirtualNetworks)
  val egressIdBits = log2Up(globalEgressParams.size)

  val topologyFunction = params.topology
  val masterAllocTable = params.masterAllocTable
  val routerParams = params.routerParams
}

