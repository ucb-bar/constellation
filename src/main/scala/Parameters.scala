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
  // src, dst, vNetId
  terminalConnectivity: (Int, Int, Int) => Boolean = (_: Int, _: Int, _: Int) => true,
  masterAllocTable: MasterAllocTable = MasterAllocTables.allLegal,
  routerParams: Int => RouterParams =
    (i: Int) => RouterParams(i, Nil, Nil, Nil, Nil, (_,_,_,_,_,_) => false, false, false),

  // Seq[nodeId]
  ingressNodes: Seq[Int] = Nil,
  // Seq[nodeId]
  egressNodes: Seq[Int] = Nil
)

case object NoCKey extends Field[NoCConfig](NoCConfig())

trait HasNoCParams {
  implicit val p: Parameters
  val params = p(NoCKey)

  val ingressNodes = params.ingressNodes
  val egressNodes = params.egressNodes

  val nNodes = params.nNodes
  val maxFlits = params.maxFlits
  val nVirtualNetworks = params.nVirtualNetworks

  val flitPayloadBits = params.flitPayloadBits
  val nodeIdBits = log2Ceil(params.nNodes)
  val flitIdBits = log2Up(params.maxFlits+1)
  val vNetBits = log2Up(params.nVirtualNetworks)
  val egressIdBits = log2Up(egressNodes.size)

  val topologyFunction = params.topology
  val masterAllocTable = params.masterAllocTable
  val terminalConnectivity = params.terminalConnectivity
  val routerParams = params.routerParams


  def ingressIdToIngressChannelId(ingressId: Int): Int = {
    val t: Seq[Int] = ingressNodes.zipWithIndex.map { case (e,i) =>
      ingressNodes.take(i).count(_ == e) }
    t(ingressId)
  }

  def egressIdToDestId(egressId: UInt): UInt = VecInit(egressNodes.map(_.U))(egressId)
  def egressIdToEgressChannelId(egressId: UInt): UInt = {
    VecInit(egressNodes.zipWithIndex.map { case (e,i) =>
      egressNodes.take(i).count(_ == e).U })(egressId)
  }

}

