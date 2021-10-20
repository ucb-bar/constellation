package constellation

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}

import constellation.topology._

case class NoCConfig(
  nNodes: Int = 3,
  flitPayloadBits: Int = 64,
  maxFlits: Int = 8,
  nPrios: Int = 1,

  // srcNodeId, destNodeId => virtualChannelParams
  topology: (Int, Int) => Option[ChannelParams] = (a: Int, b: Int) => None,
  channelAllocPolicy: ChannelAllocPolicy = ChannelAllocPolicies.allLegal,
  routingFunctions: RoutingFunction = RoutingAlgorithms.crazy,
  // Seq[nodeId]
  inputNodes: Seq[Int] = Nil,
  // Seq[nodeId]
  outputNodes: Seq[Int] = Nil
)

case object NoCKey extends Field[NoCConfig](NoCConfig())

trait HasNoCParams {
  implicit val p: Parameters
  val params = p(NoCKey)

  val nNodes = params.nNodes
  val flitPayloadBits = params.flitPayloadBits
  val nodeIdBits = log2Ceil(params.nNodes)
  val maxFlits = params.maxFlits
  val flitIdBits = log2Up(params.maxFlits+1)
  val nPrios = params.nPrios
  require (nPrios >= 1)
  val prioBits = log2Up(params.nPrios)
  // val virtualChannelBits = log2Up(Seq.tabulate(nNodes, nNodes) {
  //   case (i,j) => params.topology(i, j).size }.flatten.max)

  val topologyFunction = params.topology
  val channelAllocPolicy = params.channelAllocPolicy
  val routingFunctions = params.routingFunctions

  val inputNodes = params.inputNodes
  val outputNodes = params.outputNodes
  val outputIdBits = log2Up(outputNodes.size)
  val outChannelIdBits = log2Up((0 until nNodes).map { i => outputNodes.count(_ == i) }.max)

  def outIdToDestId(outId: UInt): UInt = VecInit(outputNodes.map(_.U))(outId)
  def outIdToDestChannelId(outId: UInt): UInt = {
    VecInit(outputNodes.zipWithIndex.map { case (e,i) => outputNodes.take(i).count(_ == e).U })(outId)
  }

}

