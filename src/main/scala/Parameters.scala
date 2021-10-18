package constellation

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}

case class NoCConfig(
  nNodes: Int = 3,
  flitPayloadBits: Int = 64,
  maxFlits: Int = 8,
  nPrios: Int = 2,

  // srcNodeId, destNodeId => virtualChannelParams
  topology: (Int, Int) => Seq[VirtualChannelParams] = (a: Int, b: Int) => Nil,
  // srcNodeId, destNodeId => depth
  channelDepths: (Int, Int) => Int = (a: Int, b: Int) => 0,
  // nodeId => (srcNodeId, inVChannelId, destNodeId, outVChannelId) => prio => legalPath
  virtualLegalPaths: Int => (Int, Int, Int, Int) => Int => Boolean = (a: Int) => (b: Int, c: Int, d: Int, e: Int) => (f: Int) => false,
  // nodeId => lastId, destId, nextId => prio => usePath
  routingFunctions: Int => (Int, Int, Int) => (Int) => Boolean = _ => (_, _, _) => _ => false,
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
  val channelDepths = params.channelDepths
  val virtualLegalPathsFunction = params.virtualLegalPaths
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

