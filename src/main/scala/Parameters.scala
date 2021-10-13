package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}

case class AstroNoCConfig(
  nNodes: Int = 3,
  flitPayloadBits: Int = 64,
  maxFlits: Int = 8,
  nPrios: Int = 2,

  virtChannelBits: Int = 2,
  // srcNodeId, destNodeId => virtualChannelParams
  topology: (Int, Int) => Seq[VirtualChannelParams] = (a: Int, b: Int) => Nil,
  // nodeId => (srcNodeId, inVChannelId, destNodeId, outVChannelId) => prio => legalPath
  virtualLegalPaths: Int => (Int, Int, Int, Int) => Int => Boolean = (a: Int) => (b: Int, c: Int, d: Int, e: Int) => (f: Int) => false,
  // nodeId => destId, nextId => prio => usePath
  routingFunctions: Int => (Int, Int) => (Int) => Boolean = (a: Int) => (b: Int, c: Int) => (c: Int) => false,
  // Seq[(nVChannels, nodeId)]
  inputNodes: Seq[(Int, Int)] = Nil,
  // Seq[nodeId]
  outputNodes: Seq[Int] = Nil
)

case object AstroNoCKey extends Field[AstroNoCConfig](AstroNoCConfig())

trait HasAstroNoCParams {
  implicit val p: Parameters
  val params = p(AstroNoCKey)

  val nNodes = params.nNodes
  val flitPayloadBits = params.flitPayloadBits
  val idBits = log2Ceil(params.nNodes)
  val maxFlits = params.maxFlits
  val flitIdBits = log2Up(params.maxFlits+1)
  val nPrios = params.nPrios
  require (nPrios >= 1)
  val prioBits = log2Up(params.nPrios)
  val virtChannelBits = params.virtChannelBits

  val topologyFunction = params.topology
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


case class VirtualChannelParams(
  bufferSize: Int
)

case class ChannelParams(
  srcId: Int,
  destId: Int,
  virtualChannelParams: Seq[VirtualChannelParams],
  inputId: Int = -1,
  outputId: Int = -1
) {
  val nVirtualChannels = virtualChannelParams.size
  val isInput = inputId >= 0
  val isOutput = outputId >= 0
  require(!(srcId == -1 ^ isInput))
  require(!(destId == -1 ^ isOutput))
  require(!(isInput && isOutput))
}

