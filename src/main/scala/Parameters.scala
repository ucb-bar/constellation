package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}

case class AstroNoCConfig(
  nNodes: Int = 3,
  flitPayloadBits: Int = 64,
  maxFlits: Int = 8,
  prioBits: Int = 1,

  virtChannelBits: Int = 2,
  // srcNodeId, destNodeId => virtualChannelParams
  topology: (Int, Int) => Seq[VirtualChannelParams] = (a: Int, b: Int) => Nil,
  // nodeId => (srcNodeId, inVChannelId, destNodeId, outVChannelId) => prio => legalPath
  virtualLegalPaths: Int => (Int, Int, Int, Int) => Int => Boolean = (a: Int) => (b: Int, c: Int, d: Int, e: Int) => (f: Int) => false,
  // nodeId => destId, nextId => prio => usePath
  routingFunctions: Int => (Int, Int) => (Int) => Boolean = (a: Int) => (b: Int, c: Int) => (c: Int) => false,
)

case object AstroNoCKey extends Field[AstroNoCConfig](AstroNoCConfig())

trait HasAstroNoCParams {
  implicit val p: Parameters
  val params = p(AstroNoCKey)

  val nNodes = params.nNodes
  val flitPayloadBits = params.flitPayloadBits
  val idBits = log2Ceil(params.nNodes)
  val maxFlits = params.maxFlits
  val prioBits = params.prioBits
  val virtChannelBits = params.virtChannelBits
  val nPrios = (1 << prioBits)

  val topologyFunction = params.topology
  val virtualLegalPathsFunction = params.virtualLegalPaths
  val routingFunctions = params.routingFunctions
}


case class VirtualChannelParams(
  bufferSize: Int
)

case class ChannelParams(
  srcId: Int,
  destId: Int,
  virtualChannelParams: Seq[VirtualChannelParams],
)

