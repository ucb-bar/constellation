package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}

case class AstroNoCConfig(
  nNodes: Int = 3,
  flitPayloadBits: Int = 64,
  maxFlits: Int = 8,
  prioBits: Int = 3,

  virtChannelBits: Int = 2,
  topology: (Int, Int) => Option[ChannelParams] = (a: Int, b: Int) => None,
  virtualLegalPaths: Int => (Int, Int, Int, Int) => UInt => Bool = (a: Int) => (b: Int, c: Int, d: Int, e: Int) => (f: UInt) => false.B
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

  val topologyFunction = params.topology
  val virtualLegalPathsFunction = params.virtualLegalPaths
}


case class VirtualChannelParams(
  bufferSize: Int
)

case class ChannelParams(
  virtualChannelParams: Seq[VirtualChannelParams]
)

