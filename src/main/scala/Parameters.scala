package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}

case class AstroNoCConfig(
  flitPayloadBits: Int = 64,
  idBits: Int = 3,
  virtChannelBits: Int = 2,
  shareRouteComputer: Boolean = false,
  maxFlits: Int = 8,
  prioBits: Int = 3
)

case object AstroNoCKey extends Field[AstroNoCConfig](AstroNoCConfig())

trait HasAstroNoCParams {
  implicit val p: Parameters
  val params = p(AstroNoCKey)

  val flitPayloadBits = params.flitPayloadBits
  val idBits = params.idBits
  val virtChannelBits = params.virtChannelBits
  val shareRouteComputer = params.shareRouteComputer
  val maxFlits = params.maxFlits
  val prioBits = params.prioBits
}


case class VirtualChannelParams(
  bufferSize: Int
)

case class ChannelParams(
  virtualChannelParams: Seq[VirtualChannelParams]
//  virtualChannels: Int,
//  bufferSize: Int
)

