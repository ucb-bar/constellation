package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}

case class AstroNoCConfig(
  flitPayloadWidth: Int = 64,
  idBits: Int = 3,
  virtChannelBits: Int = 2,
  shareRouteComputer: Boolean = false,
  maxFlits: Int = 8
)

case object AstroNoCKey extends Field[AstroNoCConfig](AstroNoCConfig())

trait HasAstroNoCParams {
  implicit val p: Parameters
  val params = p(AstroNoCKey)

  val flitPayloadWidth = params.flitPayloadWidth
  val idBits = params.idBits
  val virtChannelBits = params.virtChannelBits
  val shareRouteComputer = params.shareRouteComputer
  val maxFlits = params.maxFlits
}




case class ChannelParams(
  virtualChannels: Int,
  bufferSize: Int
)

