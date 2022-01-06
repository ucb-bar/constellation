package constellation

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import constellation.routing.PacketInfoForRouting

case class PacketInfo(
  egressId: Int,
  vNetId: Int) {
  def asPacketInfoForRouting(implicit p: Parameters): PacketInfoForRouting = {
    val egressSrcIds = p(NoCKey).egresses.map(_.srcId)
    PacketInfoForRouting(egressSrcIds(egressId), vNetId)
  }
}


class IOFlit(val cParam: BaseChannelParams)(implicit val p: Parameters) extends Bundle with HasChannelParams {
  val head = Bool()
  val tail = Bool()
  val egress_id = UInt(egressIdBits.W)
  val payload = UInt(payloadBits.W)
}

class Flit(cParam: BaseChannelParams)(implicit p: Parameters) extends IOFlit(cParam)(p) {
  val vnet_id = UInt(vNetBits.W)
  val virt_channel_id = UInt(virtualChannelBits.W)
}
