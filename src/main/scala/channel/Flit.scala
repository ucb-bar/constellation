package constellation.channel

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import constellation.routing.{PacketRoutingBundle}


class IOFlit(val cParam: BaseChannelParams)(implicit val p: Parameters) extends Bundle with HasChannelParams {
  val head = Bool()
  val tail = Bool()
  val egress_id = UInt(egressIdBits.W)
  val payload = UInt(payloadBits.W)
}

class Flit(cParam: BaseChannelParams)(implicit p: Parameters) extends IOFlit(cParam)(p) {
  val vnet_id = UInt(vNetBits.W)
  val virt_channel_id = UInt(virtualChannelBits.W)
  def route_info = {
    val info = Wire(new PacketRoutingBundle)
    info.egress := egress_id
    info.vnet := vnet_id
    info
  }
}
