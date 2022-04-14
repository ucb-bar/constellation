package constellation.channel

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import constellation.routing.{FlowIdentifierBundle}
import constellation.noc.{HasNoCParams}

class IOFlit(val cParam: BaseChannelParams)(implicit val p: Parameters) extends Bundle with HasChannelParams with HasNoCParams {
  val head = Bool()
  val tail = Bool()
  val ingress_id = UInt(ingressIdBits.W)
  val egress_id = UInt(egressIdBits.W)
  val fifo_id = UInt(fifoIdBits.W)
  val payload = UInt(payloadBits.W)
}

class Flit(cParam: BaseChannelParams)(implicit p: Parameters) extends IOFlit(cParam)(p) {
  val vnet_id = UInt(vNetBits.W)
  val virt_channel_id = UInt(virtualChannelBits.W)
  def flow = {
    val flow = Wire(new FlowIdentifierBundle)
    flow.ingress := ingress_id
    flow.egress := egress_id
    flow.vnet := vnet_id
    flow
  }
}
