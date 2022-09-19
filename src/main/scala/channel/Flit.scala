package constellation.channel

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import constellation.routing.{FlowRoutingBundle}
import constellation.noc.{HasNoCParams}

class BaseFlit(val payloadBits: Int)(implicit val p: Parameters) extends Bundle {
  val head = Bool()
  val tail = Bool()
  val payload = UInt(payloadBits.W)
}

class IngressFlit(payloadBits: Int)(implicit p: Parameters) extends BaseFlit(payloadBits)(p) with HasNoCParams {
  val egress_id = UInt(egressIdBits.W)
}

class EgressFlit(payloadBits: Int)(implicit p: Parameters) extends BaseFlit(payloadBits)(p) with HasNoCParams {
  val ingress_id = UInt(ingressIdBits.W)
}

class Flit(payloadBits: Int)(implicit p: Parameters) extends BaseFlit(payloadBits)(p) with HasNoCParams {
  val flow = new FlowRoutingBundle
  val virt_channel_id = UInt(virtualChannelBits.W)
}
