package constellation.channel

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import constellation.routing.{FlowRoutingBundle}
import constellation.noc.{HasNoCParams}

class IngressFlit(val cParam: BaseChannelParams)(implicit val p: Parameters) extends Bundle with HasChannelParams with HasNoCParams {
  val head = Bool()
  val tail = Bool()
  val egress_id = UInt(egressIdBits.W)
  val payload = UInt(payloadBits.W)
}


class EgressFlit(val cParam: BaseChannelParams)(implicit val p: Parameters) extends Bundle with HasChannelParams with HasNoCParams {
  val head = Bool()
  val tail = Bool()
  val ingress_id = UInt(ingressIdBits.W)
  val payload = UInt(payloadBits.W)
}

class Flit(val cParam: BaseChannelParams)(implicit val p: Parameters) extends Bundle with HasChannelParams with HasNoCParams {
  val head = Bool()
  val tail = Bool()
  val flow = new FlowRoutingBundle
  val payload = UInt(payloadBits.W)
  val virt_channel_id = UInt(virtualChannelBits.W)
}

class PayloadFlit(val cParam: BaseChannelParams)(implicit val p: Parameters) extends Bundle with HasChannelParams with HasNoCParams {
  val head = Bool()
  val tail = Bool()
  val payload = UInt(payloadBits.W)
}
