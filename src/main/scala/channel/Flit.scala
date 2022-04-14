package constellation.channel

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import constellation.routing.{FlowIdentifierBundle}
import constellation.noc.{HasNoCParams}

class IngressFlit(val cParam: BaseChannelParams)(implicit val p: Parameters) extends Bundle with HasChannelParams with HasNoCParams {
  val head = Bool()
  val tail = Bool()
  val egress_id = UInt(egressIdBits.W)
  val fifo_id = UInt(fifoIdBits.W)
  val payload = UInt(payloadBits.W)
}


class EgressFlit(val cParam: BaseChannelParams)(implicit val p: Parameters) extends Bundle with HasChannelParams with HasNoCParams {
  val head = Bool()
  val tail = Bool()
  val ingress_id = UInt(egressIdBits.W)
  val fifo_id = UInt(fifoIdBits.W)
  val payload = UInt(payloadBits.W)
}

class Flit(val cParam: BaseChannelParams)(implicit val p: Parameters) extends Bundle with HasChannelParams with HasNoCParams {
  val head = Bool()
  val tail = Bool()
  val flow = new FlowIdentifierBundle
  val fifo_id = UInt(fifoIdBits.W)
  val payload = UInt(payloadBits.W)
  val virt_channel_id = UInt(virtualChannelBits.W)
}
