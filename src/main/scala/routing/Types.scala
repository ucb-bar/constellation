package constellation.routing

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.{Parameters}

import constellation.noc.{NoCKey, HasNoCParams}
import constellation.channel.{Flit}

/** A representation for 1 specific virtual channel in wormhole routing
 *
 * @param src the source node
 * @param vc ID for the virtual channel
 * @param dst the destination node
 * @param n_vc the number of virtual channels
 */
case class ChannelRoutingInfo(
  src: Int, vc: Int, dst: Int, n_vc: Int
) {
  require (src >= -1 && dst >= -1 && vc >= 0, s"Illegal $this")
  require (!(src == -1 && dst == -1), s"Illegal $this")
  require (vc < n_vc, s"Illegal $this")
  val isIngress = src == -1
  val isEgress = dst == -1
}

/** Represents the properties of a packet that are relevant for routing
  * ingressId and egressId uniquely identify a flow, but vnet and dst are used here
  * to simplify the implementation of routingrelations
  *
  * @param ingressId packet's source ingress point
  * @param egressId packet's destination egress point
  * @param vNet virtual subnetwork identifier
  * @param dst packet's destination node ID
  */
case class FlowRoutingInfo(
  ingressId: Int, egressId: Int, vNet: Int, dst: Int
) {
  def isFlow(f: FlowRoutingBundle) = {
    f.ingress_id === ingressId.U && f.egress_id === egressId.U
  }
}

class FlowRoutingBundle(implicit val p: Parameters) extends Bundle with HasNoCParams {
  val ingress_id = UInt(ingressIdBits.W)
  val egress_id = UInt(egressIdBits.W)
  // egress_dst_id is the physical node of the egress
  // it can be computed from egress_id, but to improve design of route decoders
  // it is also computed at ingress and sent along with the packet
  val egress_dst_id = UInt(log2Ceil(nNodes).W)
}

