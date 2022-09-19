package constellation.routing

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.{Parameters}

import constellation.noc.{HasNoCParams}
import constellation.channel.{Flit}

/** A representation for 1 specific virtual channel in wormhole routing
 *
 * @param src the source node
 * @param vc ID for the virtual channel
 * @param dst the destination node
 * @param n_vc the number of virtual channels
  */
// BEGIN: ChannelRoutingInfo
case class ChannelRoutingInfo(
  src: Int,
  dst: Int,
  vc: Int,
  n_vc: Int
) {
  // END: ChannelRoutingInfo
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
// BEGIN: FlowRoutingInfo
case class FlowRoutingInfo(
  ingressId: Int,
  egressId: Int,
  vNetId: Int,
  ingressNode: Int,
  ingressNodeId: Int,
  egressNode: Int,
  egressNodeId: Int
) {
// END: FlowRoutingInfo
  def isFlow(f: FlowRoutingBundle): Bool = {
    (f.ingress_node === ingressNode.U &&
      f.egress_node === egressNode.U &&
      f.ingress_node_id === ingressNodeId.U &&
      f.egress_node_id === egressNodeId.U)
  }
}

class FlowRoutingBundle(implicit val p: Parameters) extends Bundle with HasNoCParams {
  // Instead of tracking ingress/egress ID, track the physical destination id and the offset at the destination
  // This simplifies the routing tables
  val vnet_id = UInt(log2Ceil(nVirtualNetworks).W)
  val ingress_node = UInt(log2Ceil(nNodes).W)
  val ingress_node_id = UInt(log2Ceil(maxIngressesAtNode).W)
  val egress_node = UInt(log2Ceil(nNodes).W)
  val egress_node_id = UInt(log2Ceil(maxEgressesAtNode).W)
}

