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

class ChannelRoutingBundle extends Bundle {
  val src = UInt()
  val vc = UInt()
  val dst = UInt()
}

/** Represents the properties of a packet that are relevant for routing
 *
 * @param egressId packet's destination egress point
 * @param vNet virtual subnetwork identifier
 */
case class PacketRoutingInfo(
  ingressId: Int, egressId: Int, vNet: Int, dst: Int
) {
  def isFlit(f: Flit) = {
    f.ingress_id === ingressId.U && f.egress_id === egressId.U && f.vnet_id === vNet.U
  }
}

class FlowIdentifierBundle(implicit val p: Parameters) extends Bundle with HasNoCParams{
  val ingress = UInt(ingressIdBits.W)
  val egress = UInt(egressIdBits.W)
  val vnet = UInt(vNetBits.W)
}

