package constellation.routing

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.{Parameters}

import constellation._

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
  egressId: Int, vNet: Int
)(implicit val p: Parameters) {
  def dst = p(NoCKey).egresses(egressId).srcId
}

case class PacketRoutingInfoInternal(
  dst: Int,
  vNet: Int
)

class PacketRoutingBundle(implicit val p: Parameters) extends Bundle with HasNoCParams{
  val egress = UInt(egressIdBits.W)
  val vnet = UInt(vNetBits.W)
  def dst(possible: Set[PacketRoutingInfo]) = MuxLookup(egress, 0.U(nodeIdBits.W),
    possible.toSeq.map(u => u.egressId.U -> egressSrcIds(u.egressId).U)
  )(nodeIdBits-1,0)
}

