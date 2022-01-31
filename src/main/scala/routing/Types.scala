package constellation.routing

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.{Parameters}

import constellation._

case class ChannelRoutingInfo(
  src: Int, vc: Int, dst: Int
) {
  require (src >= -1 && dst >= -1 && vc >= 0)
  require (!(src == -1 && dst == -1))
  val isIngress = src == -1
  val isEgress = dst == -1
}

class ChannelRoutingBundle extends Bundle {
  val src = UInt()
  val vc = UInt()
  val dst = UInt()
}

case class PacketRoutingInfo(
  egressId: Int, vNet: Int
)(implicit val p: Parameters) {
  def dst = p(NoCKey).egresses(egressId).srcId
}

case class PacketRoutingInfoInternal(
  dst: Int,
  vNet: Int
)

class PacketRoutingBundle(implicit p: Parameters) extends Bundle {
  val egress_id = UInt()
  val vnet = UInt()
  def dst = VecInit(p(NoCKey).egresses.map(_.srcId.U))(egress_id)
}

class RoutingRelation(
  f: (Int, ChannelRoutingInfo, ChannelRoutingInfo, PacketRoutingInfoInternal) => Boolean,
  val isEscape: (ChannelRoutingInfo, Int) => Boolean = (_,_) => true) {

  def apply(nodeId: Int, srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, pInfo: PacketRoutingInfo): Boolean = {
    apply(nodeId, srcC, nxtC, PacketRoutingInfoInternal(pInfo.dst, pInfo.vNet))
  }
  def apply(nodeId: Int, srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, pInfo: PacketRoutingInfoInternal): Boolean = {
    require(nodeId == srcC.dst && nodeId == nxtC.src)
    f(nodeId, srcC, nxtC, pInfo)
  }

  def unary_!()               = new RoutingRelation(
    (n, srcC, nxtC, pInfo) => !f(n, srcC, nxtC, pInfo),
    isEscape
  )
  def ||(a2: RoutingRelation) = new RoutingRelation(
    (n, srcC, nxtC, pInfo) => f(n, srcC, nxtC, pInfo) || a2(n, srcC, nxtC, pInfo),
    (c, v) => isEscape(c, v) || a2.isEscape(c, v)
  )
  def ||(a2: Boolean)         = new RoutingRelation(
    (n, srcC, nxtC, pInfo) => f(n, srcC, nxtC, pInfo) || a2,
    isEscape
  )
  def &&(a2: RoutingRelation) = new RoutingRelation(
    (n, srcC, nxtC, pInfo) => f(n, srcC, nxtC, pInfo) && a2(n, srcC, nxtC, pInfo),
    (c, v) => isEscape(c, v) || a2.isEscape(c, v)
  )
  def &&(a2: Boolean)         = new RoutingRelation(
    (n, srcC, nxtC, pInfo) => f(n, srcC, nxtC, pInfo) && a2,
    isEscape
  )
}
