package constellation

package object routing {

  case class ChannelInfoForRouting(
    src: Int, vc: Int, dst: Int
  ) {
    require (src >= -1 && dst >= -1 && vc >= 0)
    require (!(src == -1 && dst == -1))
    val isIngress = src == -1
    val isEgress = dst == -1
  }

  case class PacketInfoForRouting(
    dst: Int, vNet: Int
  )

  type NodeRoutingRelation = (ChannelInfoForRouting, ChannelInfoForRouting, PacketInfoForRouting) => Boolean


  class RoutingRelation(
    f: (Int, ChannelInfoForRouting, ChannelInfoForRouting, PacketInfoForRouting) => Boolean,
    val isEscape: (ChannelInfoForRouting, Int) => Boolean = (_,_) => true) {

    def apply(nodeId: Int): NodeRoutingRelation = (srcC: ChannelInfoForRouting, nxtC: ChannelInfoForRouting, pInfo: PacketInfoForRouting) => {
      require(nodeId == srcC.dst && nodeId == nxtC.src)
      f(nodeId, srcC, nxtC, pInfo)
    }

    def unary_!()               = new RoutingRelation(
      (n, srcC, nxtC, pInfo) => !f(n, srcC, nxtC, pInfo),
      isEscape
    )
    def ||(a2: RoutingRelation) = new RoutingRelation(
      (n, srcC, nxtC, pInfo) => f(n, srcC, nxtC, pInfo) || a2(n)(srcC, nxtC, pInfo),
      (c, v) => isEscape(c, v) || a2.isEscape(c, v)
    )
    def ||(a2: Boolean)         = new RoutingRelation(
      (n, srcC, nxtC, pInfo) => f(n, srcC, nxtC, pInfo) || a2,
      isEscape
    )
    def &&(a2: RoutingRelation) = new RoutingRelation(
      (n, srcC, nxtC, pInfo) => f(n, srcC, nxtC, pInfo) && a2(n)(srcC, nxtC, pInfo),
      (c, v) => isEscape(c, v) || a2.isEscape(c, v)
    )
    def &&(a2: Boolean)         = new RoutingRelation(
      (n, srcC, nxtC, pInfo) => f(n, srcC, nxtC, pInfo) && a2,
      isEscape
    )
  }
}


