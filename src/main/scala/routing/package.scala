package constellation

package object routing {

  case class ChannelInfoForRouting(
    src: Int, vc: Int, dst: Int
  )

  case class PacketInfoForRouting(
    dst: Int, vNet: Int
  )

  type NodeRoutingRelation = (ChannelInfoForRouting, ChannelInfoForRouting, PacketInfoForRouting) => Boolean


  class RoutingRelation(
    f: (Int, ChannelInfoForRouting, ChannelInfoForRouting, PacketInfoForRouting) => Boolean,
    val isEscape: (ChannelInfoForRouting, PacketInfoForRouting) => Boolean = (_,_) => true) {

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
      (c, p) => isEscape(c, p) || a2.isEscape(c, p)
    )
    def ||(a2: Boolean)         = new RoutingRelation(
      (n, srcC, nxtC, pInfo) => f(n, srcC, nxtC, pInfo) || a2,
      isEscape
    )
    def &&(a2: RoutingRelation) = new RoutingRelation(
      (n, srcC, nxtC, pInfo) => f(n, srcC, nxtC, pInfo) && a2(n)(srcC, nxtC, pInfo),
      (c, p) => isEscape(c, p) || a2.isEscape(c, p)
    )
    def &&(a2: Boolean)         = new RoutingRelation(
      (n, srcC, nxtC, pInfo) => f(n, srcC, nxtC, pInfo) && a2,
      isEscape
    )
  }
}


