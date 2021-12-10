package constellation

package object routing {

  case class ChannelInfoForRouting(
    src: Int, vc: Int, dst: Int
  )

  case class PacketInfoForRouting(
    dst: Int, vNet: Int
  )

  case class AllocParams(
    srcC: ChannelInfoForRouting, nxtC: ChannelInfoForRouting, pInfo: PacketInfoForRouting
  ) {
    require (srcC.dst == nxtC.src)
  }

  type NodeRoutingRelation = AllocParams => Boolean


  class RoutingRelation(f: (Int, AllocParams) => Boolean, val isEscape: AllocParams => Boolean = _ => true) {
    def apply(nodeId: Int): NodeRoutingRelation = (p: AllocParams) => {
      require(nodeId == p.srcC.dst && nodeId == p.nxtC.src)
      f(nodeId, p)
    }

    def unary_!() = new RoutingRelation((n, p) => !f(n, p), isEscape)
    def ||(a2: RoutingRelation) = new RoutingRelation((n, p) => f(n, p) || a2(n)(p), p => isEscape(p) || a2.isEscape(p))
    def ||(a2: Boolean)          = new RoutingRelation((n, p) => f(n, p) || a2      , isEscape)
    def &&(a2: RoutingRelation) = new RoutingRelation((n, p) => f(n, p) && a2(n)(p), p => isEscape(p) || a2.isEscape(p))
    def &&(a2: Boolean)          = new RoutingRelation((n, p) => f(n, p) && a2      , isEscape)
  }
}


