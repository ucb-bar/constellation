package constellation

package object topology {
  // srcNodeId, destNodeId => Bool
  type PhysicalTopology = (Int, Int) => Boolean


  case class ChannelInfo(
    src: Int, vc: Int, dst: Int
  )

  case class PacketInfo(
    dst: Int, vNet: Int
  )

  case class AllocParams(
    srcC: ChannelInfo, nxtC: ChannelInfo, pInfo: PacketInfo
  ) {
    require (srcC.dst == nxtC.src)
  }

  type NodeAllocTable = AllocParams => Boolean


  class MasterAllocTable(f: (Int, AllocParams) => Boolean, val isEscape: AllocParams => Boolean = _ => true) {
    def apply(nodeId: Int): NodeAllocTable = (p: AllocParams) => {
      require(nodeId == p.srcC.dst && nodeId == p.nxtC.src)
      f(nodeId, p)
    }

    def unary_!() = new MasterAllocTable((n, p) => !f(n, p), isEscape)
    def ||(a2: MasterAllocTable) = new MasterAllocTable((n, p) => f(n, p) || a2(n)(p), p => isEscape(p) || a2.isEscape(p))
    def ||(a2: Boolean)          = new MasterAllocTable((n, p) => f(n, p) || a2      , isEscape)
    def &&(a2: MasterAllocTable) = new MasterAllocTable((n, p) => f(n, p) && a2(n)(p), p => isEscape(p) || a2.isEscape(p))
    def &&(a2: Boolean)          = new MasterAllocTable((n, p) => f(n, p) && a2      , isEscape)
  }
}


