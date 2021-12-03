package constellation

package object topology {
  // srcNodeId, destNodeId => Bool
  type PhysicalTopology = (Int, Int) => Boolean

  case class AllocParams(
    srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, destId: Int, vNetId: Int)

  type NodeAllocTable = AllocParams => Boolean


  class MasterAllocTable(f: (Int, AllocParams) => Boolean) {
    def apply(nodeId: Int): NodeAllocTable = (p: AllocParams) => f(nodeId, p)

    def unary_!() = new MasterAllocTable((n, p) => !f(n, p))
    def ||(a2: MasterAllocTable) = new MasterAllocTable((n, p) => f(n, p) || a2(n)(p))
    def ||(a2: Boolean)          = new MasterAllocTable((n, p) => f(n, p) || a2)
    def &&(a2: MasterAllocTable) = new MasterAllocTable((n, p) => f(n, p) && a2(n)(p))
    def &&(a2: Boolean)          = new MasterAllocTable((n, p) => f(n, p) && a2)
  }
  abstract class NewAllocTable {
    val f: (Int, AllocParams) => Boolean
  }
}


