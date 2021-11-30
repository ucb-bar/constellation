package constellation

package object topology {
  // srcNodeId, destNodeId => Bool
  type PhysicalTopology = (Int, Int) => Boolean

  case class AllocParams(
    srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, destId: Int, vNetId: Int)

  type NodeAllocTable = AllocParams => Boolean
  type MasterAllocTable = Int => NodeAllocTable
}


