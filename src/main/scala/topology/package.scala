package constellation

package object topology {
  // srcNodeId, destNodeId => Bool
  type PhysicalTopology = (Int, Int) => Boolean

  // nodeId => srcNodeId, inVChannelId, nextNodeId, outVChannelId, destId, user => legalPath
  type MasterAllocTable = Int => (Int, Int, Int, Int, Int, Int) => Boolean
}


