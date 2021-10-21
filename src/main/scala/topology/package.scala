package constellation

package object topology {
  // srcNodeId, destNodeId => Bool
  type PhysicalTopology = (Int, Int) => Boolean

  // nodeId => lastId, destId, nextId, user => usePath
  type RoutingFunction = Int => (Int, Int, Int, Int) => Boolean

  // nodeId => srcNodeId, inVChannelId, nextNodeId, outVChannelId, destId, user => legalPath
  type ChannelAllocPolicy = Int => (Int, Int, Int, Int, Int, Int) => Boolean
}


