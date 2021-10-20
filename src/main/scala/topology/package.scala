package constellation

package object topology {
  // srcNodeId, destNodeId => Bool
  type PhysicalTopology = (Int, Int) => Boolean

  // nodeId => lastId, destId, nextId, prio => usePath
  type RoutingFunction = Int => (Int, Int, Int, Int) => Boolean

  // nodeId => srcNodeId, inVChannelId, nextNodeId, outVChannelId, destId, prio => legalPath
  type ChannelAllocPolicy = Int => (Int, Int, Int, Int, Int, Int) => Boolean
}


