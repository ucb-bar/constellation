package constellation.topology

object RoutingAlgorithms {
  def crazy(nodeId: Int)(lastId: Int, destId: Int, nextId: Int, prio: Int) = true

  def bidirectionalLine(nodeId: Int)(lastId: Int, destId: Int, nextId: Int, prio: Int) = {
    (if (nodeId < nextId) destId >= nextId else destId <= nextId) && nextId != lastId
  }

  def bidirectionalTorus1DShortest(nNodes: Int)(nodeId: Int)(lastId: Int, destId: Int, nextId: Int, prio: Int) = {
    val cwDist = (destId + nNodes - nodeId) % nNodes
    val ccwDist = (nodeId + nNodes - destId) % nNodes
    if (cwDist < ccwDist) {
      (nextId + nNodes - nodeId) % nNodes == 1
    } else if (cwDist > ccwDist) {
      (nodeId + nNodes - nextId) % nNodes == 1
    } else {
      true
    }
  }
  def bidirectionalTorus1DRandom(nNodes: Int)(nodeId: Int)(lastId: Int, destId: Int, nextId: Int, prio: Int) = {
    if (lastId == -1) {
      true
    } else if ((nodeId + nNodes - lastId) % nNodes == 1) {
      (nextId + nNodes - nodeId) % nNodes == 1
    } else {
      (nodeId + nNodes - nextId) % nNodes == 1
    }
  }
}
