package constellation.topology

object ChannelAllocPolicies {
  def allLegal(nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, destId: Int, prio: Int) = true

  def unidirectionalTorus1DDateline(nNodes: Int)(nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, dstId: Int, prio: Int) = {
    if (srcId == -1)  {
      nxtV != 0
    } else if (srcV == 0) {
      nxtV == 0
    } else if (nodeId == nNodes - 1) {
      nxtV < srcV
    } else {
      nxtV <= srcV && nxtV != 0
    }
  }

  def bidirectionalTorus1DDateline(nNodes: Int)(nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, dstId: Int, prio: Int) = {
    if (srcId == -1)  {
      nxtV != 0
    } else if (srcV == 0) {
      nxtV == 0
    } else if ((nxtId + nNodes - nodeId) % nNodes == 1) {
      if (nodeId == nNodes - 1) {
        nxtV < srcV
      } else {
        nxtV <= srcV && nxtV != 0
      }
    } else if ((nodeId + nNodes - nxtId) % nNodes == 1) {
      if (nodeId == 0) {
        nxtV < srcV
      } else {
        nxtV <= srcV && nxtV != 0
      }
    } else {
      false
    }
  }

  // def mesh2DWestFirst(nX: Int, nY: Int)(nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, dstId: Int, prio: Int) = {
  //   val (nxtX, nxtY) = (nxtId / nX, nxtId % nX)
  //   val (nodeX, nodeY) = (nodeId / nX, nodeId % nX)
  //   val (srcX, srcY) = (srcId / nX, srcId % nX)
  //   val (dstX, dstY) = (dstId / nX, dstId % nX)

  //   !(dstX < nodeX && nxtX != nodeX - 1)
  // }


}
