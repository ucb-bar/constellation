package constellation.topology

object ChannelAllocPolicies {
  def allLegal(nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, destId: Int, prio: Int) = true

  def virtualTLSubnetworks(f: ChannelAllocPolicy)
    (nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, destId: Int, prio: Int) = {
    if (srcV == -1) {
      (nxtV % 5 == prio &&
        virtualSubnetworks(f, 5)(nodeId)(
          srcId, srcV, nxtId, nxtV, destId, prio))
    } else {
      virtualSubnetworks(f, 5)(nodeId)(
        srcId, srcV, nxtId, nxtV, destId, prio)
    }
  }


  def virtualSubnetworks(f: ChannelAllocPolicy, n: Int)
    (nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, destId: Int, prio: Int) = {
    val srcVNetId = srcV % n
    val nxtVNetId = nxtV % n
    ((srcV == -1 || srcVNetId == nxtVNetId)
      && f(nodeId)(srcId, srcV / n, nxtId, nxtV / n, destId, prio))
  }

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

  def unidirectionalTorus2DDateline(nX: Int, nY: Int)(nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, dstId: Int, prio: Int) = {
    val (nodeX, nodeY) = (nodeId / nX, nodeId % nX)
    val (nxtX, nxtY) = (nxtId / nX, nxtId % nX)
    val (srcX, srcY) = (srcId / nX, srcId % nX)
    val (dstX, dstY) = (dstId / nX, dstId % nX)

    val turn = nxtX != srcX && nxtY != srcY
    if (srcId == -1 || turn) {
      nxtV != 0
    } else if (srcX == nxtX) {
      unidirectionalTorus1DDateline(nY)(nodeY)(srcY, srcV, nxtY, nxtV, dstY, prio)
    } else if (srcY == nxtY) {
      unidirectionalTorus1DDateline(nX)(nodeX)(srcX, srcV, nxtX, nxtV, dstX, prio)
    } else {
      false
    }
  }

  def bidirectionalTorus2DDateline(nX: Int, nY: Int)(nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, dstId: Int, prio: Int) = {
    val (nodeX, nodeY) = (nodeId / nX, nodeId % nX)
    val (nxtX, nxtY) = (nxtId / nX, nxtId % nX)
    val (srcX, srcY) = (srcId / nX, srcId % nX)
    val (dstX, dstY) = (dstId / nX, dstId % nX)

    val turn = nxtX != srcX && nxtY != srcY
    if (srcId == -1 || turn) {
      nxtV != 0
    } else if (srcX == nxtX) {
      bidirectionalTorus1DDateline(nY)(nodeY)(srcY, srcV, nxtY, nxtV, dstY, prio)
    } else if (srcY == nxtY) {
      bidirectionalTorus1DDateline(nX)(nodeX)(srcX, srcV, nxtX, nxtV, dstX, prio)
    } else {
      false
    }
  }

}
