package constellation.topology

object ChannelAllocPolicies {
  def allLegal(nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, destId: Int, user: Int) = true

  def virtualTLSubnetworks(f: ChannelAllocPolicy)
    (nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, destId: Int, user: Int) = {
    if (srcV == -1) {
      (nxtV % 5 == user &&
        virtualSubnetworks(f, 5)(nodeId)(
          srcId, srcV, nxtId, nxtV, destId, user))
    } else {
      virtualSubnetworks(f, 5)(nodeId)(
        srcId, srcV, nxtId, nxtV, destId, user)
    }
  }


  def virtualSubnetworks(f: ChannelAllocPolicy, n: Int)
    (nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, destId: Int, user: Int) = {
    val srcVNetId = srcV % n
    val nxtVNetId = nxtV % n
    ((srcV == -1 || srcVNetId == nxtVNetId)
      && f(nodeId)(srcId, srcV / n, nxtId, nxtV / n, destId, user))
  }

  def unidirectionalTorus1DDateline(nNodes: Int)(nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, dstId: Int, user: Int) = {
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

  def bidirectionalTorus1DDateline(nNodes: Int)(nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, dstId: Int, user: Int) = {
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

  def mesh2DAlternatingDimensionOrdered(nX: Int, nY: Int)(nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, dstId: Int, user: Int) = {
    val (nodeX, nodeY) = (nodeId / nX, nodeId % nX)
    val (nxtX, nxtY) = (nxtId / nX, nxtId % nX)
    val (srcX, srcY) = (srcId / nX, srcId % nX)
    val (dstX, dstY) = (dstId / nX, dstId % nX)

    val turn = nxtX != srcX && nxtY != srcY
    val canRouteThis = RoutingAlgorithms.mesh2DDimensionOrdered(srcV % 2)(nX, nY)(nodeId)(
      srcId, dstId, nxtId, user)
    val canRouteNext = RoutingAlgorithms.mesh2DDimensionOrdered(nxtV % 2)(nX, nY)(nodeId)(
      srcId, dstId, nxtId, user)

    if (srcId == -1) {
      nxtV != 0 && canRouteNext
    } else if (canRouteThis) {
      nxtV % 2 == srcV % 2 && nxtV <= srcV
    } else if (canRouteNext) {
      nxtV % 2 != srcV % 2 && nxtV <= srcV
    } else {
      false
    }
  }

  def unidirectionalTorus2DDateline(nX: Int, nY: Int)(nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, dstId: Int, user: Int) = {
    val (nodeX, nodeY) = (nodeId / nX, nodeId % nX)
    val (nxtX, nxtY) = (nxtId / nX, nxtId % nX)
    val (srcX, srcY) = (srcId / nX, srcId % nX)
    val (dstX, dstY) = (dstId / nX, dstId % nX)

    val turn = nxtX != srcX && nxtY != srcY
    if (srcId == -1 || turn) {
      nxtV != 0
    } else if (srcX == nxtX) {
      unidirectionalTorus1DDateline(nY)(nodeY)(srcY, srcV, nxtY, nxtV, dstY, user)
    } else if (srcY == nxtY) {
      unidirectionalTorus1DDateline(nX)(nodeX)(srcX, srcV, nxtX, nxtV, dstX, user)
    } else {
      false
    }
  }

  def bidirectionalTorus2DDateline(nX: Int, nY: Int)(nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, dstId: Int, user: Int) = {
    val (nodeX, nodeY) = (nodeId / nX, nodeId % nX)
    val (nxtX, nxtY) = (nxtId / nX, nxtId % nX)
    val (srcX, srcY) = (srcId / nX, srcId % nX)
    val (dstX, dstY) = (dstId / nX, dstId % nX)

    val turn = nxtX != srcX && nxtY != srcY
    if (srcId == -1 || turn) {
      nxtV != 0
    } else if (srcX == nxtX) {
      bidirectionalTorus1DDateline(nY)(nodeY)(srcY, srcV, nxtY, nxtV, dstY, user)
    } else if (srcY == nxtY) {
      bidirectionalTorus1DDateline(nX)(nodeX)(srcX, srcV, nxtX, nxtV, dstX, user)
    } else {
      false
    }
  }

}
