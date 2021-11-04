package constellation.topology

import scala.math.pow

object MasterAllocTables {
  def allLegal(nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, destId: Int, vNetId: Int) = true

  def bidirectionalLine(nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, dstId: Int, vNetId: Int) = {
    (if (nodeId < nxtId) dstId >= nxtId else dstId <= nxtId) && nxtId != srcId
  }

  def unidirectionalTorus1DDateline(nNodes: Int)(nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, dstId: Int, vNetId: Int) = {
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



  private def bidirectionalTorus1DDateline(nNodes: Int)(nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, dstId: Int, vNetId: Int) = {
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

  def bidirectionalTorus1DShortest(nNodes: Int)(nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, dstId: Int, vNetId: Int) = {

    val cwDist = (dstId + nNodes - nodeId) % nNodes
    val ccwDist = (nodeId + nNodes - dstId) % nNodes
    val distSel = if (cwDist < ccwDist) {
      (nxtId + nNodes - nodeId) % nNodes == 1
    } else if (cwDist > ccwDist) {
      (nodeId + nNodes - nxtId) % nNodes == 1
    } else {
      true
    }
    distSel && bidirectionalTorus1DDateline(nNodes)(nodeId)(srcId, srcV, nxtId, nxtV, dstId, vNetId)
  }

  def bidirectionalTorus1DRandom(nNodes: Int)(nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, dstId: Int, vNetId: Int) = {
    val sel = if (srcId == -1) {
      true
    } else if ((nodeId + nNodes - srcId) % nNodes == 1) {
      (nxtId + nNodes - nodeId) % nNodes == 1
    } else {
      (nodeId + nNodes - nxtId) % nNodes == 1
    }
    sel && bidirectionalTorus1DDateline(nNodes)(nodeId)(srcId, srcV, nxtId, nxtV, dstId, vNetId)
  }

  def butterfly(kAry: Int, nFly: Int) = {
    require(kAry >= 2 && nFly >= 2)
    val height = pow(kAry, nFly-1).toInt
    def digitsToNum(dig: Seq[Int]) = dig.zipWithIndex.map { case (d,i) => d * pow(kAry,i).toInt }.sum
    val table = (0 until pow(kAry, nFly).toInt).map { i =>
      (0 until nFly).map { n => (i / pow(kAry, n).toInt) % kAry }
    }
    val channels = (1 until nFly).map { i =>
      table.map { e => (digitsToNum(e.drop(1)), digitsToNum(e.updated(i, e(0)).drop(1))) }
    }

    (nodeId: Int) => (srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, dstId: Int, vNetId: Int) => {
      val (nxtX, nxtY) = (nxtId / height, nxtId % height)
      val (nodeX, nodeY) = (nodeId / height, nodeId % height)
      val (dstX, dstY) = (dstId / height, dstId % height)
      if (dstX <= nodeX) {
        false
      } else if (nodeX == nFly-1) {
        true
      } else {
        val dsts = (nxtX until nFly-1).foldRight((0 until height).map { i => Seq(i) }) {
          case (i,l) => (0 until height).map { s => channels(i).filter(_._1 == s).map { case (_,d) =>
            l(d)
          }.flatten }
        }
        dsts(nxtY).contains(dstId % height)
      }
    }
  }


  def mesh2DDimensionOrdered(firstDim: Int = 0)(nX: Int, nY: Int)(nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, dstId: Int, vNetId: Int) = {
    val (nxtX, nxtY)   = (nxtId % nX , nxtId / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (dstId % nX , dstId / nX)
    val (srcX, srcY)   = (srcId % nX , srcId / nX)

    if (firstDim == 0) {
      if (dstX != nodeX) {
        (if (nodeX < nxtX) dstX >= nxtX else dstX <= nxtX) && nxtY == nodeY
      } else {
        (if (nodeY < nxtY) dstY >= nxtY else dstY <= nxtY) && nxtX == nodeX
      }
    } else {
      if (dstY != nodeY) {
        (if (nodeY < nxtY) dstY >= nxtY else dstY <= nxtY) && nxtX == nodeX
      } else {
        (if (nodeX < nxtX) dstX >= nxtX else dstX <= nxtX) && nxtY == nodeY
      }
    }
  }

  private def mesh2DMinimal(nX: Int, nY: Int)(nodeId: Int)(srcId: Int, nxtId: Int, dstId: Int, vNetId: Int) = {
    val (nxtX, nxtY)   = (nxtId % nX , nxtId / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (dstId % nX , dstId / nX)
    val (srcX, srcY)   = (srcId % nX , srcId / nX)

    val xR = (if (nodeX < nxtX) dstX >= nxtX else if (nodeX > nxtX) dstX <= nxtX else nodeX == nxtX)
    val yR = (if (nodeY < nxtY) dstY >= nxtY else if (nodeY > nxtY) dstY <= nxtY else nodeY == nxtY)
    xR && yR
  }

  def mesh2DWestFirst(nX: Int, nY: Int)(nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, dstId: Int, vNetId: Int) = {
    val (nxtX, nxtY)   = (nxtId % nX , nxtId / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (dstId % nX , dstId / nX)
    val (srcX, srcY)   = (srcId % nX , srcId / nX)

    if (dstX < nodeX) {
      nxtX == nodeX - 1
    } else {
      mesh2DMinimal(nX, nY)(nodeId)(srcId, nxtId, dstId, vNetId)
    }
  }

  def mesh2DNorthLast(nX: Int, nY: Int)(nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, dstId: Int, vNetId: Int) = {
    val (nxtX, nxtY)   = (nxtId % nX , nxtId / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (dstId % nX , dstId / nX)
    val (srcX, srcY)   = (srcId % nX , srcId / nX)

    if (dstY > nodeY && dstX != nodeX) {
      mesh2DMinimal(nX, nY)(nodeId)(srcId, nxtId, dstId, vNetId) && nxtY != nodeY + 1
    } else if (dstY > nodeY) {
      nxtY == nodeY + 1
    } else {
      mesh2DMinimal(nX, nY)(nodeId)(srcId, nxtId, dstId, vNetId)
    }
  }



  def mesh2DAlternatingDimensionOrdered(nX: Int, nY: Int)(nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, dstId: Int, vNetId: Int) = {
    val (nxtX, nxtY)   = (nxtId % nX , nxtId / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (dstId % nX , dstId / nX)
    val (srcX, srcY)   = (srcId % nX , srcId / nX)

    val turn = nxtX != srcX && nxtY != srcY
    val canRouteThis = mesh2DDimensionOrdered(srcV % 2)(nX, nY)(nodeId)(
      srcId, srcV, nxtId, nxtV, dstId, vNetId)
    val canRouteNext = mesh2DDimensionOrdered(nxtV % 2)(nX, nY)(nodeId)(
      srcId, dstId, nxtId, nxtV, dstId, vNetId)

    val sel = if (srcId == -1) {
      canRouteNext
    } else if (canRouteThis) {
      nxtV % 2 == srcV % 2 && nxtV <= srcV
    } else if (canRouteNext) {
      nxtV % 2 != srcV % 2 && nxtV <= srcV
    } else {
      false
    }
    sel && mesh2DMinimal(nX, nY)(nodeId)(srcId, nxtId, dstId, vNetId)
  }

  def mesh2DDimensionOrderedHighest(nX: Int, nY: Int)(nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, dstId: Int, vNetId: Int) = {
    val (nxtX, nxtY)   = (nxtId % nX , nxtId / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (dstId % nX , dstId / nX)
    val (srcX, srcY)   = (srcId % nX , srcId / nX)

    if (nxtV == 0) {
      mesh2DDimensionOrdered()(nX, nY)(nodeId)(
        srcId, srcV, nxtId, nxtV, dstId, vNetId)
    } else if (srcId == -1) {
      nxtV != 0 && mesh2DMinimal(nX, nY)(nodeId)(srcId, nxtId, dstId, vNetId)
    } else {
      nxtV <= srcV && mesh2DMinimal(nX, nY)(nodeId)(srcId, nxtId, dstId, vNetId)
    }
  }

  private def unidirectionalTorus2DDateline(nX: Int, nY: Int)(nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, dstId: Int, vNetId: Int) = {
    val (nxtX, nxtY)   = (nxtId % nX , nxtId / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (dstId % nX , dstId / nX)
    val (srcX, srcY)   = (srcId % nX , srcId / nX)

    val turn = nxtX != srcX && nxtY != srcY
    if (srcId == -1 || turn) {
      nxtV != 0
    } else if (srcX == nxtX) {
      unidirectionalTorus1DDateline(nY)(nodeY)(srcY, srcV, nxtY, nxtV, dstY, vNetId)
    } else if (srcY == nxtY) {
      unidirectionalTorus1DDateline(nX)(nodeX)(srcX, srcV, nxtX, nxtV, dstX, vNetId)
    } else {
      false
    }
  }

  private def bidirectionalTorus2DDateline(nX: Int, nY: Int)(nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, dstId: Int, vNetId: Int) = {
    val (nxtX, nxtY)   = (nxtId % nX , nxtId / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (dstId % nX , dstId / nX)
    val (srcX, srcY)   = (srcId % nX , srcId / nX)

    if (srcId == -1) {
      nxtV != 0
    } else if (nodeX == nxtX) {
      bidirectionalTorus1DDateline(nY)(nodeY)(srcY, srcV, nxtY, nxtV, dstY, vNetId)
    } else if (nodeY == nxtY) {
      bidirectionalTorus1DDateline(nX)(nodeX)(srcX, srcV, nxtX, nxtV, dstX, vNetId)
    } else {
      false
    }
  }



  def dimensionOrderedUnidirectionalTorus2DDateline(nX: Int, nY: Int)(nodeId: Int)(
    srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, dstId: Int, vNetId: Int) = {

    val (nxtX, nxtY)   = (nxtId % nX , nxtId / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (dstId % nX , dstId / nX)
    val (srcX, srcY)   = (srcId % nX , srcId / nX)

    def sel = if (dstX != nodeX) {
      nxtY == nodeY
    } else {
      nxtX == nodeX
    }
    sel && unidirectionalTorus2DDateline(nX, nY)(nodeId)(srcId, srcV, nxtId, nxtV, dstId, vNetId)
  }

  def dimensionOrderedBidirectionalTorus2DDateline(nX: Int, nY: Int)(nodeId: Int)(
    srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, dstId: Int, vNetId: Int) = {

    val (nxtX, nxtY)   = (nxtId % nX , nxtId / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (dstId % nX , dstId / nX)
    val (srcX, srcY)   = (srcId % nX , srcId / nX)

    val xdir = bidirectionalTorus1DShortest(nX)(nodeX)(if (srcId == -1) -1 else srcX, srcV, nxtX, nxtV, dstX, vNetId)
    val ydir = bidirectionalTorus1DShortest(nY)(nodeY)(if (srcId == -1) -1 else srcY, srcV, nxtY, nxtV, dstY, vNetId)
    val base = bidirectionalTorus2DDateline(nX, nY)(nodeId)(srcId, srcV, nxtId, nxtV, dstId, vNetId)
    val sel = if (dstX != nodeX) xdir else ydir

    sel && base
  }


  // The below tables implement support for virtual subnetworks in a variety of ways
  // NOTE: The topology must have sufficient virtual channels for these to work correctly
  // TODO: Write assertions to check this

  // Independent virtual subnets with no resource sharing
  def nonblockingVirtualSubnetworks(f: MasterAllocTable, n: Int)
    (nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, destId: Int, vNetId: Int) = {
    (vNetId % n == nxtV) && f(nodeId)(srcId, srcV / n, nxtId, nxtV / n, destId, 0)
  }

  // Virtual subnets with 1 dedicated virtual channel each, and some number of shared channels
  def sharedNonblockingVirtualSubnetworks(f: MasterAllocTable, n: Int, nSharedChannels: Int)
    (nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, destId: Int, vNetId: Int) = {
    def trueVIdToVirtualVId(vId: Int) = if (vId < n) 0 else vId - n
    f(nodeId)(srcId, trueVIdToVirtualVId(srcV), nxtId, trueVIdToVirtualVId(nxtV), destId, 0)
  }

  def blockingVirtualSubnetworks(f: MasterAllocTable, n: Int)
    (nodeId: Int)(srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, destId: Int, vNetId: Int) = {
    val lNxtV = nxtV - vNetId
    if (lNxtV < 0) {
      false
    } else {
      f(nodeId)(srcId, srcV, nxtId, lNxtV, destId, 0)
    }
  }
}
