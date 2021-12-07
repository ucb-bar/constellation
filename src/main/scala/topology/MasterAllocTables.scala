package constellation.topology

import scala.math.pow

object MasterAllocTables {

  // Utility functions
  val srcIsIngress = new MasterAllocTable((_, p) => p.srcId == -1)
  val nxtIsVC0     = new MasterAllocTable((_, p) => p.nxtV == 0)
  val srcIsVC0     = new MasterAllocTable((_, p) => p.srcV == 0)
  val nxtVLTSrcV   = new MasterAllocTable((_, p) => p.nxtV < p.srcV)
  val nxtVLESrcV   = new MasterAllocTable((_, p) => p.nxtV <= p.srcV)

  // Usable policies
  val allLegal = new MasterAllocTable((_, _) => true)

  val bidirectionalLine = new MasterAllocTable((nodeId, p) => {
    if (nodeId < p.nxtId) p.destId >= p.nxtId else p.destId <= p.nxtId
  })

  def unidirectionalTorus1DDateline(nNodes: Int) = new MasterAllocTable((nodeId, p) => {
    // (if (srcIsIngress(nodeId)(p)) {
    //   !nxtIsVC0
    // } else if (srcIsVC0(nodeId)(p)) {
    //   nxtIsVC0
    // } else if (nodeId == nNodes - 1) {
    //   nxtVLTSrcV
    // } else {
    //   nxtVLESrcV && !nxtIsVC0
    // })(nodeId)(p)

    if (p.srcId == -1)  {
      p.nxtV != 0
    } else if (p.srcV == 0) {
      p.nxtV == 0
    } else if (nodeId == nNodes - 1) {
      p.nxtV < p.srcV
    } else {
      p.nxtV <= p.srcV && p.nxtV != 0
    }
  })



  def bidirectionalTorus1DDateline(nNodes: Int) = new MasterAllocTable((nodeId, p) => {
    if (p.srcId == -1)  {
      p.nxtV != 0
    } else if (p.srcV == 0) {
      p.nxtV == 0
    } else if ((p.nxtId + nNodes - nodeId) % nNodes == 1) {
      if (nodeId == nNodes - 1) {
        p.nxtV < p.srcV
      } else {
        p.nxtV <= p.srcV && p.nxtV != 0
      }
    } else if ((nodeId + nNodes - p.nxtId) % nNodes == 1) {
      if (nodeId == 0) {
        p.nxtV < p.srcV
      } else {
        p.nxtV <= p.srcV && p.nxtV != 0
      }
    } else {
      false
    }
  })

  def bidirectionalTorus1DShortest(nNodes: Int) = new MasterAllocTable((nodeId, p) => {
    val cwDist = (p.destId + nNodes - nodeId) % nNodes
    val ccwDist = (nodeId + nNodes - p.destId) % nNodes
    val distSel = if (cwDist < ccwDist) {
      (p.nxtId + nNodes - nodeId) % nNodes == 1
    } else if (cwDist > ccwDist) {
      (nodeId + nNodes - p.nxtId) % nNodes == 1
    } else {
      true
    }
    distSel && bidirectionalTorus1DDateline(nNodes)(nodeId)(p)
  })

  def bidirectionalTorus1DRandom(nNodes: Int) = new MasterAllocTable((nodeId, p) => {
    val sel = if (p.srcId == -1) {
      true
    } else if ((nodeId + nNodes - p.srcId) % nNodes == 1) {
      (p.nxtId + nNodes - nodeId) % nNodes == 1
    } else {
      (nodeId + nNodes - p.nxtId) % nNodes == 1
    }
    sel && bidirectionalTorus1DDateline(nNodes)(nodeId)(p)
  })

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

    new MasterAllocTable((nodeId, p) => {
      val (nxtX, nxtY) = (p.nxtId / height, p.nxtId % height)
      val (nodeX, nodeY) = (nodeId / height, nodeId % height)
      val (dstX, dstY) = (p.destId / height, p.destId % height)
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
        dsts(nxtY).contains(p.destId % height)
      }
    })
  }


  def mesh2DDimensionOrdered(firstDim: Int = 0)(nX: Int, nY: Int) = new MasterAllocTable((nodeId, p) => {
    val (nxtX, nxtY)   = (p.nxtId % nX , p.nxtId / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (p.destId % nX , p.destId / nX)
    val (srcX, srcY)   = (p.srcId % nX , p.srcId / nX)

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
  })

  // WARNING: Not deadlock free
  def mesh2DMinimal(nX: Int, nY: Int) = new MasterAllocTable((nodeId, p) => {
    val (nxtX, nxtY)   = (p.nxtId % nX , p.nxtId / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (p.destId % nX, p.destId / nX)
    val (srcX, srcY)   = (p.srcId % nX , p.srcId / nX)

    val xR = (if (nodeX < nxtX) dstX >= nxtX else if (nodeX > nxtX) dstX <= nxtX else nodeX == nxtX)
    val yR = (if (nodeY < nxtY) dstY >= nxtY else if (nodeY > nxtY) dstY <= nxtY else nodeY == nxtY)
    xR && yR
  })


  def mesh2DWestFirst(nX: Int, nY: Int) = new MasterAllocTable((nodeId, p) => {
    val (nxtX, nxtY)   = (p.nxtId % nX , p.nxtId / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (p.destId % nX , p.destId / nX)
    val (srcX, srcY)   = (p.srcId % nX , p.srcId / nX)

    (if (dstX < nodeX) {
      new MasterAllocTable((nodeId, p) => nxtX == nodeX - 1)
    } else {
      mesh2DMinimal(nX, nY)
    })(nodeId)(p)
  })

  def mesh2DNorthLast(nX: Int, nY: Int) = new MasterAllocTable((nodeId, p) => {
    val (nxtX, nxtY)   = (p.nxtId % nX , p.nxtId / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (p.destId % nX , p.destId / nX)
    val (srcX, srcY)   = (p.srcId % nX , p.srcId / nX)

    (if (dstY > nodeY && dstX != nodeX) {
      mesh2DMinimal(nX, nY) && nxtY != nodeY + 1
    } else if (dstY > nodeY) {
      new MasterAllocTable((nodeId, p) => nxtY == nodeY + 1)
    } else {
      mesh2DMinimal(nX, nY)
    })(nodeId)(p)
  })



  def mesh2DAlternatingDimensionOrdered(nX: Int, nY: Int) = new MasterAllocTable((nodeId, p) => {
    val (nxtX, nxtY)   = (p.nxtId % nX , p.nxtId / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (p.destId % nX , p.destId / nX)
    val (srcX, srcY)   = (p.srcId % nX , p.srcId / nX)

    val turn = nxtX != srcX && nxtY != srcY
    val canRouteThis = mesh2DDimensionOrdered(p.srcV % 2)(nX, nY)
    val canRouteNext = mesh2DDimensionOrdered(p.nxtV % 2)(nX, nY)

    val sel = if (p.srcId == -1) {
      canRouteNext
    } else {
      (canRouteThis && p.nxtV % 2 == p.srcV % 2 && p.nxtV <= p.srcV) || (canRouteNext && p.nxtV % 2 != p.srcV % 2 && p.nxtV <= p.srcV)
    }
    (mesh2DMinimal(nX, nY) && sel)(nodeId)(p)
  })

  def mesh2DDimensionOrderedHighest(nX: Int, nY: Int) = new MasterAllocTable((nodeId, p) => {
    (if (p.nxtV == 0) {
      mesh2DDimensionOrdered()(nX, nY)
    } else if (p.srcId == -1) {
      !nxtIsVC0 && mesh2DMinimal(nX, nY)
    } else {
      nxtVLESrcV && mesh2DMinimal(nX, nY)
    })(nodeId)(p)
  })

  def escapeChannels(escapeRouter: MasterAllocTable, normalRouter: MasterAllocTable, nEscapeChannels: Int = 1) = new MasterAllocTable((nodeId, p) => {
    if (p.srcId == -1) {
      if (p.nxtV >= nEscapeChannels) {
        normalRouter(nodeId)(p.copy(nxtV=p.nxtV-nEscapeChannels))
      } else {
        escapeRouter(nodeId)(p)
      }
    } else if (p.srcV < nEscapeChannels && p.nxtV < nEscapeChannels) {
      escapeRouter(nodeId)(p)
    } else if (p.srcV >= nEscapeChannels && p.nxtV >= nEscapeChannels) {
      normalRouter(nodeId)(p.copy(srcV=p.srcV-nEscapeChannels, nxtV=p.nxtV-nEscapeChannels))
    } else if (p.srcV >= nEscapeChannels && p.nxtV < nEscapeChannels) {
      normalRouter(nodeId)(p.copy(srcV=p.srcV-nEscapeChannels, nxtV=0))
    } else {
      false
    }
  })

  def mesh2DBestRouter(nX: Int, nY: Int) = escapeChannels(mesh2DDimensionOrdered()(nX, nY), mesh2DMinimal(nX, nY))

  def unidirectionalTorus2DDateline(nX: Int, nY: Int) = new MasterAllocTable((nodeId, p) => {
    val (nxtX, nxtY)   = (p.nxtId % nX , p.nxtId / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (p.destId % nX , p.destId / nX)
    val (srcX, srcY)   = (p.srcId % nX , p.srcId / nX)

    val turn = nxtX != srcX && nxtY != srcY
    if (p.srcId == -1 || turn) {
      p.nxtV != 0
    } else if (srcX == nxtX) {
      unidirectionalTorus1DDateline(nY)(nodeY)(p.copy(srcId=srcY, nxtId=nxtY, destId=dstY))
    } else if (srcY == nxtY) {
      unidirectionalTorus1DDateline(nX)(nodeX)(p.copy(srcId=srcX, nxtId=nxtX, destId=dstX))
    } else {
      false
    }
  })

  def bidirectionalTorus2DDateline(nX: Int, nY: Int) = new MasterAllocTable((nodeId, p) => {
    val (nxtX, nxtY)   = (p.nxtId % nX , p.nxtId / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (p.destId % nX , p.destId / nX)
    val (srcX, srcY)   = (p.srcId % nX , p.srcId / nX)

    if (p.srcId == -1) {
      p.nxtV != 0
    } else if (nodeX == nxtX) {
      bidirectionalTorus1DDateline(nY)(nodeY)(p.copy(srcId=srcY, nxtId=nxtY, destId=dstY))
    } else if (nodeY == nxtY) {
      bidirectionalTorus1DDateline(nX)(nodeX)(p.copy(srcId=srcX, nxtId=nxtX, destId=dstX))
    } else {
      false
    }
  })



  def dimensionOrderedUnidirectionalTorus2DDateline(nX: Int, nY: Int) = new MasterAllocTable((nodeId, p) => {
    val (nxtX, nxtY)   = (p.nxtId % nX , p.nxtId / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (p.destId % nX , p.destId / nX)
    val (srcX, srcY)   = (p.srcId % nX , p.srcId / nX)

    def sel = if (dstX != nodeX) {
      nxtY == nodeY
    } else {
      nxtX == nodeX
    }
    (unidirectionalTorus2DDateline(nX, nY) && sel)(nodeId)(p)
  })

  def dimensionOrderedBidirectionalTorus2DDateline(nX: Int, nY: Int) = new MasterAllocTable((nodeId, p) => {
    val (nxtX, nxtY)   = (p.nxtId % nX , p.nxtId / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (p.destId % nX , p.destId / nX)
    val (srcX, srcY)   = (p.srcId % nX , p.srcId / nX)

    val xdir = bidirectionalTorus1DShortest(nX)(nodeX)(p.copy(srcId=(if (p.srcId == -1) -1 else srcX), nxtId=nxtX, destId=dstX))
    val ydir = bidirectionalTorus1DShortest(nY)(nodeY)(p.copy(srcId=(if (p.srcId == -1) -1 else srcY), nxtId=nxtY, destId=dstY))
    val base = bidirectionalTorus2DDateline(nX, nY)(nodeId)(p)
    val sel = if (dstX != nodeX) xdir else ydir

    sel && base
  })


  // The below tables implement support for virtual subnetworks in a variety of ways
  // NOTE: The topology must have sufficient virtual channels for these to work correctly
  // TODO: Write assertions to check this

  // Independent virtual subnets with no resource sharing
  def nonblockingVirtualSubnetworks(f: MasterAllocTable, n: Int) = new MasterAllocTable((nodeId, p) => {
    (p.nxtV % n == p.vNetId) && f(nodeId)(p.copy(srcV=p.srcV / n, nxtV=p.nxtV / n, vNetId=0))
  })

  // Virtual subnets with 1 dedicated virtual channel each, and some number of shared channels
  def sharedNonblockingVirtualSubnetworks(f: MasterAllocTable, n: Int, nSharedChannels: Int) = new MasterAllocTable((nodeId, p) => {
    def trueVIdToVirtualVId(vId: Int) = if (vId < n) 0 else vId - n
    f(nodeId)(p.copy(srcV=trueVIdToVirtualVId(p.srcV), nxtV=trueVIdToVirtualVId(p.nxtV), vNetId=0))
  })

  def blockingVirtualSubnetworks(f: MasterAllocTable, n: Int) = new MasterAllocTable((nodeId, p) => {
    val lNxtV = p.nxtV - p.vNetId
    if (lNxtV < 0) {
      false
    } else {
      f(nodeId)(p.copy(nxtV=lNxtV, vNetId=0))
    }
  })
}
