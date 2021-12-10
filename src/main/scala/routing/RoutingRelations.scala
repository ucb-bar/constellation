package constellation.routing

import scala.math.pow

object RoutingRelations {

  // Utility functions
  val srcIsIngress = new RoutingRelation((_, p) => p.srcC.src == -1)
  val nxtIsVC0     = new RoutingRelation((_, p) => p.nxtC.vc == 0)
  val srcIsVC0     = new RoutingRelation((_, p) => p.srcC.vc == 0)
  val nxtVLTSrcV   = new RoutingRelation((_, p) => p.nxtC.vc < p.srcC.vc)
  val nxtVLESrcV   = new RoutingRelation((_, p) => p.nxtC.vc <= p.srcC.vc)

  // Usable policies
  val allLegal = new RoutingRelation((_, _) => true)

  val bidirectionalLine = new RoutingRelation((nodeId, p) => {
    if (nodeId < p.nxtC.dst) p.pInfo.dst >= p.nxtC.dst else p.pInfo.dst <= p.nxtC.dst
  })

  def unidirectionalTorus1DDateline(nNodes: Int) = new RoutingRelation((nodeId, p) => {
    // (if (srcIsIngress(nodeId)(p)) {
    //   !nxtIsVC0
    // } else if (srcIsVC0(nodeId)(p)) {
    //   nxtIsVC0
    // } else if (nodeId == nNodes - 1) {
    //   nxtVLTSrcV
    // } else {
    //   nxtVLESrcV && !nxtIsVC0
    // })(nodeId)(p)

    if (p.srcC.src == -1)  {
      p.nxtC.vc != 0
    } else if (p.srcC.vc == 0) {
      p.nxtC.vc == 0
    } else if (nodeId == nNodes - 1) {
      p.nxtC.vc < p.srcC.vc
    } else {
      p.nxtC.vc <= p.srcC.vc && p.nxtC.vc != 0
    }
  })



  def bidirectionalTorus1DDateline(nNodes: Int) = new RoutingRelation((nodeId, p) => {
    if (p.srcC.src == -1)  {
      p.nxtC.vc != 0
    } else if (p.srcC.vc == 0) {
      p.nxtC.vc == 0
    } else if ((p.nxtC.dst + nNodes - nodeId) % nNodes == 1) {
      if (nodeId == nNodes - 1) {
        p.nxtC.vc < p.srcC.vc
      } else {
        p.nxtC.vc <= p.srcC.vc && p.nxtC.vc != 0
      }
    } else if ((nodeId + nNodes - p.nxtC.dst) % nNodes == 1) {
      if (nodeId == 0) {
        p.nxtC.vc < p.srcC.vc
      } else {
        p.nxtC.vc <= p.srcC.vc && p.nxtC.vc != 0
      }
    } else {
      false
    }
  })

  def bidirectionalTorus1DShortest(nNodes: Int) = new RoutingRelation((nodeId, p) => {
    val cwDist = (p.pInfo.dst + nNodes - nodeId) % nNodes
    val ccwDist = (nodeId + nNodes - p.pInfo.dst) % nNodes
    val distSel = if (cwDist < ccwDist) {
      (p.nxtC.dst + nNodes - nodeId) % nNodes == 1
    } else if (cwDist > ccwDist) {
      (nodeId + nNodes - p.nxtC.dst) % nNodes == 1
    } else {
      true
    }
    distSel && bidirectionalTorus1DDateline(nNodes)(nodeId)(p)
  })

  def bidirectionalTorus1DRandom(nNodes: Int) = new RoutingRelation((nodeId, p) => {
    val sel = if (p.srcC.src == -1) {
      true
    } else if ((nodeId + nNodes - p.srcC.src) % nNodes == 1) {
      (p.nxtC.dst + nNodes - nodeId) % nNodes == 1
    } else {
      (nodeId + nNodes - p.nxtC.dst) % nNodes == 1
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

    new RoutingRelation((nodeId, p) => {
      val (nxtX, nxtY) = (p.nxtC.dst / height, p.nxtC.dst % height)
      val (nodeX, nodeY) = (nodeId / height, nodeId % height)
      val (dstX, dstY) = (p.pInfo.dst / height, p.pInfo.dst % height)
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
        dsts(nxtY).contains(p.pInfo.dst % height)
      }
    })
  }


  def mesh2DDimensionOrdered(firstDim: Int = 0)(nX: Int, nY: Int) = new RoutingRelation((nodeId, p) => {
    val (nxtX, nxtY)   = (p.nxtC.dst % nX , p.nxtC.dst / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (p.pInfo.dst % nX , p.pInfo.dst / nX)

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
  def mesh2DMinimal(nX: Int, nY: Int) = new RoutingRelation((nodeId, p) => {
    val (nxtX, nxtY)   = (p.nxtC.dst % nX , p.nxtC.dst / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (p.pInfo.dst % nX, p.pInfo.dst / nX)

    val xR = (if (nodeX < nxtX) dstX >= nxtX else if (nodeX > nxtX) dstX <= nxtX else nodeX == nxtX)
    val yR = (if (nodeY < nxtY) dstY >= nxtY else if (nodeY > nxtY) dstY <= nxtY else nodeY == nxtY)
    xR && yR
  })


  def mesh2DWestFirst(nX: Int, nY: Int) = new RoutingRelation((nodeId, p) => {
    val (nxtX, nxtY)   = (p.nxtC.dst % nX , p.nxtC.dst / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (p.pInfo.dst % nX , p.pInfo.dst / nX)

    (if (dstX < nodeX) {
      new RoutingRelation((nodeId, p) => nxtX == nodeX - 1)
    } else {
      mesh2DMinimal(nX, nY)
    })(nodeId)(p)
  })

  def mesh2DNorthLast(nX: Int, nY: Int) = new RoutingRelation((nodeId, p) => {
    val (nxtX, nxtY)   = (p.nxtC.dst % nX , p.nxtC.dst / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (p.pInfo.dst % nX , p.pInfo.dst / nX)

    (if (dstY > nodeY && dstX != nodeX) {
      mesh2DMinimal(nX, nY) && nxtY != nodeY + 1
    } else if (dstY > nodeY) {
      new RoutingRelation((nodeId, p) => nxtY == nodeY + 1)
    } else {
      mesh2DMinimal(nX, nY)
    })(nodeId)(p)
  })



  def mesh2DAlternatingDimensionOrdered(nX: Int, nY: Int) = new RoutingRelation((nodeId, p) => {
    val (nxtX, nxtY)   = (p.nxtC.dst % nX , p.nxtC.dst / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (p.pInfo.dst % nX , p.pInfo.dst / nX)
    val (srcX, srcY)   = (p.srcC.src % nX , p.srcC.src / nX)

    val turn = nxtX != srcX && nxtY != srcY
    val canRouteThis = mesh2DDimensionOrdered(p.srcC.vc % 2)(nX, nY)
    val canRouteNext = mesh2DDimensionOrdered(p.nxtC.vc % 2)(nX, nY)

    val sel = if (p.srcC.src == -1) {
      canRouteNext
    } else {
      (canRouteThis && p.nxtC.vc % 2 == p.srcC.vc % 2 && p.nxtC.vc <= p.srcC.vc) || (canRouteNext && p.nxtC.vc % 2 != p.srcC.vc % 2 && p.nxtC.vc <= p.srcC.vc)
    }
    (mesh2DMinimal(nX, nY) && sel)(nodeId)(p)
  })

  def mesh2DDimensionOrderedHighest(nX: Int, nY: Int) = new RoutingRelation((nodeId, p) => {
    (if (p.nxtC.vc == 0) {
      mesh2DDimensionOrdered()(nX, nY)
    } else if (p.srcC.src == -1) {
      !nxtIsVC0 && mesh2DMinimal(nX, nY)
    } else {
      nxtVLESrcV && mesh2DMinimal(nX, nY)
    })(nodeId)(p)
  })

  def escapeChannels(escapeRouter: RoutingRelation, normalRouter: RoutingRelation, nEscapeChannels: Int = 1) = new RoutingRelation((nodeId, p) => {
    if (p.srcC.src == -1) {
      if (p.nxtC.vc >= nEscapeChannels) {
        normalRouter(nodeId)(p.copy(nxtC=p.nxtC.copy(vc=p.nxtC.vc-nEscapeChannels)))
      } else {
        escapeRouter(nodeId)(p)
      }
    } else if (p.srcC.vc < nEscapeChannels && p.nxtC.vc < nEscapeChannels) {
      escapeRouter(nodeId)(p)
    } else if (p.srcC.vc >= nEscapeChannels && p.nxtC.vc >= nEscapeChannels) {
      normalRouter(nodeId)(p.copy(srcC=p.srcC.copy(vc=p.srcC.vc-nEscapeChannels), nxtC=p.nxtC.copy(vc=p.nxtC.vc-nEscapeChannels)))
    } else if (p.srcC.vc >= nEscapeChannels && p.nxtC.vc < nEscapeChannels) {
      normalRouter(nodeId)(p.copy(srcC=p.srcC.copy(vc=p.srcC.vc-nEscapeChannels), nxtC=p.nxtC.copy(vc=0)))
    } else {
      false
    }
  })

  def mesh2DBestRouter(nX: Int, nY: Int) = escapeChannels(mesh2DDimensionOrdered()(nX, nY), mesh2DMinimal(nX, nY))

  def unidirectionalTorus2DDateline(nX: Int, nY: Int) = new RoutingRelation((nodeId, p) => {
    val (nxtX, nxtY)   = (p.nxtC.dst % nX , p.nxtC.dst / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (p.pInfo.dst % nX , p.pInfo.dst / nX)
    val (srcX, srcY)   = (p.srcC.src % nX , p.srcC.src / nX)

    val turn = nxtX != srcX && nxtY != srcY
    if (p.srcC.src == -1 || turn) {
      p.nxtC.vc != 0
    } else if (srcX == nxtX) {
      unidirectionalTorus1DDateline(nY)(nodeY)(p.copy(
        srcC=p.srcC.copy(src=srcY, dst=nodeY),
        nxtC=p.nxtC.copy(src=nodeY, dst=nxtY),
        pInfo=p.pInfo.copy(dst=dstY)
      ))
    } else if (srcY == nxtY) {
      unidirectionalTorus1DDateline(nX)(nodeX)(p.copy(
        srcC=p.srcC.copy(src=srcX, dst=nodeX),
        nxtC=p.nxtC.copy(src=nodeX, dst=nxtX),
        pInfo=p.pInfo.copy(dst=dstX)
      ))
    } else {
      false
    }
  })

  def bidirectionalTorus2DDateline(nX: Int, nY: Int) = new RoutingRelation((nodeId, p) => {
    val (nxtX, nxtY)   = (p.nxtC.dst % nX , p.nxtC.dst / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (p.pInfo.dst % nX , p.pInfo.dst / nX)
    val (srcX, srcY)   = (p.srcC.src % nX , p.srcC.src / nX)

    if (p.srcC.src == -1) {
      p.nxtC.vc != 0
    } else if (nodeX == nxtX) {
      bidirectionalTorus1DDateline(nY)(nodeY)(p.copy(
        srcC=p.srcC.copy(src=srcY, dst=nodeY),
        nxtC=p.nxtC.copy(src=nodeY, dst=nxtY),
        pInfo=p.pInfo.copy(dst=dstY)
      ))
    } else if (nodeY == nxtY) {
      bidirectionalTorus1DDateline(nX)(nodeX)(p.copy(
        srcC=p.srcC.copy(src=srcX, dst=nodeX),
        nxtC=p.nxtC.copy(src=nodeX, dst=nxtX),
        pInfo=p.pInfo.copy(dst=dstX)
      ))
    } else {
      false
    }
  })



  def dimensionOrderedUnidirectionalTorus2DDateline(nX: Int, nY: Int) = new RoutingRelation((nodeId, p) => {
    val (nxtX, nxtY)   = (p.nxtC.dst % nX , p.nxtC.dst / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (p.pInfo.dst % nX , p.pInfo.dst / nX)
    val (srcX, srcY)   = (p.srcC.src % nX , p.srcC.src / nX)

    def sel = if (dstX != nodeX) {
      nxtY == nodeY
    } else {
      nxtX == nodeX
    }
    (unidirectionalTorus2DDateline(nX, nY) && sel)(nodeId)(p)
  })

  def dimensionOrderedBidirectionalTorus2DDateline(nX: Int, nY: Int) = new RoutingRelation((nodeId, p) => {
    val (nxtX, nxtY)   = (p.nxtC.dst % nX , p.nxtC.dst / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (p.pInfo.dst % nX , p.pInfo.dst / nX)
    val (srcX, srcY)   = (p.srcC.src % nX , p.srcC.src / nX)

    val xdir = bidirectionalTorus1DShortest(nX)(nodeX)(p.copy(
      srcC=p.srcC.copy(src=(if (p.srcC.src == -1) -1 else srcX), dst=nodeX),
      nxtC=p.nxtC.copy(src=nodeX, dst=nxtX),
      pInfo=p.pInfo.copy(dst=dstX)
    ))
    val ydir = bidirectionalTorus1DShortest(nY)(nodeY)(p.copy(
      srcC=p.srcC.copy(src=(if (p.srcC.src == -1) -1 else srcY), dst=nodeY),
      nxtC=p.nxtC.copy(src=nodeY, dst=nxtY),
      pInfo=p.pInfo.copy(dst=dstY)
    ))

    val base = bidirectionalTorus2DDateline(nX, nY)(nodeId)(p)
    val sel = if (dstX != nodeX) xdir else ydir

    sel && base
  })


  // The below tables implement support for virtual subnetworks in a variety of ways
  // NOTE: The topology must have sufficient virtual channels for these to work correctly
  // TODO: Write assertions to check this

  // Independent virtual subnets with no resource sharing
  def nonblockingVirtualSubnetworks(f: RoutingRelation, n: Int) = new RoutingRelation((nodeId, p) => {
    (p.nxtC.vc % n == p.pInfo.vNet) && f(nodeId)(p.copy(
      srcC=p.srcC.copy(vc=p.srcC.vc / n),
      nxtC=p.nxtC.copy(vc=p.nxtC.vc / n),
      pInfo=p.pInfo.copy(vNet=0)
    ))
  })

  // Virtual subnets with 1 dedicated virtual channel each, and some number of shared channels
  def sharedNonblockingVirtualSubnetworks(f: RoutingRelation, n: Int, nSharedChannels: Int) = new RoutingRelation((nodeId, p) => {
    def trueVIdToVirtualVId(vId: Int) = if (vId < n) 0 else vId - n
    f(nodeId)(p.copy(
      srcC=p.srcC.copy(vc=trueVIdToVirtualVId(p.srcC.vc)),
      nxtC=p.nxtC.copy(vc=trueVIdToVirtualVId(p.nxtC.vc)),
      pInfo=p.pInfo.copy(vNet=0)
    ))
  })

  def blockingVirtualSubnetworks(f: RoutingRelation, n: Int) = new RoutingRelation((nodeId, p) => {
    val lNxtV = p.nxtC.vc - p.pInfo.vNet
    if (lNxtV < 0) {
      false
    } else {
      f(nodeId)(p.copy(nxtC=p.nxtC.copy(vc=lNxtV), pInfo=p.pInfo.copy(vNet=0)))
    }
  })
}
