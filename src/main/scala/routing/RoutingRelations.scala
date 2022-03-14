package constellation.routing

import scala.math.pow

/** Routing and channel allocation policy
 *
 * @param f function that takes in a nodeId, source channel, next channel, and packet routing info.
 *          Returns True if packet can acquire/proceed to this next channel, False if not. An example
 *          is the bidirectionalLine method; see its docstring.
 * @param isEscape function that takes in ChannelRoutingInfo and a virtual network ID. Returns True if the
 *                 channel represented by ChannelRoutingInfo is an escape channel and False if it is not.
 *                 By default, we ignore the escape-channel-based deadlock-free properties, and just check
 *                 for deadlock-freedom by verifying lack of a cyclic dependency.
 */
class RoutingRelation(
  f: (Int, ChannelRoutingInfo, ChannelRoutingInfo, PacketRoutingInfoInternal) => Boolean,
  val isEscape: (ChannelRoutingInfo, Int) => Boolean = (_,_) => true) {

  def apply(nodeId: Int, srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, pInfo: PacketRoutingInfo): Boolean = {
    apply(nodeId, srcC, nxtC, PacketRoutingInfoInternal(pInfo.dst, pInfo.vNet))
  }
  def apply(nodeId: Int, srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, pInfo: PacketRoutingInfoInternal): Boolean = {
    require(nodeId == srcC.dst && nodeId == nxtC.src)
    f(nodeId, srcC, nxtC, pInfo)
  }

  def unary_!()               = new RoutingRelation(
    (n, srcC, nxtC, pInfo) => !f(n, srcC, nxtC, pInfo),
    isEscape
  )
  def ||(a2: RoutingRelation) = new RoutingRelation(
    (n, srcC, nxtC, pInfo) => f(n, srcC, nxtC, pInfo) || a2(n, srcC, nxtC, pInfo),
    (c, v) => isEscape(c, v) || a2.isEscape(c, v)
  )
  def ||(a2: Boolean)         = new RoutingRelation(
    (n, srcC, nxtC, pInfo) => f(n, srcC, nxtC, pInfo) || a2,
    isEscape
  )
  def &&(a2: RoutingRelation) = new RoutingRelation(
    (n, srcC, nxtC, pInfo) => f(n, srcC, nxtC, pInfo) && a2(n, srcC, nxtC, pInfo),
    (c, v) => isEscape(c, v) || a2.isEscape(c, v)
  )
  def &&(a2: Boolean)         = new RoutingRelation(
    (n, srcC, nxtC, pInfo) => f(n, srcC, nxtC, pInfo) && a2,
    isEscape
  )
}


object RoutingRelation {

  /** Given a deadlock-prone routing relation and a routing relation representing the network's escape
   *  channels, returns a routing relation that adds the escape channels to the deadlock-prone relation.
   *
   *  @param escapeRouter routing relation representing the network's escape channels
   *  @param normalrouter deadlock-prone routing relation
   *  @param nEscapeChannels number of escape channels
   */
  def escapeChannels(escapeRouter: RoutingRelation, normalRouter: RoutingRelation, nEscapeChannels: Int = 1) = {
    new RoutingRelation((nodeId, srcC, nxtC, pInfo) => {
      if (srcC.src == -1) {
        if (nxtC.vc >= nEscapeChannels) { // if ingress and jumping into normal channel, use normal relation
          normalRouter(nodeId, srcC, nxtC.copy(vc=nxtC.vc-nEscapeChannels, n_vc=nxtC.n_vc-nEscapeChannels), pInfo)
        } else { // else use ingress and jump into escape channel
          escapeRouter(nodeId, srcC, nxtC.copy(n_vc=nEscapeChannels), pInfo)
        }
      } else if (nxtC.vc < nEscapeChannels) {
        escapeRouter(nodeId, srcC, nxtC.copy(n_vc=nEscapeChannels), pInfo)
      } else if (srcC.vc >= nEscapeChannels && nxtC.vc >= nEscapeChannels) {
        normalRouter(nodeId,
          srcC.copy(vc=srcC.vc-nEscapeChannels, n_vc=srcC.n_vc-nEscapeChannels),
          nxtC.copy(vc=nxtC.vc-nEscapeChannels, n_vc=nxtC.n_vc-nEscapeChannels),
          pInfo)
      } else {
        false
    }
    }, (c, v) => {
      c.isIngress || c.isEgress || c.vc < nEscapeChannels
    })
  }

  def noRoutingAtEgress = new RoutingRelation((nodeId, srcC, nxtC, pInfo) => pInfo.dst != nodeId)


  // Usable policies
  val allLegal = new RoutingRelation((nodeId, srcC, nxtC, pInfo) => true)

  /** An example of the parameter `f` for the RoutingRelation class that routes traffic along a
   * straight-line network in the direction of the packet's destination. Returns true if the selected
   * next channel moves the packet in the direction of the destination node and false if it does not.
   *
   * @param nodeId ID of the node the packet is currently at
   * @param srcC the source channel that brought the packet to nodeId
   * @param nxtC the channel proposed for the packet's next hop along the network
   * @param pInfo the packet's routing info, including its destination node
   */
  val bidirectionalLine = new RoutingRelation((nodeId, srcC, nxtC, pInfo) => {
    if (nodeId < nxtC.dst) pInfo.dst >= nxtC.dst else pInfo.dst <= nxtC.dst
  }) && noRoutingAtEgress

  def unidirectionalTorus1DDateline(nNodes: Int) = new RoutingRelation((nodeId, srcC, nxtC, pInfo) => {
    if (srcC.src == -1)  {
      nxtC.vc == nxtC.n_vc - 1
    } else if (srcC.vc == 0) {
      nxtC.vc == 0
    } else if (nodeId == nNodes - 1) {
      nxtC.vc < srcC.vc
    } else {
      nxtC.vc <= srcC.vc && nxtC.vc != 0
    }
  }) && noRoutingAtEgress



  def bidirectionalTorus1DDateline(nNodes: Int) = new RoutingRelation((nodeId, srcC, nxtC, pInfo) => {
    if (srcC.src == -1)  {
      nxtC.vc == nxtC.n_vc - 1
    } else if (srcC.vc == 0) {
      nxtC.vc == 0
    } else if ((nxtC.dst + nNodes - nodeId) % nNodes == 1) {
      if (nodeId == nNodes - 1) {
        nxtC.vc < srcC.vc
      } else {
        nxtC.vc <= srcC.vc && nxtC.vc != 0
      }
    } else if ((nodeId + nNodes - nxtC.dst) % nNodes == 1) {
      if (nodeId == 0) {
        nxtC.vc < srcC.vc
      } else {
        nxtC.vc <= srcC.vc && nxtC.vc != 0
      }
    } else {
      false
    }
  })

  def bidirectionalTorus1DShortest(nNodes: Int) = new RoutingRelation((nodeId, srcC, nxtC, pInfo) => {
    val cwDist = (pInfo.dst + nNodes - nodeId) % nNodes
    val ccwDist = (nodeId + nNodes - pInfo.dst) % nNodes
    val distSel = if (cwDist < ccwDist) {
      (nxtC.dst + nNodes - nodeId) % nNodes == 1
    } else if (cwDist > ccwDist) {
      (nodeId + nNodes - nxtC.dst) % nNodes == 1
    } else {
      true
    }
    distSel && bidirectionalTorus1DDateline(nNodes)(nodeId, srcC, nxtC, pInfo)
  }) && noRoutingAtEgress

  def bidirectionalTorus1DRandom(nNodes: Int) = new RoutingRelation((nodeId, srcC, nxtC, pInfo) => {
    val sel = if (srcC.src == -1) {
      true
    } else if ((nodeId + nNodes - srcC.src) % nNodes == 1) {
      (nxtC.dst + nNodes - nodeId) % nNodes == 1
    } else {
      (nodeId + nNodes - nxtC.dst) % nNodes == 1
    }
    sel && bidirectionalTorus1DDateline(nNodes)(nodeId, srcC, nxtC, pInfo)
  }) && noRoutingAtEgress

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

    new RoutingRelation((nodeId, srcC, nxtC, pInfo) => {
      val (nxtX, nxtY) = (nxtC.dst / height, nxtC.dst % height)
      val (nodeX, nodeY) = (nodeId / height, nodeId % height)
      val (dstX, dstY) = (pInfo.dst / height, pInfo.dst % height)
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
        dsts(nxtY).contains(pInfo.dst % height)
      }
    })
  }

  def bidirectionalTree(nNodes: Int) = {
    def canReach(src: Int, dst: Int) =
      if (src == dst) {
        true
      } else if (src > dst) {
        false
      } else {
        reachable(src * 2 + 1) || reachable(src * 2 + 2)
      }

    new RoutingRelation((nodeId, srcC, nxtC, pInfo) => {
        canReach(nxtC.dst, nodeId)
        // TODO (ANIMESH)
    })
  }



  def mesh2DDimensionOrdered(firstDim: Int = 0)(nX: Int, nY: Int) = new RoutingRelation((nodeId, srcC, nxtC, pInfo) => {
    val (nxtX, nxtY)   = (nxtC.dst % nX , nxtC.dst / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (pInfo.dst % nX , pInfo.dst / nX)

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
  def mesh2DMinimal(nX: Int, nY: Int) = new RoutingRelation((nodeId, srcC, nxtC, pInfo) => {
    val (nxtX, nxtY)   = (nxtC.dst % nX , nxtC.dst / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (pInfo.dst % nX, pInfo.dst / nX)

    val xR = (if (nodeX < nxtX) dstX >= nxtX else if (nodeX > nxtX) dstX <= nxtX else nodeX == nxtX)
    val yR = (if (nodeY < nxtY) dstY >= nxtY else if (nodeY > nxtY) dstY <= nxtY else nodeY == nxtY)
    xR && yR
  })


  def mesh2DWestFirst(nX: Int, nY: Int) = new RoutingRelation((nodeId, srcC, nxtC, pInfo) => {
    val (nxtX, nxtY)   = (nxtC.dst % nX , nxtC.dst / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (pInfo.dst % nX , pInfo.dst / nX)

    (if (dstX < nodeX) {
      new RoutingRelation((nodeId, srcC, nxtC, pInfo) => nxtX == nodeX - 1)
    } else {
      mesh2DMinimal(nX, nY)
    })(nodeId, srcC, nxtC, pInfo)
  })

  def mesh2DNorthLast(nX: Int, nY: Int) = new RoutingRelation((nodeId, srcC, nxtC, pInfo) => {
    val (nxtX, nxtY)   = (nxtC.dst % nX , nxtC.dst / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (pInfo.dst % nX , pInfo.dst / nX)

    (if (dstY > nodeY && dstX != nodeX) {
      mesh2DMinimal(nX, nY) && nxtY != nodeY + 1
    } else if (dstY > nodeY) {
      new RoutingRelation((nodeId, srcC, nxtC, pInfo) => nxtY == nodeY + 1)
    } else {
      mesh2DMinimal(nX, nY)
    })(nodeId, srcC, nxtC, pInfo)
  })



  def mesh2DAlternatingDimensionOrdered(nX: Int, nY: Int) = new RoutingRelation((nodeId, srcC, nxtC, pInfo) => {
    val (nxtX, nxtY)   = (nxtC.dst % nX , nxtC.dst / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (pInfo.dst % nX , pInfo.dst / nX)
    val (srcX, srcY)   = (srcC.src % nX , srcC.src / nX)

    val turn = nxtX != srcX && nxtY != srcY
    val canRouteThis = mesh2DDimensionOrdered(srcC.vc % 2)(nX, nY)
    val canRouteNext = mesh2DDimensionOrdered(nxtC.vc % 2)(nX, nY)

    val sel = if (srcC.src == -1) {
      canRouteNext
    } else {
      (canRouteThis && nxtC.vc % 2 == srcC.vc % 2 && nxtC.vc <= srcC.vc) || (canRouteNext && nxtC.vc % 2 != srcC.vc % 2 && nxtC.vc <= srcC.vc)
    }
    (mesh2DMinimal(nX, nY) && sel)(nodeId, srcC, nxtC, pInfo)
  }) && noRoutingAtEgress


  def mesh2DEscapeRouter(nX: Int, nY: Int) = escapeChannels(mesh2DDimensionOrdered()(nX, nY), mesh2DMinimal(nX, nY))

  def unidirectionalTorus2DDateline(nX: Int, nY: Int) = new RoutingRelation((nodeId, srcC, nxtC, pInfo) => {
    val (nxtX, nxtY)   = (nxtC.dst % nX , nxtC.dst / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (pInfo.dst % nX , pInfo.dst / nX)
    val (srcX, srcY)   = (srcC.src % nX , srcC.src / nX)

    val turn = nxtX != srcX && nxtY != srcY
    if (srcC.src == -1 || turn) {
      nxtC.vc == nxtC.n_vc - 1
    } else if (srcX == nxtX) {
      unidirectionalTorus1DDateline(nY)(
        nodeY,
        srcC.copy(src=srcY, dst=nodeY),
        nxtC.copy(src=nodeY, dst=nxtY),
        pInfo.copy(dst=dstY)
      )
    } else if (srcY == nxtY) {
      unidirectionalTorus1DDateline(nX)(
        nodeX,
        srcC.copy(src=srcX, dst=nodeX),
        nxtC.copy(src=nodeX, dst=nxtX),
        pInfo.copy(dst=dstX)
      )
    } else {
      false
    }
  })

  def bidirectionalTorus2DDateline(nX: Int, nY: Int) = new RoutingRelation((nodeId, srcC, nxtC, pInfo) => {
    val (nxtX, nxtY)   = (nxtC.dst % nX , nxtC.dst / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (pInfo.dst % nX , pInfo.dst / nX)
    val (srcX, srcY)   = (srcC.src % nX , srcC.src / nX)

    if (srcC.src == -1) {
      nxtC.vc == nxtC.n_vc - 1
    } else if (nodeX == nxtX) {
      bidirectionalTorus1DDateline(nY)(
        nodeY,
        srcC.copy(src=srcY, dst=nodeY),
        nxtC.copy(src=nodeY, dst=nxtY),
        pInfo.copy(dst=dstY)
      )
    } else if (nodeY == nxtY) {
      bidirectionalTorus1DDateline(nX)(
        nodeX,
        srcC.copy(src=srcX, dst=nodeX),
        nxtC.copy(src=nodeX, dst=nxtX),
        pInfo.copy(dst=dstX)
      )
    } else {
      false
    }
  })



  def dimensionOrderedUnidirectionalTorus2DDateline(nX: Int, nY: Int) = new RoutingRelation((nodeId, srcC, nxtC, pInfo) => {
    val (nxtX, nxtY)   = (nxtC.dst % nX , nxtC.dst / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (pInfo.dst % nX , pInfo.dst / nX)
    val (srcX, srcY)   = (srcC.src % nX , srcC.src / nX)

    def sel = if (dstX != nodeX) {
      nxtY == nodeY
    } else {
      nxtX == nodeX
    }
    (unidirectionalTorus2DDateline(nX, nY) && sel)(nodeId, srcC, nxtC, pInfo)
  })

  def dimensionOrderedBidirectionalTorus2DDateline(nX: Int, nY: Int) = new RoutingRelation((nodeId, srcC, nxtC, pInfo) => {
    val (nxtX, nxtY)   = (nxtC.dst % nX , nxtC.dst / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (pInfo.dst % nX , pInfo.dst / nX)
    val (srcX, srcY)   = (srcC.src % nX , srcC.src / nX)

    val xdir = bidirectionalTorus1DShortest(nX)(
      nodeX,
      srcC.copy(src=(if (srcC.src == -1) -1 else srcX), dst=nodeX),
      nxtC.copy(src=nodeX, dst=nxtX),
      pInfo.copy(dst=dstX)
    )
    val ydir = bidirectionalTorus1DShortest(nY)(
      nodeY,
      srcC.copy(src=(if (srcC.src == -1) -1 else srcY), dst=nodeY),
      nxtC.copy(src=nodeY, dst=nxtY),
      pInfo.copy(dst=dstY)
    )

    val base = bidirectionalTorus2DDateline(nX, nY)(nodeId, srcC, nxtC, pInfo)
    val sel = if (dstX != nodeX) xdir else ydir

    sel && base
  })


  // The below tables implement support for virtual subnetworks in a variety of ways
  // NOTE: The topology must have sufficient virtual channels for these to work correctly
  // TODO: Write assertions to check this

  /** Produces a system with support for n virtual subnetworks. The output routing relation ensures that
   * the virtual subnetworks do not block each other. This is accomplished by allocating a portion of the
   * virtual channels to each subnetwork; all physical resources are shared between virtual subnetworks
   * but virtual channels and buffers are not shared.
   */
  def nonblockingVirtualSubnetworks(f: RoutingRelation, n: Int) = new RoutingRelation((nodeId, srcC, nxtC, pInfo) => {
    if (srcC.isIngress) {
      (nxtC.vc % n == pInfo.vNet) && f(
        nodeId,
        srcC,
        nxtC.copy(vc=nxtC.vc / n, n_vc=nxtC.n_vc / n),
        pInfo.copy(vNet=0)
      )
    } else {
      // only allow a virtual subnet to use virtual channel is channelID % numnetworks = virtual network ID
      (nxtC.vc % n == pInfo.vNet) && f(
        nodeId,
        srcC.copy(vc=srcC.vc / n, n_vc=srcC.n_vc / n),
        nxtC.copy(vc=nxtC.vc / n, n_vc=nxtC.n_vc / n),
        pInfo.copy(vNet=0)
      )
    }
  }, (c, v) => {
    f.isEscape(c.copy(vc=c.vc / n, n_vc=c.n_vc / n), 0)
  })

  // Virtual subnets with 1 dedicated virtual channel each, and some number of shared channels
  def sharedNonblockingVirtualSubnetworks(f: RoutingRelation, n: Int, nSharedChannels: Int) = new RoutingRelation((nodeId, srcC, nxtC, pInfo) => {
    def trueVIdToVirtualVId(vId: Int) = if (vId < n) 0 else vId - n
    if (nxtC.vc < n) {
      nxtC.vc == pInfo.vNet && f(
        nodeId,
        srcC.copy(vc=trueVIdToVirtualVId(srcC.vc), n_vc = 1 + nSharedChannels),
        nxtC.copy(vc=0, n_vc = 1 + nSharedChannels),
        pInfo.copy(vNet=0)
      )
    } else {
      f(nodeId,
        srcC.copy(vc=trueVIdToVirtualVId(srcC.vc), n_vc = 1 + nSharedChannels),
        nxtC.copy(vc=trueVIdToVirtualVId(nxtC.vc), n_vc = 1 + nSharedChannels),
        pInfo.copy(vNet=0)
      )
    }
  }, (c, v) => {
    def trueVIdToVirtualVId(vId: Int) = if (vId < n) 0 else vId - n
    f.isEscape(c.copy(vc=trueVIdToVirtualVId(c.vc), n_vc = 1 + nSharedChannels), 0)
  })

  def blockingVirtualSubnetworks(f: RoutingRelation, n: Int, nDedicated: Int = 1) = new RoutingRelation((nodeId, srcC, nxtC, pInfo) => {
    val lNxtV = nxtC.vc - pInfo.vNet * nDedicated
    val lSrcV = srcC.vc - pInfo.vNet * nDedicated
    if (srcC.isIngress && lNxtV >= 0) {
      f(nodeId,
        srcC,
        nxtC.copy(n_vc = nxtC.n_vc - pInfo.vNet * nDedicated, vc = lNxtV),
        pInfo.copy(vNet=0)
      )
    } else if (lNxtV < 0 || lSrcV < 0) {
      false
    } else {
      f(nodeId,
        srcC.copy(n_vc = srcC.n_vc - pInfo.vNet * nDedicated, vc = lSrcV),
        nxtC.copy(n_vc = nxtC.n_vc - pInfo.vNet * nDedicated, vc = lNxtV),
        pInfo.copy(vNet=0)
      )
    }
  }, (c, v) => {
    c.vc >= v && f.isEscape(c.copy(vc=c.vc - v * nDedicated, n_vc=c.n_vc - v * nDedicated), 0)
  })
}
