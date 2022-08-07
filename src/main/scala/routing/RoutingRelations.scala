package constellation.routing

import scala.math.{min, max, pow}
import scala.collection.mutable.HashMap
import freechips.rocketchip.config.{Parameters}
import scala.collection.immutable.ListMap
import constellation.topology.{PhysicalTopology, Mesh2DLikePhysicalTopology}

/** Routing and channel allocation policy
 *
 * @param f function that takes in source channel, next channel, and packet routing info.
 *          Returns True if packet can acquire/proceed to this next channel, False if not. An example
 *          is the bidirectionalLine method; see its docstring.
 * @param isEscape function that takes in ChannelRoutingInfo and a virtual network ID. Returns True if the
 *                 channel represented by ChannelRoutingInfo is an escape channel and False if it is not.
 *                 By default, we ignore the escape-channel-based deadlock-free properties, and just check
 *                 for deadlock-freedom by verifying lack of a cyclic dependency.
 */
abstract class RoutingRelation(topo: PhysicalTopology) {
  def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Boolean

  val memoize = new HashMap[(ChannelRoutingInfo, ChannelRoutingInfo, FlowRoutingInfo), Boolean]()
  def apply(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Boolean = {
    require(srcC.dst == nxtC.src)
    val key = (srcC, nxtC, flow)
    memoize.getOrElseUpdate(key, rel(srcC, nxtC, flow))
  }

  def isEscape(c: ChannelRoutingInfo, vNetId: Int): Boolean = true
  def getNPrios(src: ChannelRoutingInfo): Int = 1
  def getPrio(src: ChannelRoutingInfo, a: ChannelRoutingInfo, flow: FlowRoutingInfo): Int = 0
}

/** Given a deadlock-prone routing relation and a routing relation representing the network's escape
  *  channels, returns a routing relation that adds the escape channels to the deadlock-prone relation.
  *
  *  @param escapeRouter routing relation representing the network's escape channels
  *  @param normalrouter deadlock-prone routing relation
  *  @param nEscapeChannels number of escape channels
  */
object EscapeChannelRouting {
  def apply(
    escapeRouter: PhysicalTopology => RoutingRelation,
    normalRouter: PhysicalTopology => RoutingRelation,
    nEscapeChannels: Int = 1) = (topo: PhysicalTopology) => new RoutingRelation(topo) {
    val escape = escapeRouter(topo)
    val normal = normalRouter(topo)
    def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {
      if (srcC.src == -1) {
        if (nxtC.vc >= nEscapeChannels) { // if ingress and jumping into normal channel, use normal relation
          normal(srcC, nxtC.copy(vc=nxtC.vc-nEscapeChannels, n_vc=nxtC.n_vc-nEscapeChannels), flow)
        } else { // else use ingress and jump into escape channel
          escape(srcC, nxtC.copy(n_vc=nEscapeChannels), flow)
        }
      } else if (nxtC.vc < nEscapeChannels) {
        escape(srcC, nxtC.copy(n_vc=nEscapeChannels), flow)
      } else if (srcC.vc >= nEscapeChannels && nxtC.vc >= nEscapeChannels) {
        normal(
          srcC.copy(vc=srcC.vc-nEscapeChannels, n_vc=srcC.n_vc-nEscapeChannels),
          nxtC.copy(vc=nxtC.vc-nEscapeChannels, n_vc=nxtC.n_vc-nEscapeChannels),
          flow)
      } else {
        false
      }
    }
    override def isEscape(c: ChannelRoutingInfo, v: Int) = {
      c.isIngress || c.isEgress || c.vc < nEscapeChannels
    }
    override def getNPrios(c: ChannelRoutingInfo): Int = {
      escape.getNPrios(c) + normal.getNPrios(c)
    }
    override def getPrio(src: ChannelRoutingInfo, a: ChannelRoutingInfo, flow: FlowRoutingInfo): Int = {
      val aEscape = isEscape(a, flow.vNet)
      val nSrc = if (src.isIngress) {
        src
      } else if (isEscape(src, flow.vNet)) {
        src.copy(vc=src.vc, n_vc=nEscapeChannels)
      } else {
        src.copy(vc=src.vc-nEscapeChannels, n_vc=src.n_vc-nEscapeChannels)
      }
      if (aEscape) {
        escape.getPrio(nSrc, a.copy(n_vc=nEscapeChannels), flow) + normal.getNPrios(src)
      } else {
        normal.getPrio(nSrc, a.copy(vc=a.vc-nEscapeChannels, n_vc=a.n_vc-nEscapeChannels), flow)
      }
    }
  }
}

/**
  * Allows all routing. Probably not very useful
  */
object AllLegalRouting {
  def apply() = (topo: PhysicalTopology) => new RoutingRelation(topo) {
    def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = true
  }
}

object UnidirectionalLineRouting {
  def apply() = (topo: PhysicalTopology) => new RoutingRelation(topo) {
    def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {
      nxtC.dst <= flow.dst
    }
    override def getNPrios(src: ChannelRoutingInfo) = 4
    override def getPrio(src: ChannelRoutingInfo, a: ChannelRoutingInfo, flow: FlowRoutingInfo): Int = {
      max(src.dst - a.dst + getNPrios(src), 0)
    }
  }
}

object BidirectionalLineRouting {
  def apply() = (topo: PhysicalTopology) => new RoutingRelation(topo) {
    def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {
      if (nxtC.src < nxtC.dst) flow.dst >= nxtC.dst else flow.dst <= nxtC.dst
    }
  }
}

object UnidirectionalTorus1DDatelineRouting {
  def apply() = (topo: PhysicalTopology) => new RoutingRelation(topo) {
    def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {
      if (srcC.src == -1)  {
        nxtC.vc == nxtC.n_vc - 1
      } else if (srcC.vc == 0) {
        nxtC.vc == 0
      } else if (nxtC.src == topo.nNodes - 1) {
        nxtC.vc < srcC.vc
      } else {
        nxtC.vc <= srcC.vc && (nxtC.vc != 0 || flow.egressNode > nxtC.src)
      }
    }
  }
}

object BidirectionalTorus1DDatelineRouting {
  def apply() = (topo: PhysicalTopology) => new RoutingRelation(topo) {
    def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {
      if (srcC.src == -1)  {
        nxtC.vc == nxtC.n_vc - 1
      } else if (srcC.vc == 0) {
        nxtC.vc == 0
      } else if ((nxtC.dst + topo.nNodes - nxtC.src) % topo.nNodes == 1) {
        if (nxtC.src == topo.nNodes - 1) {
          nxtC.vc < srcC.vc
        } else {
          nxtC.vc <= srcC.vc && (nxtC.vc != 0 || flow.dst > nxtC.src)
        }
      } else if ((nxtC.src + topo.nNodes - nxtC.dst) % topo.nNodes == 1) {
        if (nxtC.src == 0) {
          nxtC.vc < srcC.vc
        } else {
          nxtC.vc <= srcC.vc && (nxtC.vc != 0 || flow.dst < nxtC.src)
        }
      } else {
        false
      }
    }
  }
}

object BidirectionalTorus1DShortestRouting {
  def apply() = (topo: PhysicalTopology) => new RoutingRelation(topo) {
    val base = BidirectionalTorus1DDatelineRouting()(topo)
    def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {
      val cwDist = (flow.dst + topo.nNodes - nxtC.src) % topo.nNodes
      val ccwDist = (nxtC.src + topo.nNodes - flow.dst) % topo.nNodes
      val distSel = if (cwDist < ccwDist) {
        (nxtC.dst + topo.nNodes - nxtC.src) % topo.nNodes == 1
      } else if (cwDist > ccwDist) {
        (nxtC.src + topo.nNodes - nxtC.dst) % topo.nNodes == 1
      } else {
        true
      }
      distSel && base(srcC, nxtC, flow)
    }
  }
}

object BidirectionalTorus1DRandomRouting {
  def apply() = (topo: PhysicalTopology) => new RoutingRelation(topo) {
    val base = BidirectionalTorus1DDatelineRouting()(topo)
    def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {
      val sel = if (srcC.src == -1) {
        true
      } else if ((nxtC.src + topo.nNodes - srcC.src) % topo.nNodes == 1) {
        (nxtC.dst + topo.nNodes - nxtC.src) % topo.nNodes == 1
      } else {
        (nxtC.src + topo.nNodes - nxtC.dst) % topo.nNodes == 1
      }
      sel && base(srcC, nxtC, flow)
    }
  }
}

object ButterflyRouting {
  def apply() = (topo: PhysicalTopology) => topo match {
    case topo: constellation.topology.Butterfly => new RoutingRelation(topo) {
      def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {
        val (nxtX, nxtY) = (nxtC.dst / topo.height, nxtC.dst % topo.height)
        val (nodeX, nodeY) = (nxtC.src / topo.height, nxtC.src % topo.height)
        val (dstX, dstY) = (flow.dst / topo.height, flow.dst % topo.height)
        if (dstX <= nodeX) {
          false
        } else if (nodeX == topo.nFly-1) {
          true
        } else {
          val dsts = (nxtX until topo.nFly-1).foldRight((0 until topo.height).map { i => Seq(i) }) {
            case (i,l) => (0 until topo.height).map { s => topo.channels(i).filter(_._1 == s).map { case (_,d) =>
              l(d)
            }.flatten }
          }
          dsts(nxtY).contains(flow.dst % topo.height)
        }
      }
    }
  }
}

object BidirectionalTreeRouting {
  def apply() = (topo: PhysicalTopology) => topo match {
    case topo: constellation.topology.BidirectionalTree => new RoutingRelation(topo) {
      /** Returns a boolean indicating whether src is an ancestor node of dst */
      def isAncestor(src: Int, dst: Int): Boolean = {
        if (src == dst) {
          true
        } else if (src > dst) {
          false
        } else {
          ((topo.dAry * src + 1) to (topo.dAry * src + topo.dAry)).foldLeft(false)((sofar, nextChild) => sofar || isAncestor(nextChild, dst))
        }
      }
      def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {
        if (isAncestor(nxtC.src, flow.dst)) {
          isAncestor(nxtC.dst, flow.dst) && (nxtC.dst >= nxtC.src)
        } else {
          isAncestor(nxtC.dst, nxtC.src)
        }
      }
    }
  }
}

object Mesh2DDimensionOrderedRouting {
  def apply(firstDim: Int = 0) = (topo: PhysicalTopology) => topo match {
    case topo: Mesh2DLikePhysicalTopology => new RoutingRelation(topo) {
      def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {
        val (nxtX, nxtY)   = (nxtC.dst % topo.nX , nxtC.dst / topo.nX)
        val (nodeX, nodeY) = (nxtC.src % topo.nX , nxtC.src / topo.nX)
        val (dstX, dstY)   = (flow.dst % topo.nX , flow.dst / topo.nX)

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
    }
  }
}

// WARNING: Not deadlock free
object Mesh2DMinimalRouting {
  def apply() = (topo: PhysicalTopology) => topo match {
    case topo: Mesh2DLikePhysicalTopology => new RoutingRelation(topo) {
      def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {
        val (nxtX, nxtY)   = (nxtC.dst % topo.nX , nxtC.dst / topo.nX)
        val (nodeX, nodeY) = (nxtC.src % topo.nX , nxtC.src / topo.nX)
        val (dstX, dstY)   = (flow.dst % topo.nX , flow.dst / topo.nX)

        val xR = (if (nodeX < nxtX) dstX >= nxtX else if (nodeX > nxtX) dstX <= nxtX else nodeX == nxtX)
        val yR = (if (nodeY < nxtY) dstY >= nxtY else if (nodeY > nxtY) dstY <= nxtY else nodeY == nxtY)
        xR && yR
      }
    }
  }
}

object Mesh2DWestFirstRouting {
  def apply() = (topo: PhysicalTopology) => topo match {
    case topo: Mesh2DLikePhysicalTopology => new RoutingRelation(topo) {
      val base = Mesh2DMinimalRouting()(topo)
      def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {
        val (nxtX, nxtY)   = (nxtC.dst % topo.nX , nxtC.dst / topo.nX)
        val (nodeX, nodeY) = (nxtC.src % topo.nX , nxtC.src / topo.nX)
        val (dstX, dstY)   = (flow.dst % topo.nX , flow.dst / topo.nX)

        if (dstX < nodeX) {
          nxtX == nodeX - 1
        } else {
          base(srcC, nxtC, flow)
        }
      }
    }
  }
}

object Mesh2DNorthLastRouting {
  def apply() = (topo: PhysicalTopology) => topo match {
    case topo: Mesh2DLikePhysicalTopology => new RoutingRelation(topo) {
      val base = Mesh2DMinimalRouting()(topo)
      def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {
        val (nxtX, nxtY)   = (nxtC.dst % topo.nX , nxtC.dst / topo.nX)
        val (nodeX, nodeY) = (nxtC.src % topo.nX , nxtC.src / topo.nX)
        val (dstX, dstY)   = (flow.dst % topo.nX , flow.dst / topo.nX)

        val minimal = base(srcC, nxtC, flow)
        if (dstY > nodeY && dstX != nodeX) {
          minimal && nxtY != nodeY + 1
        } else if (dstY > nodeY) {
          nxtY == nodeY + 1
        } else {
          minimal
        }
      }
    }
  }
}

object Mesh2DEscapeRouting {
  def apply() = (topo: PhysicalTopology) => topo match {
    case topo: Mesh2DLikePhysicalTopology => EscapeChannelRouting(
      escapeRouter=Mesh2DDimensionOrderedRouting(),
      normalRouter=Mesh2DMinimalRouting())(topo)
  }
}

object UnidirectionalTorus2DDatelineRouting {
  def apply() = (topo: PhysicalTopology) => topo match {
    case topo: Mesh2DLikePhysicalTopology => new RoutingRelation(topo) {
      val baseX = UnidirectionalTorus1DDatelineRouting()(constellation.topology.UnidirectionalTorus1D(topo.nX))
      val baseY = UnidirectionalTorus1DDatelineRouting()(constellation.topology.UnidirectionalTorus1D(topo.nY))
      def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {

        val (nxtX, nxtY)   = (nxtC.dst % topo.nX , nxtC.dst / topo.nX)
        val (nodeX, nodeY) = (nxtC.src % topo.nX , nxtC.src / topo.nX)
        val (dstX, dstY)   = (flow.dst % topo.nX , flow.dst / topo.nX)
        val (srcX, srcY)   = (srcC.src % topo.nX , srcC.src / topo.nX)

        val turn = nxtX != srcX && nxtY != srcY
        if (srcC.src == -1 || turn) {
          nxtC.vc == nxtC.n_vc - 1
        } else if (srcX == nxtX) {
          baseY(
            srcC.copy(src=srcY, dst=nodeY),
            nxtC.copy(src=nodeY, dst=nxtY),
            flow.copy(dst=dstY)
          )
        } else if (srcY == nxtY) {
          baseX(
            srcC.copy(src=srcX, dst=nodeX),
            nxtC.copy(src=nodeX, dst=nxtX),
            flow.copy(dst=dstX)
          )
        } else {
          false
        }
      }
    }
  }
}

object BidirectionalTorus2DDatelineRouting {
  def apply() = (topo: PhysicalTopology) => topo match {
    case topo: Mesh2DLikePhysicalTopology => new RoutingRelation(topo) {
      val baseX = UnidirectionalTorus1DDatelineRouting()(constellation.topology.UnidirectionalTorus1D(topo.nX))
      val baseY = UnidirectionalTorus1DDatelineRouting()(constellation.topology.UnidirectionalTorus1D(topo.nY))
      def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {
        val (nxtX, nxtY)   = (nxtC.dst % topo.nX , nxtC.dst / topo.nX)
        val (nodeX, nodeY) = (nxtC.src % topo.nX , nxtC.src / topo.nX)
        val (dstX, dstY)   = (flow.dst % topo.nX , flow.dst / topo.nX)
        val (srcX, srcY)   = (srcC.src % topo.nX , srcC.src / topo.nX)

        if (srcC.src == -1) {
          nxtC.vc == nxtC.n_vc - 1
        } else if (nodeX == nxtX) {
          baseY(
            srcC.copy(src=srcY, dst=nodeY),
            nxtC.copy(src=nodeY, dst=nxtY),
            flow.copy(dst=dstY)
          )
        } else if (nodeY == nxtY) {
          baseX(
            srcC.copy(src=srcX, dst=nodeX),
            nxtC.copy(src=nodeX, dst=nxtX),
            flow.copy(dst=dstX)
          )
        } else {
          false
        }
      }
    }
  }
}

object DimensionOrderedUnidirectionalTorus2DDatelineRouting {
  def apply() = (topo: PhysicalTopology) => topo match {
    case topo: Mesh2DLikePhysicalTopology => new RoutingRelation(topo) {
      val base = UnidirectionalTorus2DDatelineRouting()(topo)
      def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {
        val (nxtX, nxtY)   = (nxtC.dst % topo.nX , nxtC.dst / topo.nX)
        val (nodeX, nodeY) = (nxtC.src % topo.nX , nxtC.src / topo.nX)
        val (dstX, dstY)   = (flow.dst % topo.nX , flow.dst / topo.nX)
        val (srcX, srcY)   = (srcC.src % topo.nX , srcC.src / topo.nX)

        def sel = if (dstX != nodeX) {
          nxtY == nodeY
        } else {
          nxtX == nodeX
        }
        base(srcC, nxtC, flow) && sel
      }
    }
  }
}

object DimensionOrderedBidirectionalTorus2DDatelineRouting {
  def apply() = (topo: PhysicalTopology) => topo match {
    case topo: Mesh2DLikePhysicalTopology => new RoutingRelation(topo) {
      val baseX = BidirectionalTorus1DShortestRouting()(constellation.topology.BidirectionalTorus1D(topo.nX))
      val baseY = BidirectionalTorus1DShortestRouting()(constellation.topology.BidirectionalTorus1D(topo.nY))
      val base = BidirectionalTorus2DDatelineRouting()(topo)
      def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {

        val (nxtX, nxtY)   = (nxtC.dst % topo.nX , nxtC.dst / topo.nX)
        val (nodeX, nodeY) = (nxtC.src % topo.nX , nxtC.src / topo.nX)
        val (dstX, dstY)   = (flow.dst % topo.nX , flow.dst / topo.nX)
        val (srcX, srcY)   = (srcC.src % topo.nX , srcC.src / topo.nX)

        val xdir = baseX(
          srcC.copy(src=(if (srcC.src == -1) -1 else srcX), dst=nodeX),
          nxtC.copy(src=nodeX, dst=nxtX),
          flow.copy(dst=dstX)
        )
        val ydir = baseY(
          srcC.copy(src=(if (srcC.src == -1) -1 else srcY), dst=nodeY),
          nxtC.copy(src=nodeY, dst=nxtY),
          flow.copy(dst=dstY)
        )

        val sel = if (dstX != nodeX) xdir else ydir

        sel && base(srcC, nxtC, flow)
      }
    }
  }
}


object TerminalPlaneRouting {
  def apply(baseRouting: PhysicalTopology => RoutingRelation) = (topo: PhysicalTopology) => topo match {
    case topo: constellation.topology.TerminalPlane => new RoutingRelation(topo) {
      val base = baseRouting(topo.base)
      def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {
        if (srcC.isIngress && nxtC.dst < topo.base.nNodes) {
          true
        } else if (nxtC.dst == flow.dst) {
          true
        } else if (nxtC.src < topo.base.nNodes && nxtC.dst < topo.base.nNodes && nxtC.src != flow.dst - 2 * topo.base.nNodes) {
          if (srcC.src >= topo.base.nNodes) {
            base(srcC.copy(src=(-1), vc=0, n_vc=1),
              nxtC,
              flow.copy(dst=flow.dst-2*topo.base.nNodes))
          } else {
            base(srcC,
              nxtC,
              flow.copy(dst=flow.dst-2*topo.base.nNodes))
          }
        } else {
          false
        }
      }
      override def isEscape(c: ChannelRoutingInfo, v: Int) = {
        c.src >= topo.base.nNodes || c.dst >= topo.base.nNodes || base.isEscape(c, v)
      }
    }
  }
}


// The below tables implement support for virtual subnetworks in a variety of ways
// NOTE: The topology must have sufficient virtual channels for these to work correctly
// TODO: Write assertions to check this

/** Produces a system with support for n virtual subnetworks. The output routing relation ensures that
  * the virtual subnetworks do not block each other. This is accomplished by allocating a portion of the
  * virtual channels to each subnetwork; all physical resources are shared between virtual subnetworks
  * but virtual channels and buffers are not shared.
  */
object NonblockingVirtualSubnetworksRouting {
  def apply(f: PhysicalTopology => RoutingRelation, n: Int) = (topo: PhysicalTopology) => new RoutingRelation(topo) {
    val base = f(topo)
    def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {
      if (srcC.isIngress) {
        (nxtC.vc % n == flow.vNet) && base(
          srcC,
          nxtC.copy(vc=nxtC.vc / n, n_vc=nxtC.n_vc / n),
          flow.copy(vNet=0)
        )
      } else {
        // only allow a virtual subnet to use virtual channel is channelID % numnetworks = virtual network ID
        (nxtC.vc % n == flow.vNet) && base(
          srcC.copy(vc=srcC.vc / n, n_vc=srcC.n_vc / n),
          nxtC.copy(vc=nxtC.vc / n, n_vc=nxtC.n_vc / n),
          flow.copy(vNet=0)
        )
      }
    }
    override def isEscape(c: ChannelRoutingInfo, v: Int) = {
      base.isEscape(c.copy(vc=c.vc / n, n_vc=c.n_vc / n), 0)
    }
  }
}

// Virtual subnets with 1 dedicated virtual channel each, and some number of shared channels
object SharedNonblockingVirtualSubnetworksRouting {
  def apply(f: PhysicalTopology => RoutingRelation, n: Int, nSharedChannels: Int) = (topo: PhysicalTopology) => new RoutingRelation(topo) {
    def trueVIdToVirtualVId(vId: Int) = if (vId < n) 0 else vId - n +1
    val base = f(topo)
    def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {
      if (nxtC.vc < n) {
        nxtC.vc == flow.vNet && base(
          srcC.copy(vc=trueVIdToVirtualVId(srcC.vc), n_vc = 1 + nSharedChannels),
          nxtC.copy(vc=0, n_vc = 1 + nSharedChannels),
          flow.copy(vNet=0)
        )
      } else {
        base(srcC.copy(vc=trueVIdToVirtualVId(srcC.vc), n_vc = 1 + nSharedChannels),
          nxtC.copy(vc=trueVIdToVirtualVId(nxtC.vc), n_vc = 1 + nSharedChannels),
          flow.copy(vNet=0)
        )
      }
    }
    override def isEscape(c: ChannelRoutingInfo, v: Int) = {
      base.isEscape(c.copy(vc=trueVIdToVirtualVId(c.vc), n_vc = 1 + nSharedChannels), 0)
    }
  }
}

object BlockingVirtualSubnetworksRouting {
  def apply(f: PhysicalTopology => RoutingRelation, n: Int, nDedicated: Int = 1) = (topo: PhysicalTopology) => new RoutingRelation(topo) {
    val base = f(topo)
    def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {
      val lNxtV = nxtC.vc - flow.vNet * nDedicated
      val lSrcV = srcC.vc - flow.vNet * nDedicated
      if (srcC.isIngress && lNxtV >= 0) {
        base(srcC,
          nxtC.copy(n_vc = nxtC.n_vc - flow.vNet * nDedicated, vc = lNxtV),
          flow.copy(vNet=0)
        )
      } else if (lNxtV < 0 || lSrcV < 0) {
        false
      } else {
        base(srcC.copy(n_vc = srcC.n_vc - flow.vNet * nDedicated, vc = lSrcV),
          nxtC.copy(n_vc = nxtC.n_vc - flow.vNet * nDedicated, vc = lNxtV),
          flow.copy(vNet=0)
        )
      }
    }
    override def isEscape(c: ChannelRoutingInfo, v: Int) = {
      c.vc >= v && base.isEscape(c.copy(vc=c.vc - v * nDedicated, n_vc=c.n_vc - v * nDedicated), 0)
    }
  }
}
