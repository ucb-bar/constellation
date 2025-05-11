package constellation.routing

import scala.math.{min, max, pow}
import scala.collection.mutable.HashMap
import org.chipsalliance.cde.config.{Parameters}
import scala.collection.immutable.ListMap
import constellation.topology.{PhysicalTopology, Mesh2DLikePhysicalTopology, HierarchicalTopology, CustomTopology}

import scala.collection.mutable

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
// BEGIN: RoutingRelation
abstract class RoutingRelation(topo: PhysicalTopology) {
  // Child classes must implement these
  def rel       (srcC: ChannelRoutingInfo,
                 nxtC: ChannelRoutingInfo,
                 flow: FlowRoutingInfo): Boolean

  def isEscape  (c: ChannelRoutingInfo,
                 vNetId: Int): Boolean = true

  def getNPrios (src: ChannelRoutingInfo): Int = 1

  def getPrio   (srcC: ChannelRoutingInfo,
                 nxtC: ChannelRoutingInfo,
                 flow: FlowRoutingInfo): Int = 0

  // END: RoutingRelation

  private val memoize = new HashMap[(ChannelRoutingInfo, ChannelRoutingInfo, FlowRoutingInfo), Boolean]()
  def apply(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Boolean = {
    require(srcC.dst == nxtC.src)
    val key = (srcC, nxtC, flow)
    memoize.getOrElseUpdate(key, rel(srcC, nxtC, flow))
  }
}
// END: RoutingRelation

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
      val aEscape = isEscape(a, flow.vNetId)
      val nSrc = if (src.isIngress) {
        src
      } else if (isEscape(src, flow.vNetId)) {
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
      nxtC.dst <= flow.egressNode
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
      if (nxtC.src < nxtC.dst) flow.egressNode >= nxtC.dst else flow.egressNode <= nxtC.dst
    }
  }
}

object UnidirectionalTorus1DDatelineRouting {
  def apply() = (topo: PhysicalTopology) => new RoutingRelation(topo) {
    def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {
      if (srcC.src == -1)  {
        nxtC.vc != 0
      } else if (srcC.vc == 0) {
        nxtC.vc == 0
      } else if (nxtC.src == topo.nNodes - 1) {
        nxtC.vc < srcC.vc
      } else {
        nxtC.vc <= srcC.vc && (nxtC.vc != 0 || flow.egressNode > nxtC.src)
      }
    }
    override def getNPrios(src: ChannelRoutingInfo) = 2
    override def getPrio(src: ChannelRoutingInfo, nxt: ChannelRoutingInfo, flow: FlowRoutingInfo): Int = {
      if (nxt.vc == 0 || nxt.vc == 1) 1 else 0
    }
    override def isEscape(c: ChannelRoutingInfo, v: Int) = {
      c.isIngress || c.isEgress || c.vc < 2
    }
  }
}

object BidirectionalTorus1DDatelineRouting {
  def apply() = (topo: PhysicalTopology) => new RoutingRelation(topo) {
    def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {
      if (srcC.src == -1)  {
        nxtC.vc != 0
      } else if (srcC.vc == 0) {
        nxtC.vc == 0
      } else if ((nxtC.dst + topo.nNodes - nxtC.src) % topo.nNodes == 1) {
        if (nxtC.src == topo.nNodes - 1) {
          nxtC.vc < srcC.vc
        } else {
          nxtC.vc <= srcC.vc && (nxtC.vc != 0 || flow.egressNode > nxtC.src)
        }
      } else if ((nxtC.src + topo.nNodes - nxtC.dst) % topo.nNodes == 1) {
        if (nxtC.src == 0) {
          nxtC.vc < srcC.vc
        } else {
          nxtC.vc <= srcC.vc && (nxtC.vc != 0 || flow.egressNode < nxtC.src)
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
      val cwDist = (flow.egressNode + topo.nNodes - nxtC.src) % topo.nNodes
      val ccwDist = (nxtC.src + topo.nNodes - flow.egressNode) % topo.nNodes
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
        val (dstX, dstY) = (flow.egressNode / topo.height, flow.egressNode % topo.height)
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
          dsts(nxtY).contains(flow.egressNode % topo.height)
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
        if (isAncestor(nxtC.src, flow.egressNode)) {
          isAncestor(nxtC.dst, flow.egressNode) && (nxtC.dst >= nxtC.src)
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
        val (dstX, dstY)   = (flow.egressNode % topo.nX , flow.egressNode / topo.nX)

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
        val (dstX, dstY)   = (flow.egressNode % topo.nX , flow.egressNode / topo.nX)

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
        val (dstX, dstY)   = (flow.egressNode % topo.nX , flow.egressNode / topo.nX)

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
        val (dstX, dstY)   = (flow.egressNode % topo.nX , flow.egressNode / topo.nX)

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
        val (dstX, dstY)   = (flow.egressNode % topo.nX , flow.egressNode / topo.nX)
        val (srcX, srcY)   = (srcC.src % topo.nX , srcC.src / topo.nX)

        val turn = nxtX != srcX && nxtY != srcY
        if (srcC.src == -1 || turn) {
          nxtC.vc != 0
        } else if (srcX == nxtX) {
          baseY(
            srcC.copy(src=srcY, dst=nodeY),
            nxtC.copy(src=nodeY, dst=nxtY),
            flow.copy(egressNode=dstY)
          )
        } else if (srcY == nxtY) {
          baseX(
            srcC.copy(src=srcX, dst=nodeX),
            nxtC.copy(src=nodeX, dst=nxtX),
            flow.copy(egressNode=dstX)
          )
        } else {
          false
        }
      }
      override def isEscape(c: ChannelRoutingInfo, v: Int) = {
        c.isIngress || c.isEgress || c.vc < 2
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
        val (dstX, dstY)   = (flow.egressNode % topo.nX , flow.egressNode / topo.nX)
        val (srcX, srcY)   = (srcC.src % topo.nX , srcC.src / topo.nX)

        if (srcC.src == -1) {
          nxtC.vc != 0
        } else if (nodeX == nxtX) {
          baseY(
            srcC.copy(src=srcY, dst=nodeY),
            nxtC.copy(src=nodeY, dst=nxtY),
            flow.copy(egressNode=dstY)
          )
        } else if (nodeY == nxtY) {
          baseX(
            srcC.copy(src=srcX, dst=nodeX),
            nxtC.copy(src=nodeX, dst=nxtX),
            flow.copy(egressNode=dstX)
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
        val (dstX, dstY)   = (flow.egressNode % topo.nX , flow.egressNode / topo.nX)
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
        val (dstX, dstY)   = (flow.egressNode % topo.nX , flow.egressNode / topo.nX)
        val (srcX, srcY)   = (srcC.src % topo.nX , srcC.src / topo.nX)

        val xdir = baseX(
          srcC.copy(src=(if (srcC.src == -1) -1 else srcX), dst=nodeX),
          nxtC.copy(src=nodeX, dst=nxtX),
          flow.copy(egressNode=dstX)
        ) && nxtX != nodeX
        val ydir = baseY(
          srcC.copy(src=(if (srcC.src == -1) -1 else srcY), dst=nodeY),
          nxtC.copy(src=nodeY, dst=nxtY),
          flow.copy(egressNode=dstY)
        ) && nxtY != nodeY

        val sel = if (dstX != nodeX) xdir else ydir

        sel && base(srcC, nxtC, flow)
      }
    }
  }
}


object TerminalRouterRouting {
  def apply(baseRouting: PhysicalTopology => RoutingRelation) = (topo: PhysicalTopology) => topo match {
    case topo: constellation.topology.TerminalRouter => new RoutingRelation(topo) {
      val base = baseRouting(topo.base)
      def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {
        if (srcC.isIngress && topo.isBase(nxtC.dst)) {
          true
        } else if (nxtC.dst == flow.egressNode) {
          true
        } else if (topo.isBase(nxtC.src) &&
                   topo.isBase(nxtC.dst) &&
                   nxtC.src % topo.base.nNodes != flow.egressNode % topo.base.nNodes) {
          if (topo.isTerminal(srcC.src)) {
            base(
              srcC.copy(src=(-1), dst=srcC.dst % topo.base.nNodes, vc=0, n_vc=1),
              nxtC.copy(src=nxtC.src % topo.base.nNodes, dst=nxtC.dst % topo.base.nNodes),
              flow.copy(egressNode=flow.egressNode % topo.base.nNodes))
          } else {
            base(
              srcC.copy(src=srcC.src % topo.base.nNodes, dst=srcC.dst % topo.base.nNodes),
              nxtC.copy(src=nxtC.src % topo.base.nNodes, dst=nxtC.dst % topo.base.nNodes),
              flow.copy(egressNode=flow.egressNode % topo.base.nNodes))
          }
        } else {
          false
        }
      }
      override def getNPrios(c: ChannelRoutingInfo): Int = {
        if (topo.isBase(c.dst) && topo.isBase(c.src)){
          base.getNPrios(c.copy(src=c.src % topo.base.nNodes, dst=c.dst % topo.base.nNodes))
        } else {
          1
        }
      }
      override def getPrio(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Int = {
        if (topo.isBase(srcC.dst) && topo.isBase(nxtC.dst)) {
          if (topo.isTerminal(srcC.src)) {
            base.getPrio(
              srcC.copy(src=(-1)                       , dst=srcC.dst % topo.base.nNodes, vc=0, n_vc=1),
              nxtC.copy(src=nxtC.src % topo.base.nNodes, dst=nxtC.dst % topo.base.nNodes),
              flow.copy(egressNode=flow.egressNode % topo.base.nNodes))
          } else {
            base.getPrio(
              srcC.copy(src=srcC.src % topo.base.nNodes, dst=srcC.dst % topo.base.nNodes),
              nxtC.copy(src=nxtC.src % topo.base.nNodes, dst=nxtC.dst % topo.base.nNodes),
              flow.copy(egressNode=flow.egressNode % topo.base.nNodes))
          }
        } else {
          0
        }
      }

      override def isEscape(c: ChannelRoutingInfo, v: Int) = {
        !topo.isBase(c.src) || !topo.isBase(c.dst) || base.isEscape(
          c.copy(src=c.src % topo.base.nNodes, dst=c.dst % topo.base.nNodes), v)
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
  def apply(f: PhysicalTopology => RoutingRelation, n: Int, nDedicatedChannels: Int) = (topo: PhysicalTopology) => new RoutingRelation(topo) {
    def trueVIdToVirtualVId(vId: Int) = if (vId < n * nDedicatedChannels) {
      vId % nDedicatedChannels
    } else {
      nDedicatedChannels + vId - n * nDedicatedChannels
    }
    def lower(c: ChannelRoutingInfo) = c.copy(
      vc = trueVIdToVirtualVId(c.vc),
      n_vc = if (c.isIngress) 1 else c.n_vc - n * nDedicatedChannels + nDedicatedChannels
    )
    val base = f(topo)
    def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {
      val able = (nxtC.vc >= n * nDedicatedChannels) || ((nxtC.vc / nDedicatedChannels) == flow.vNetId)
      able && base(lower(srcC), lower(nxtC), flow.copy(vNetId=0))
    }
    override def isEscape(c: ChannelRoutingInfo, v: Int) = {
      base.isEscape(lower(c), 0)
    }
  }
}

object BlockingVirtualSubnetworksRouting {
  def apply(f: PhysicalTopology => RoutingRelation, n: Int, nDedicated: Int = 1) = (topo: PhysicalTopology) => new RoutingRelation(topo) {
    val base = f(topo)
    def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {
      val lNxtV = nxtC.vc - flow.vNetId * nDedicated
      val lSrcV = srcC.vc - flow.vNetId * nDedicated
      val r = if (srcC.isIngress && lNxtV >= 0) {
        base(srcC,
          nxtC.copy(n_vc = nxtC.n_vc - flow.vNetId * nDedicated, vc = lNxtV),
          flow.copy(vNetId=0)
        )
      } else if (lNxtV < 0 || lSrcV < 0) {
        false
      } else {
        base(srcC.copy(n_vc = srcC.n_vc - flow.vNetId * nDedicated, vc = lSrcV),
          nxtC.copy(n_vc = nxtC.n_vc - flow.vNetId * nDedicated, vc = lNxtV),
          flow.copy(vNetId=0)
        )
      }
      r
    }
    override def isEscape(c: ChannelRoutingInfo, v: Int) = {
      c.vc >= v * nDedicated && base.isEscape(c.copy(vc=c.vc - v * nDedicated, n_vc=c.n_vc - v * nDedicated), 0)
    }
  }
}


object HierarchicalRouting {
  def apply(
    baseRouting: PhysicalTopology => RoutingRelation,
    childRouting: Seq[PhysicalTopology => RoutingRelation]
  ) = (topo: PhysicalTopology) => topo match {
    case topo: HierarchicalTopology => new RoutingRelation(topo) {
      val base = baseRouting(topo.base)
      val children = (childRouting zip topo.children).map { case (l,r) => l(r.topo) }
      def copyChannel(c: ChannelRoutingInfo, o: Int) = {
        val nSrc = if (c.src < topo.base.nNodes) -1 else c.src - o
        val nDst = if (c.dst == -1) -1 else c.dst - o
        assert(nSrc >= -1 && nDst >= -1, s"$c $o")
        c.copy(src=nSrc, dst=nDst)
      }

      def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {
        val thisBase = topo.isBase(srcC.dst)
        val flowBase = topo.isBase(flow.egressNode)
        val nextBase = topo.isBase(nxtC.dst)
        (thisBase, flowBase, nextBase) match {
          case (true , true , true ) => base(
            srcC.copy(src=if (srcC.src >= topo.base.nNodes) -1 else srcC.src),
            nxtC, flow
          )
          case (true , true , false) => false
          case (true , false, true ) => {
            val src = topo.children(topo.childId(flow.egressNode)).src
            val atDst = src == srcC.dst
            !atDst && base(
              srcC.copy(src=if (srcC.src >= topo.base.nNodes) -1 else srcC.src),
              nxtC,
              flow.copy(egressNode=src)
            )
          }
          case (true , false, false) => topo.childId(nxtC.dst) == topo.childId(flow.egressNode)
          case (false, true , true ) => true
          case (false, true , false) => {
            val id = topo.childId(srcC.dst)
            children(id)(
              srcC=copyChannel(srcC, topo.offsets(id)),
              nxtC=copyChannel(nxtC, topo.offsets(id)),
              flow=flow.copy(egressNode=topo.children(id).dst)
            ) && topo.children(id).dst != srcC.dst - topo.offsets(id)
          }
          case (false, false, true ) => topo.childId(srcC.dst) != topo.childId(flow.egressNode)
          case (false, false, false) => {
            val fid = topo.childId(flow.egressNode)
            val sid = topo.childId(srcC.dst)
            val fdst = if (fid == sid) {
              flow.egressNode - topo.offsets(sid)
            } else {
              topo.children(sid).dst
            }
            val at_exit = (fid != sid &&
              (srcC.dst - topo.offsets(sid) == topo.children(sid).dst))
            children(sid)(
              srcC=copyChannel(srcC, topo.offsets(sid)),
              nxtC=copyChannel(nxtC, topo.offsets(sid)),
              flow=flow.copy(egressNode=fdst)
            ) && !at_exit
          }
        }
      }
      override def getNPrios(c: ChannelRoutingInfo): Int = {
        if (topo.isBase(c.dst) && topo.isBase(c.src)){
          base.getNPrios(c)
        } else if (topo.childId(c.src) == topo.childId(c.dst) && !c.isEgress) {
          children(topo.childId(c.src)).getNPrios(c.copy(
            src=if (c.src >= topo.base.nNodes) -1 else c.src))
        } else {
          1
        }
      }
      override def getPrio(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Int = {
        val thisBase = topo.isBase(srcC.dst)
        val nextBase = topo.isBase(nxtC.dst)
        val id = topo.childId(srcC.dst)
        (thisBase, nextBase) match {
          case (true, true) => base.getPrio(
            srcC.copy(src=if (srcC.src >= topo.base.nNodes) -1 else srcC.src),
            nxtC,
            flow.copy(egressNode=topo.children(topo.childId(flow.egressNode)).src))
          case (false, false) => {
            if (topo.childId(srcC.dst) == topo.childId(nxtC.dst)) {
              children(id).getPrio(
                copyChannel(srcC, topo.offsets(id)),
                copyChannel(nxtC, topo.offsets(id)),
                flow.copy(egressNode=topo.children(id).dst))
            } else {
              0
            }
          }
          case _ => 0
        }
      }

      override def isEscape(c: ChannelRoutingInfo, v: Int) = {
        val srcBase = topo.isBase(c.src)
        val dstBase = topo.isBase(c.dst)
          (srcBase, dstBase) match {
          case (true, true) => base.isEscape(c, v)
          case (true, false) => true
          case (false, true) => true
          case (false, false) => {
            val sid = topo.childId(c.dst)
            children(sid).isEscape(copyChannel(c, topo.offsets(sid)), v)
          }
        }
      }
    }
  }
}


/**
 * Pure shortest-path routing strategy with some VC-based load balancing.
 *
 * This routing relation computes single-path shortest routes between all node pairs
 * using BFS and restricts flow transitions to only those that follow the precomputed
 * shortest-path next hop. No deadlock avoidance mechanism is enforced.
 *
 * If multiple virtual channels (VCs) are available, we can assist the
 * PrioritizingVCAllocator by assigning priorities deterministically via a hashing 
 * of (flow.ingressNode, flow.egressNode) to help with better VC utilization. 
 * However VC transitions are not enforced and per-hop flows are allowed to use any VC 
 * as long as the edge is on the shortest path.
 *
 * This routing strategy is best for minimal-hop designs, and 
 * scenarios where higher-level deadlock prevention is handled 
 * externally such as through EscapeChannelRouting.
 *
 * maxVCs: The number of virtual channels available per physical channel.
 *         This must be >= 1. Defaults to 1 (no VC usage).
 * 
 */

object ShortestPathRouting {
  def apply(maxVCs: Int = 1) = (topo: PhysicalTopology) => new RoutingRelation(topo) {
    require(maxVCs >= 1, s"[ShortestPathRouting] maxVCs must be >= 1 (got $maxVCs)")

    // Build adjacency list
    val adjList: Map[Int, Seq[Int]] = (0 until topo.nNodes).map { src =>
      src -> (0 until topo.nNodes).filter(dst => topo.topo(src, dst))
    }.toMap

    // Precompute shortest-path next hops using BFS
    val nextHop: Map[(Int, Int), Int] = {
      val paths = for (src <- 0 until topo.nNodes; dst <- 0 until topo.nNodes if src != dst) yield {
        val next = bfsNextHop(src, dst)
        (src, dst) -> next
      }
      paths.collect { case ((s, d), Some(n)) => (s, d) -> n }.toMap
    }

    // BFS shortest-path helper
    private def bfsNextHop(src: Int, dst: Int): Option[Int] = {
      val visited = scala.collection.mutable.Set[Int]()
      val queue = scala.collection.mutable.Queue[(Int, List[Int])]()
      queue.enqueue((src, List()))
      while (queue.nonEmpty) {
        val (node, path) = queue.dequeue()
        if (node == dst) return path.headOption
        if (!visited.contains(node)) {
          visited += node
          adjList(node).foreach { neighbor =>
            queue.enqueue((neighbor, if (path.isEmpty) List(neighbor) else path))
          }
        }
      }
      None
    }

    def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Boolean = {
      val flowKey = (flow.ingressNode, flow.egressNode)

      if (srcC.src == -1) {
        // Ingress: only inject into first hop of shortest path
        val expected = nextHop.get((flow.ingressNode, flow.egressNode))
        val legal = nxtC.src == flow.ingressNode && expected.contains(nxtC.dst)
        return legal
      }

      if (srcC.dst == flow.egressNode) return false

      val expected = nextHop.get((srcC.dst, flow.egressNode))
      val legal = expected.contains(nxtC.dst)
      legal
    }

    // VC configuration
    override def getNPrios(src: ChannelRoutingInfo): Int = maxVCs

    override def getPrio(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Int = {
      (flow.ingressNode * 31 + flow.egressNode) % maxVCs
    }

    override def isEscape(c: ChannelRoutingInfo, vNetId: Int): Boolean = c.isIngress || c.isEgress

    println(s"[ShortestPathRouting] Initialized with maxVCs = $maxVCs")
  }
}


object EscapeIDOrderRouting {
  def apply() = (topo: PhysicalTopology) => new RoutingRelation(topo) {
    def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {
      if (srcC.src == -1) {
        true // allow ingress to inject anywhere
      } else {
        (srcC.dst < nxtC.dst) && topo.topo(srcC.dst, nxtC.dst)
      }
    }
  }
}

/*
 * CustomLayeredRouting: This is a a generalized deadlock-free routing policy for arbitrary
 * topologies. 
 * 
 * This routing relation assigns each flow (src, dst) a dedicated virtual channel (VC) layer
 * such that all paths in the same layer do not introduce cyclic dependencies. This is done by:
 *   1. Finding all shortest paths between flow pairs
 *   2. Deduplicating and pruning strict subpaths
 *   3. Packing flow paths into the minimum number of layers while preserving DAG constraints
 *   4. Assigning a VC layer to each flow based on its path
 * 
 * The rel() function enforces this policy by:
 *   - Ensuring packets flow only along the assigned VC
 *   - Ensuring only legal hops in the assigned flow path are taken
 *   - Ensuring the hop is within the assigned VC layer
 */

object CustomLayeredRouting {
  
  def apply() = (topo: PhysicalTopology) => topo match {
    case topo: CustomTopology => new RoutingRelation(topo) {
      val edgeList = topo.edgeList.map(e => (e.src, e.dst))
      val graph = new CustomGraph(topo.nNodes, edgeList)
      val allSSPs = graph.generateSSPs()

      val deduped = graph.deduplicateSSPs(allSSPs)
      val pruned = graph.pruneSubpaths(deduped)
      val packed = graph.packLayersMinimal(pruned)

      println(s"[CustomLayeredRouting] Required VCs: ${packed.length}")

      // VC assignment for all flows based on first matching layer
      val allFlowAssignments: Map[(Int, Int), Int] = allSSPs.map { case ((src, dst), path) =>
        val assignedLayer = packed.indexWhere(layer => path.toSet.subsetOf(layer.flatMap(_._2).toSet))
        ((src, dst), assignedLayer)
      }.toMap

      // Build nextHop map per VC (layer)
      val nextHopMap: Map[Int, Map[(Int, Int), Int]] = packed.zipWithIndex.map { case (layer, vc) =>
        val nhs = mutable.Map[(Int, Int), Int]()

        for (((src, dst), path) <- layer) {
          val nodes = path.map(_._1) :+ path.last._2
          for (i <- 0 until nodes.length - 1) {
            nhs((nodes(i), dst)) = nodes(i + 1)
          }
        }

        vc -> nhs.toMap
      }.toMap

      val flowPaths: Map[(Int, Int), List[(Int, Int)]] = allSSPs.toMap
      val vcToEdges = packed.map(_.flatMap(_._2).toSet)

      def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Boolean = {
        val flowKey = (flow.ingressNode, flow.egressNode)

        // Step 1: Lookup assigned VC
        val assignedVC = allFlowAssignments.getOrElse(flowKey, {
          return false
        })

        // Step 2: Lookup full path and verify layer correctness
        val flowPath = flowPaths.getOrElse(flowKey, {
          return false
        })

        val flowPathEdges = flowPath.toSet
        val assignedEdgeSet = vcToEdges(assignedVC)

        val fullPathOk = flowPathEdges.subsetOf(assignedEdgeSet)
        if (!fullPathOk) {
          return false
        }

        // Step 3: Current and next node
        val curNode  = if (srcC.src == -1) flow.ingressNode else srcC.dst
        val nextNode = nxtC.dst
        val edge     = (curNode, nextNode)

        // Step 4: Legal check
        val edgeOk = flowPathEdges.contains(edge)
        val vcOk   = nxtC.vc == assignedVC && (srcC.src == -1 || srcC.vc == assignedVC)

        val legal = edgeOk && vcOk

        legal
      }


      override def isEscape(c: ChannelRoutingInfo, v: Int): Boolean =
        c.isIngress || c.isEgress

      val guAssignedLayer = allFlowAssignments
      val nVirtualLayers = vcToEdges.length

      override def getNPrios(c: ChannelRoutingInfo): Int = nVirtualLayers

      override def getPrio(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Int = 0

    }
  }

}

/**
 * ShortestPathGeneralizedDatelineRouting implements a deadlock-free routing relation
 * based on identifying cycle-breaking datelines and the presence of virtual channels.
 *
 * This routing strategy:
 * 1. Analyzes the given topology using DatelineAnalyzer to:
 *    - Identify cycles in the channel dependency graph (CDG)
 *    - Select a minimal set of "dateline" edges to break those cycles
 *    - Compute the minimum number of virtual channels (VCs) needed to support
 *      all shortest paths without deadlock
 *
 * 2. Recomputes all-pairs shortest paths and annotates each hop with the
 *    required VC level. VC level increases only at dateline crossings,
 *    and remains constant otherwise. This leads to an enforced acyclic traversal 
 *    of the dependency graph.
 *
 * 3. rel()) only permits transitions which:
 *    - Follow the exact shortest path between (ingress, egress) nodes
 *    - Match the expected VC at each hop
 *    - Begin from the ingress using an assigned VC
 *        - If useStaticVC0 is true then injections are only allowed on VC 0
 *        - Otherwise there is a hashing scheme used to calculate the injection VC
 *
 */

object ShortestPathGeneralizedDatelineRouting {
  // def apply() = (topo: PhysicalTopology) => new RoutingRelation(topo) {
  def apply(
    useStaticVC0: Boolean = false,
    vcHashCoeffs: (Int, Int) = (19, 4)
  ) = (topo: PhysicalTopology) => new RoutingRelation(topo) {
    val result = DatelineAnalyzer.analyze(topo)
    val datelineEdges = result.datelineEdges
    val maxVC = result.vcCount
    val nextHop = result.nextHop

    // Recompute shortest paths and expected VC transitions
    val (shortestPaths, pathVCs): (Map[(Int, Int), List[Int]], Map[(Int, Int), List[Int]]) = {
      val g = new CustomGraph(topo.nNodes, (0 until topo.nNodes).flatMap(u => (0 until topo.nNodes).collect {
        case v if topo.topo(u, v) => (u, v)
      }))
      val sspPaths = g.generateSSPs()
      val pathMap = sspPaths.map { case ((src, dst), edges) => ((src, dst), src +: edges.map(_._2)) }.toMap
      val vcMap = pathMap.map { case ((src, dst), path) =>
        val vcs = if (useStaticVC0) {
          path.sliding(2).scanLeft(0) {
            case (vc, Seq(u, v)) =>
              if (datelineEdges.contains((u, v))) vc + 1 else vc
          }.toList
        } else {
          val (a, b) = vcHashCoeffs
          val baseVC = (src * a + dst * b) % maxVC 
          path.sliding(2).scanLeft(baseVC) {
            case (vc, Seq(u, v)) =>
              if (datelineEdges.contains((u, v))) (vc + 1) % maxVC else vc
          }.toList
        }
        ((src, dst), vcs)
      }
      (pathMap, vcMap)
    }

    println(s"[ShortestPathGeneralizedDatelineRouting] Initialized with ${datelineEdges.size} datelines and $maxVC VCs")

    def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Boolean = {

      val flowKey = (flow.ingressNode, flow.egressNode)

      val path = shortestPaths.getOrElse((flow.ingressNode, flow.egressNode), Nil)
      val vcs  = pathVCs.getOrElse((flow.ingressNode, flow.egressNode), Nil)

      if (srcC.src == -1) {
          val validInjectionEdge = path.headOption.contains(nxtC.src) && path.lift(1).contains(nxtC.dst)
          val expectedVC = vcs.lift(1).getOrElse(-1)
          val vcValid = nxtC.vc == expectedVC
          validInjectionEdge && vcValid
      } else if (flow.egressNode == srcC.dst) {
        false
      } else {
        val edge = (srcC.dst, nxtC.dst)
        val pathEdges = path.sliding(2).toList
        val idx = pathEdges.indexWhere { case Seq(a, b) => a == edge._1 && b == edge._2 }

        val validEdge = idx != -1
        val expectedVC = if (idx >= 0 && idx < vcs.length) vcs(idx + 1) else -1
        val vcValid = nxtC.vc == expectedVC

        validEdge && vcValid
      }
    }

    override def getNPrios(src: ChannelRoutingInfo): Int = {
      maxVC
    }

    override def getPrio(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Int = {
      // Priority is highest for lowest VC to prefer dateline avoidance
      maxVC - nxtC.vc
    }

    override def isEscape(c: ChannelRoutingInfo, vNetId: Int): Boolean = {
      c.isIngress || c.isEgress || c.vc == 0
    }
  }
}
