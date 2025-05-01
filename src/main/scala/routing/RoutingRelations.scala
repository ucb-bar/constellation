package constellation.routing

import scala.math.{min, max, pow}
import scala.collection.mutable.HashMap
import org.chipsalliance.cde.config.{Parameters}
import scala.collection.immutable.ListMap
import constellation.topology.{PhysicalTopology, Mesh2DLikePhysicalTopology, HierarchicalTopology, CustomTopology}

import scala.collection.mutable
import scala.collection.mutable.{HashMap, HashSet, Queue}
import constellation.topology.TopologyEdge
import scala.collection.mutable.{Set => MSet, Map => MMap}

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


// Shortest path routing + Dateline escape priority
object ShortestPathRouting {
  def apply() = (topo: PhysicalTopology) => new RoutingRelation(topo) {
    // Build adjacency list
    val adjList: Map[Int, Seq[Int]] = (0 until topo.nNodes).map { src =>
      src -> (0 until topo.nNodes).filter(dst => topo.topo(src, dst))
    }.toMap

    // Precompute next hops
    val nextHop: Map[(Int, Int), Int] = {
      val paths = for (src <- 0 until topo.nNodes; dst <- 0 until topo.nNodes if src != dst) yield {
        val next = bfsNextHop(src, dst)
        (src, dst) -> next
      }
      paths.collect { case ((s,d), Some(n)) => (s,d) -> n }.toMap
    }

    // Helper: BFS to find next hop
    private def bfsNextHop(src: Int, dst: Int): Option[Int] = {
      val visited = scala.collection.mutable.Set[Int]()
      val queue = scala.collection.mutable.Queue[(Int, List[Int])]()
      queue.enqueue((src, List()))
      while (queue.nonEmpty) {
        val (node, path) = queue.dequeue()
        if (node == dst) {
          return path.headOption
        }
        if (!visited.contains(node)) {
          visited += node
          adjList(node).foreach { neighbor =>
            queue.enqueue((neighbor, if (path.isEmpty) List(neighbor) else path))
          }
        }
      }
      None
    }

    // Dateline definition
    val dateline = topo.nNodes / 2
    def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Boolean = {
      if (srcC.src == -1) {
        true
      } else if (flow.egressNode == srcC.dst) {
        false // Already at destination
      } else {
        // Must follow shortest path
        val correctNext = nextHop.get((srcC.dst, flow.egressNode)).contains(nxtC.dst)

        // Must use correct VC if crossing dateline
        val needsEscape = crossesDateline(srcC.dst, nxtC.dst)
        val correctVC = if (needsEscape) {
          nxtC.vc == 0 // escape VC
        } else {
          nxtC.vc != 0 // normal VC
        }

        correctNext && correctVC
      }
    }


    // VC Priority logic
    override def getNPrios(src: ChannelRoutingInfo): Int = 2 // Two VCs: escape VC and normal VC

    override def getPrio(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Int = {
      if (srcC.src == -1) {
        1
      } else if (crossesDateline(srcC.dst, nxtC.dst)) {
        0
      } else {
        1
      }
    }

    override def isEscape(c: ChannelRoutingInfo, vNetId: Int): Boolean = {
      c.isIngress || c.isEgress || c.vc == 0
    }

    // Define dateline crossing
    private def crossesDateline(src: Int, dst: Int): Boolean = {
      (src < dateline && dst >= dateline) || (src >= dateline && dst < dateline)
    }

    // Debug output for verification
    println("[Constellation] === ShortestPathRouting Debug Output ===")
    for (src <- 0 until topo.nNodes) {
      for (dst <- 0 until topo.nNodes if src != dst) {
        val path = buildFullPath(src, dst)
        if (path.nonEmpty) {
          println(s"[Path] $src -> $dst : ${path.mkString(" -> ")}")
        } else {
          println(s"[WARNING] No path from $src -> $dst")
        }
      }
    }
    println("[Constellation] === End ofShortestPathRouting Debug Output ===")

    // Helper to build full path
    private def buildFullPath(src: Int, dst: Int): Seq[Int] = {
      var path = Seq(src)
      var current = src
      while (current != dst) {
        nextHop.get((current, dst)) match {
          case Some(nxt) =>
            path = path :+ nxt
            current = nxt
          case None =>
            return Seq()
        }
      }
      path
    }
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

object SpanningTreeRouting {
  def apply(root: Int = 0, debugPrint: Boolean = true) = (topo: PhysicalTopology) => new RoutingRelation(topo) {

    // Step 1: Build a BFS Spanning Tree based only on available edges
    val parent = mutable.Map[Int, Int]()
    val treeEdges = mutable.Set[(Int, Int)]()
    val visited = mutable.Set[Int]()
    val queue = mutable.Queue[Int]()

    queue.enqueue(root)
    visited += root

    while (queue.nonEmpty) {
      val current = queue.dequeue()
      for (neighbor <- 0 until topo.nNodes if topo.topo(current, neighbor) && !visited.contains(neighbor)) {
        // Only follow existing edges
        parent(neighbor) = current
        treeEdges += ((current, neighbor))
        visited += neighbor
        queue.enqueue(neighbor)
      }
    }

    // Check full connectivity
    if (visited.size != topo.nNodes) {
      throw new RuntimeException(s"Spanning tree could not reach all nodes! Visited ${visited.size}/${topo.nNodes} nodes.")
    }

    // Optionally print the tree
    if (debugPrint) {
      println("[SpanningTreeRouting] === Tree Edges ===")
      treeEdges.foreach { case (src, dst) => println(s"  $src -> $dst") }
      println("[SpanningTreeRouting] === End ===")
    }

    // Step 2: Routing function
    def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Boolean = {
      if (srcC.src == -1) {
        // Ingress packet: can move to any starting node (as long as it's part of the tree)
        true
      } else {
        val from = srcC.dst
        val to = nxtC.dst

        // Allowed if the move is along a tree edge (either parent → child or child → parent)
        treeEdges.contains((from, to)) || treeEdges.contains((to, from))
      }
    }
  }
}

object EscapeDAGRouting {
  def apply(root: Int = 0) = (topo: PhysicalTopology) => new RoutingRelation(topo) {
    val nNodes = topo.nNodes

    // Build a DAG
    val dagEdges: Set[(Int, Int)] = {
      val visited = scala.collection.mutable.Set[Int]()
      val edges = scala.collection.mutable.Set[(Int, Int)]()
      val queue = scala.collection.mutable.Queue[Int]()

      queue.enqueue(root)
      visited += root

      while (queue.nonEmpty) {
        val dst = queue.dequeue()
        for (src <- 0 until nNodes if topo.topo(src, dst)) {
          if (!visited.contains(src)) {
            edges += ((src, dst)) // src --> dst (directed toward root)
            visited += src
            queue.enqueue(src)
          }
        }
      }

      edges.toSet
    }

    println("[EscapeDAGRouting] === DAG Edges ===")
    dagEdges.foreach { case (src, dst) => println(s"  $src -> $dst") }
    println("[EscapeDAGRouting] === End ===")

    // Routing rule: allow moving if following DAG edge
    def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Boolean = {
      srcC.src == -1 || dagEdges.contains((srcC.dst, nxtC.dst))
    }
  }
}

class UpDownRoutingRelation(topo: PhysicalTopology, root: Int = 0) extends RoutingRelation(topo) {
  // Maps a node to its parent in the spanning tree
  val parent = mutable.Map[Int, Int]()
  val level = mutable.Map[Int, Int]()
  val visited = mutable.Set[Int]()
  val queue = mutable.Queue[Int]()

  // Build the spanning tree (BFS)
  {
    queue.enqueue(root)
    level(root) = 0
    visited += root

    while (queue.nonEmpty) {
      val current = queue.dequeue()
      for (neighbor <- 0 until topo.nNodes if topo.topo(current, neighbor) && !visited.contains(neighbor)) {
        parent(neighbor) = current
        level(neighbor) = level(current) + 1
        visited += neighbor
        queue.enqueue(neighbor)
      }
    }

    // Important sanity check: Must connect the whole graph
    if (visited.size != topo.nNodes) {
      throw new RuntimeException(s"Spanning tree could not reach all nodes! Only visited ${visited.size}/${topo.nNodes}")
    }
  }

  // Function to determine if an edge is an "up" or "down" edge
  def isUpEdge(src: Int, dst: Int): Boolean = {
    // Higher level (farther from root) --> Lower level (closer to root) == up
    level(src) > level(dst)
  }

  // Core routing relation: Can we transition from srcC to nxtC for a packet?
  def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Boolean = {
    if (srcC.src == -1) {
      true
    } else {
      require(srcC.dst == nxtC.src, "Invalid transition: Channels must be adjacent")

      val srcNode = srcC.src
      val midNode = srcC.dst
      val nxtNode = nxtC.dst

      val srcToMidIsUp = isUpEdge(srcC.src, srcC.dst)
      val midToNxtIsUp = isUpEdge(nxtC.src, nxtC.dst)

      // Figure out if the flow (from flow.src to flow.dst) needs to keep going up or can start going down
      val goingUp = level(flow.ingressNode) > level(flow.egressNode)

      if (goingUp) {
        // Haven't reached downward yet: can take up edges freely
        if (midToNxtIsUp) {
          true // Keep going up
        } else {
          // Only allow first down move if midNode is an ancestor of dst
          isAncestor(midNode, flow.egressNode)
        }
      } else {
        // Already supposed to be going downward — only allow down edges
        !midToNxtIsUp
      }
    }
  }

  // Helper to check if ancestor relationship holds
  def isAncestor(ancestor: Int, node: Int): Boolean = {
    var current = node
    while (current != root && parent.contains(current)) {
      if (current == ancestor) return true
      current = parent(current)
    }
    current == ancestor
  }
}


object ShortestPathDatelineRouting {

  case class Edge(from: Int, to: Int)
  case class DepEdge(from: (Int, Int), to: (Int, Int)) // (nodeId, vcId) → (nodeId, vcId)

  // Helper to find any cycle (in DepEdges)
  def findAnyCycle(edges: mutable.Set[DepEdge]): Option[List[DepEdge]] = {
    val outgoing = mutable.Map[(Int, Int), mutable.Set[(Int, Int)]]().withDefaultValue(mutable.Set())
    edges.foreach(e => outgoing(e.from) += e.to)

    val visited = mutable.Set[(Int, Int)]()
    var cycleFound: Option[List[DepEdge]] = None

    def dfs(u: (Int, Int), path: List[DepEdge]): Unit = {
      if (cycleFound.nonEmpty) return

      visited += u
      for (v <- outgoing(u)) {
        if (cycleFound.nonEmpty) return
        if (!visited(v)) {
          dfs(v, DepEdge(u, v) :: path)
        } else {
          // Found a back edge → cycle
          cycleFound = Some(DepEdge(u, v) :: path)
        }
      }
      visited -= u
    }

    for (start <- outgoing.keys if cycleFound.isEmpty) {
      dfs(start, Nil)
    }

    cycleFound
  }

  def apply() = (topo: PhysicalTopology) => new RoutingRelation(topo) {

    // -------------------------
    // Step 1: Build all shortest paths (first hops via BFS)
    // -------------------------
    val nextHop = Array.fill(topo.nNodes, topo.nNodes)(-1)

    for (start <- 0 until topo.nNodes) {
      val visited = mutable.Set[Int]()
      val queue = mutable.Queue[Int]()
      visited += start
      queue.enqueue(start)

      while (queue.nonEmpty) {
        val current = queue.dequeue()
        for (neighbor <- 0 until topo.nNodes if topo.topo(current, neighbor)) {
          if (!visited.contains(neighbor)) {
            visited += neighbor
            queue.enqueue(neighbor)
            if (nextHop(start)(neighbor) == -1) {
              if (current == start) nextHop(start)(neighbor) = neighbor
              else nextHop(start)(neighbor) = nextHop(start)(current)
            }
          }
        }
      }
    }

    println("[ShortestPathDatelineRouting] === Shortest Paths (First Hops) ===")
    for (i <- 0 until topo.nNodes) {
      for (j <- 0 until topo.nNodes if i != j) {
        println(s"  $i -> $j : first hop ${nextHop(i)(j)}")
      }
    }
    println("[ShortestPathDatelineRouting] === End Shortest Paths ===")

    // -------------------------
    // Step 2: Build basic dependency edges (edges between nodes)
    // -------------------------
    val basicEdges = mutable.Set[Edge]()
    for (src <- 0 until topo.nNodes; dst <- 0 until topo.nNodes if topo.topo(src, dst)) {
      basicEdges += Edge(src, dst)
    }


    // -------------------------
    // Step 3: Find SCCs
    // -------------------------
    val edgesFrom = mutable.Map[Int, mutable.Set[Int]]().withDefaultValue(mutable.Set())
    basicEdges.foreach(e => edgesFrom(e.from) += e.to)

    var index = 0
    val indices = mutable.Map[Int, Int]()
    val lowlinks = mutable.Map[Int, Int]()
    val onStack = mutable.Set[Int]()
    val stack = mutable.Stack[Int]()
    val sccs = mutable.ArrayBuffer[Set[Int]]()

    def strongConnect(v: Int): Unit = {
      indices(v) = index
      lowlinks(v) = index
      index += 1
      stack.push(v)
      onStack += v

      for (w <- edgesFrom(v)) {
        if (!indices.contains(w)) {
          strongConnect(w)
          lowlinks(v) = math.min(lowlinks(v), lowlinks(w))
        } else if (onStack(w)) {
          lowlinks(v) = math.min(lowlinks(v), indices(w))
        }
      }

      if (lowlinks(v) == indices(v)) {
        val scc = mutable.Set[Int]()
        var w = -1
        do {
          w = stack.pop()
          onStack -= w
          scc += w
        } while (w != v)
        if (scc.size > 1) {
          sccs += scc.toSet
        }
      }
    }

    for (v <- 0 until topo.nNodes if !indices.contains(v)) {
      strongConnect(v)
    }

    println("[ShortestPathDatelineRouting] === Detected SCCs ===")
    for ((scc, idx) <- sccs.zipWithIndex) {
      println(s"  SCC #$idx: Nodes ${scc.mkString(",")}")
    }
    println("[ShortestPathDatelineRouting] === End SCCs ===")

    // -------------------------
    // Step 4: Greedy dateline selection (for node-level graph)
    // -------------------------
    val datelineEdges = mutable.Set[Edge]()
    for (scc <- sccs) {
      val subgraph = basicEdges.filter(e => scc.contains(e.from) && scc.contains(e.to)).to(mutable.Set)
      var done = false

      while (!done) {
        val simpleEdges = subgraph.map(e => DepEdge((e.from, 0), (e.to, 0)))
        val cycle = findAnyCycle(simpleEdges)
        if (cycle.isEmpty) {
          done = true
        } else {
          val e = cycle.get.head
          println(s"[Dateline] Breaking cycle: ${cycle.get.map(e => s"${e.from}→${e.to}").mkString(" → ")}")
          datelineEdges += Edge(e.from._1, e.to._1)
          println(s"[ShortestPathDatelineRouting] Inserted dateline on edge ${e.from._1} -> ${e.to._1}")
          subgraph -= Edge(e.from._1, e.to._1)
        }
      }
    }

    println("[ShortestPathDatelineRouting] === Total Datelines ===")
    datelineEdges.foreach(e => println(s"  ${e.from} -> ${e.to}"))
    println("[ShortestPathDatelineRouting] === End Datelines ===")

    // -------------------------
    // Step 5: Build full dependency graph (modeling VC transitions)
    // -------------------------
    val fullDependencyEdges = mutable.Set[DepEdge]()
    for (src <- 0 until topo.nNodes; dst <- 0 until topo.nNodes if src != dst) {
      var current = src
      var vc = 0
      while (current != dst && nextHop(current)(dst) != -1) {
        val next = nextHop(current)(dst)
        val isDateline = datelineEdges.contains(Edge(current, next))
        val nextVc = if (isDateline) vc + 1 else vc

        fullDependencyEdges += DepEdge((current, vc), (next, nextVc))

        current = next
        vc = nextVc
      }
    }

    // -------------------------
    // Step 6: Routing Relation Functions
    // -------------------------
    override def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Boolean = {
      if (srcC.src == -1) true
      else {
        val mid = srcC.dst
        val next = nxtC.dst
        next == nextHop(mid)(flow.egressNode)
      }
    }

    override def getNPrios(src: ChannelRoutingInfo): Int = 2 // 2 VCs: normal and after dateline

    override def getPrio(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Int = {
      if (srcC.src == -1) {
        0
      } else {
        val mid = srcC.dst
        val next = nxtC.dst

        val srcVC = srcC.vc - flow.vNetId * getNPrios(srcC)
        val nextVC = nxtC.vc - flow.vNetId * getNPrios(nxtC)

        val isDateline = datelineEdges.contains(Edge(mid, next))

        if (isDateline) {
          1 // upgrade from VC0 to VC1
        } else {
          srcVC
        }
      }
    }

    override def isEscape(c: ChannelRoutingInfo, vNetId: Int): Boolean = true
  }
}

// object CustomLASHRouting {
//   def apply(maxVCs: Int = 4) = (topo: PhysicalTopology) => new RoutingRelation(topo) {
//     val n = topo.nNodes

//     // Step 1: Build all shortest paths
//     val allPaths = mutable.Map[(Int, Int), Seq[Int]]()
//     for {
//       src <- 0 until n
//       dst <- 0 until n if src != dst
//     } {
//       val path = shortestPath(topo, src, dst)
//       allPaths((src, dst)) = path
//     }

//     // Step 2: Build dependency edges for all paths
//     val depEdges = allPaths.values.flatMap { path =>
//       path.sliding(2).map { case Seq(a, b) => (a, b) }
//     }.toSet

//     // Step 3: Assign to layers
//     val virtualLayers = Array.fill(maxVCs)(mutable.Set[(Int, Int)]())
//     var remainingEdges = depEdges

//     for (layerIdx <- 0 until maxVCs if remainingEdges.nonEmpty) {
//       var added = true
//       while (added) {
//         added = false
//         for ((u, v) <- remainingEdges.toList) {
//           if (!introducesCycle(virtualLayers(layerIdx) + ((u, v)))) {
//             virtualLayers(layerIdx) += ((u, v))
//             remainingEdges -= ((u, v))
//             added = true
//           }
//         }
//       }
//     }

//     if (remainingEdges.nonEmpty) {
//       throw new Exception(s"Insufficient VCs for deadlock-free routing. Remaining edges: ${remainingEdges.size}")
//     }

//     override def rel(srcC: ChannelRoutingInfo, dstC: ChannelRoutingInfo, flow: FlowRoutingInfo): Boolean = {
//       if (srcC.dst == -1) {
//         true
//       } else {
//         (0 until maxVCs).exists { i =>
//           virtualLayers(i).contains((srcC.dst, dstC.dst))
//         }
//       }
//     }

//     // === Helper functions ===

//     def shortestPath(topo: PhysicalTopology, src: Int, dst: Int): Seq[Int] = {
//       val prev = Array.fill(topo.nNodes)(-1)
//       val visited = Array.fill(topo.nNodes)(false)
//       val queue = mutable.Queue[Int]()

//       queue.enqueue(src)
//       visited(src) = true

//       while (queue.nonEmpty) {
//         val current = queue.dequeue()
//         if (current == dst) {
//           var path = List[Int](dst)
//           while (prev(path.head) != -1) {
//             path = prev(path.head) :: path
//           }
//           return path
//         }

//         for (neighbor <- 0 until topo.nNodes if topo.topo(current, neighbor) && !visited(neighbor)) {
//           queue.enqueue(neighbor)
//           visited(neighbor) = true
//           prev(neighbor) = current
//         }
//       }

//       throw new RuntimeException(s"No path found from $src to $dst")
//     }

//     def introducesCycle(edges: Iterable[(Int, Int)]): Boolean = {
//       val graph = edges.groupBy(_._1).mapValues(_.map(_._2).toSet).toMap.withDefaultValue(Set())
//       val visited = mutable.Set[Int]()
//       val recStack = mutable.Set[Int]()

//       def dfs(node: Int): Boolean = {
//         if (recStack.contains(node)) return true
//         if (visited.contains(node)) return false

//         visited += node
//         recStack += node
//         for (neighbor <- graph(node)) {
//           if (dfs(neighbor)) return true
//         }
//         recStack -= node
//         false
//       }

//       val nodes = edges.flatMap { case (u, v) => Seq(u, v) }.toSet
//       for (node <- nodes) {
//         if (!visited.contains(node)) {
//           if (dfs(node)) return true
//         }
//       }
//       false
//     }
//   }
// }


// object CustomLASHRouting {
//   def apply() = (topo: PhysicalTopology) => new RoutingRelation(topo) {

//     // --- Step 1: Compute shortest paths between all nodes ---
//     val shortestPaths: Map[(Int, Int), Seq[Int]] = {
//       val paths = HashMap[(Int, Int), Seq[Int]]()
//       for (src <- 0 until topo.nNodes) {
//         val dist = Array.fill(topo.nNodes)(Int.MaxValue)
//         val prev = Array.fill(topo.nNodes)(-1)
//         val visited = Array.fill(topo.nNodes)(false)
//         dist(src) = 0

//         val q = Queue[Int](src)
//         while (q.nonEmpty) {
//           val u = q.dequeue()
//           visited(u) = true
//           for (v <- 0 until topo.nNodes if topo.topo(u, v)) {
//             if (dist(v) > dist(u) + 1) {
//               dist(v) = dist(u) + 1
//               prev(v) = u
//               q.enqueue(v)
//             }
//           }
//         }

//         for (dst <- 0 until topo.nNodes if src != dst && dist(dst) != Int.MaxValue) {
//           var path = List[Int](dst)
//           var curr = dst
//           while (prev(curr) != -1) {
//             curr = prev(curr)
//             path = curr +: path
//           }
//           paths((src, dst)) = path
//         }
//       }
//       paths.toMap
//     }

//     println(s"[CustomLASH] Computed shortest paths for ${shortestPaths.size} pairs")

//     // --- Step 2: Group (src,dst) pairs into GUs (one GU per source node) ---
//     val granularUnits: Map[Int, Set[(Int, Int)]] = {
//       val gu = HashMap[Int, mutable.Set[(Int, Int)]]()
//       for (((src, dst), _) <- shortestPaths) {
//         gu.getOrElseUpdate(src, mutable.Set()) += ((src, dst))
//       }
//       gu.mapValues(_.toSet).toMap
//     }

//     println(s"[CustomLASH] Grouped into ${granularUnits.size} granular units")

//     // --- Step 3: Assign GUs to Virtual Layers ---
//     val guAssignedLayer = HashMap[(Int, Int), Int]()
//     val channelDepsPerLayer = mutable.ArrayBuffer[mutable.Set[(Int, Int)]]()

//     def closesCycle(deps: Set[(Int, Int)], newDeps: Set[(Int, Int)]): Boolean = {
//       val fullDeps = deps ++ newDeps
//       def hasCycle: Boolean = {
//         val adj = HashMap[Int, Set[Int]]().withDefaultValue(Set())
//         for ((from, to) <- fullDeps) adj(from) += to

//         def dfs(u: Int, visited: Set[Int], recStack: Set[Int]): Boolean = {
//           if (recStack.contains(u)) return true
//           if (visited.contains(u)) return false
//           adj(u).exists(v => dfs(v, visited + u, recStack + u))
//         }

//         (0 until topo.nNodes).exists(u => dfs(u, Set(), Set()))
//       }
//       hasCycle
//     }

//     var currentLayers = 0
//     for ((src, guPaths) <- granularUnits) {
//       var placed = false
//       val guDeps = guPaths.flatMap { case (s, d) =>
//         val path = shortestPaths((s, d))
//         val deps = path.sliding(2).map { case Seq(a, b) => (a, b) }.toSet
//         println(s"[CustomLASH] GU ($s->$d) path: ${path.mkString("->")}, deps: ${deps.mkString(", ")}")
//         deps
//       }
//       for (i <- 0 until currentLayers if !placed) {
//         if (!closesCycle(channelDepsPerLayer(i).toSet, guDeps)) {
//           guPaths.foreach(p => guAssignedLayer(p) = i)
//           channelDepsPerLayer(i) ++= guDeps
//           placed = true
//         }
//       }
//       if (!placed) {
//         channelDepsPerLayer += mutable.Set[(Int, Int)]()
//         guPaths.foreach(p => guAssignedLayer(p) = currentLayers)
//         channelDepsPerLayer(currentLayers) ++= guDeps
//         currentLayers += 1
//       }
//     }

//     println(s"[CustomLASH] Assigned GUs into $currentLayers virtual layers")
//     for (layer <- 0 until currentLayers) {
//       val pathsInLayer = guAssignedLayer.filter(_._2 == layer).keys
//       val pretty = pathsInLayer.map { case (s, d) => s"$s→$d" }.mkString(", ")
//       println(s"[CustomLASH] Layer $layer: $pretty")
//     }

//     val nVirtualLayers = currentLayers

//     // --- Step 4: Define the RoutingRelation behavior ---
//     def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Boolean = {
//       if (srcC.dst != nxtC.src) {
//         println(s"[CustomLASH][DENY] Bad connectivity: ${srcC.dst} != ${nxtC.src}")
//         return false
//       }

//       val pathOpt = shortestPaths.get((flow.ingressNode, flow.egressNode))
//       if (pathOpt.isEmpty) {
//         println(s"[CustomLASH][DENY] No path from ${flow.ingressNode} to ${flow.egressNode}")
//         return false
//       }

//       val path = pathOpt.get
//       val idx = path.indexOf(srcC.dst)
//       if (idx == -1 || idx + 1 >= path.length) {
//         println(s"[CustomLASH][DENY] ${srcC.dst} not in valid path position in ${path.mkString("->")}")
//         return false
//       }

//       val expectedNext = path(idx + 1)
//       val assignedLayer = guAssignedLayer.getOrElse((flow.ingressNode, flow.egressNode), -1)
//       if (assignedLayer == -1) {
//         println(s"[CustomLASH][DENY] No layer assigned to flow ${flow.ingressNode}->${flow.egressNode}")
//         return false
//       }

//       val sameVC = nxtC.vc == assignedLayer
//       val allowed = nxtC.dst == expectedNext && sameVC
//       println(s"[CustomLASH][${if (allowed) "ALLOW" else "DENY"}] ${srcC.dst}->${nxtC.dst} | flow ${flow.ingressNode}->${flow.egressNode}, vc=${nxtC.vc}, layer=$assignedLayer")
//       allowed
//     }

//     override def getNPrios(src: ChannelRoutingInfo): Int = nVirtualLayers
//     override def getPrio(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Int = 0
//   }
// }


object CustomDeadlockFreeRouting {
  def apply(nVirtualSubnets: Int = 5, vcsPerSubnet: Int = 2) = (topo: PhysicalTopology) => new RoutingRelation(topo) {
    val n = topo.nNodes
    val root = 0
    require(vcsPerSubnet >= 2, "Each subnet must have at least 2 VCs (Up and Down)")

    // Step 1: Build a BFS tree rooted at node 0
    val parent = Array.fill(n)(-1)
    val depth = Array.fill(n)(-1)
    val visited = Array.fill(n)(false)
    val queue = mutable.Queue[Int](root)
    depth(root) = 0
    visited(root) = true

    while (queue.nonEmpty) {
      val node = queue.dequeue()
      for (neighbor <- 0 until n if topo.topo(node, neighbor) && !visited(neighbor)) {
        visited(neighbor) = true
        parent(neighbor) = node
        depth(neighbor) = depth(node) + 1
        queue.enqueue(neighbor)
      }
    }

    // Step 2: Classify each edge as Up or Down
    val edgeDirection = mutable.Map[(Int, Int), String]()
    for (a <- 0 until n; b <- 0 until n if topo.topo(a, b)) {
      if (depth(a) < depth(b)) edgeDirection((a, b)) = "Down"
      else if (depth(a) > depth(b)) edgeDirection((a, b)) = "Up"
      else edgeDirection((a, b)) = "Flat"
    }

    println("[CustomDeadlockFreeRouting] Edge directions:")
    edgeDirection.foreach { case ((a, b), dir) => println(s"  $a -> $b : $dir") }

    def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Boolean = {
      if (srcC.dst != nxtC.src) return false

      val trueEdge = (nxtC.src, nxtC.dst)
      val direction = edgeDirection.getOrElse(trueEdge, "Invalid")
      val baseVC = flow.vNetId * vcsPerSubnet

      if (srcC.isIngress) {
        // If coming from ingress, allow entering directly into Down VC
        direction match {
          case "Up" => nxtC.vc == baseVC
          case "Down" => nxtC.vc == baseVC + 1
          case _ => false
        }
      } else {
        // Normal node-to-node movement
        direction match {
          case "Up" => nxtC.vc == baseVC
          case "Down" => (srcC.vc == baseVC) && (nxtC.vc == baseVC + 1)
          case _ => false
        }
      }
    }


    override def isEscape(c: ChannelRoutingInfo, vNetId: Int): Boolean = {
      c.isIngress || c.isEgress
    }

    override def getNPrios(src: ChannelRoutingInfo): Int = 1

    override def getPrio(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Int = {
      0
    }
  }
} 


object CustomLASHRouting {
  def apply(numSubnetworks: Int = 5, vcsPerSubnet: Int = 2) = (topo: PhysicalTopology) => new RoutingRelation(topo) {
    val n = topo.nNodes

    // We use one LASH layer per subnetwork (vNetId), up to numSubnetworks
    val virtualLayers = Array.fill(numSubnetworks)(mutable.Set[(Int, Int)]())

    // Step 1: Build all shortest paths
    val allPaths = mutable.Map[(Int, Int), Seq[Int]]()
    for {
      src <- 0 until n
      dst <- 0 until n if src != dst
    } {
      val path = shortestPath(topo, src, dst)
      allPaths((src, dst)) = path
    }

    // Step 2: Build dependency edges for all paths
    val depEdges = allPaths.values.flatMap { path =>
      path.sliding(2).map { case Seq(a, b) => (a, b) }
    }.toSet

    // Step 3: Assign to virtual subnet layers (one layer per vNetId)
    var remainingEdges = depEdges

    for (layerIdx <- 0 until numSubnetworks if remainingEdges.nonEmpty) {
      var added = true
      while (added) {
        added = false
        for ((u, v) <- remainingEdges.toList) {
          if (!introducesCycle(virtualLayers(layerIdx) + ((u, v)))) {
            virtualLayers(layerIdx) += ((u, v))
            remainingEdges -= ((u, v))
            added = true
          }
        }
      }
    }

    if (remainingEdges.nonEmpty) {
      throw new Exception(s"Insufficient virtual subnetworks for deadlock-free routing. Remaining edges: ${remainingEdges.size}")
    }

    override def rel(srcC: ChannelRoutingInfo, dstC: ChannelRoutingInfo, flow: FlowRoutingInfo): Boolean = {
      if (srcC.dst == -1) {
        true
      } else {
        val layerIdx = flow.vNetId
        if (layerIdx >= virtualLayers.length) false
        else virtualLayers(layerIdx).contains((srcC.dst, dstC.dst))
      }
    }

    // === Helper functions ===

    def shortestPath(topo: PhysicalTopology, src: Int, dst: Int): Seq[Int] = {
      val prev = Array.fill(topo.nNodes)(-1)
      val visited = Array.fill(topo.nNodes)(false)
      val queue = mutable.Queue[Int]()

      queue.enqueue(src)
      visited(src) = true

      while (queue.nonEmpty) {
        val current = queue.dequeue()
        if (current == dst) {
          var path = List[Int](dst)
          while (prev(path.head) != -1) {
            path = prev(path.head) :: path
          }
          return path
        }

        for (neighbor <- 0 until topo.nNodes if topo.topo(current, neighbor) && !visited(neighbor)) {
          queue.enqueue(neighbor)
          visited(neighbor) = true
          prev(neighbor) = current
        }
      }

      throw new RuntimeException(s"No path found from $src to $dst")
    }

    def introducesCycle(edges: Iterable[(Int, Int)]): Boolean = {
      val graph = edges.groupBy(_._1).mapValues(_.map(_._2).toSet).toMap.withDefaultValue(Set())
      val visited = mutable.Set[Int]()
      val recStack = mutable.Set[Int]()

      def dfs(node: Int): Boolean = {
        if (recStack.contains(node)) return true
        if (visited.contains(node)) return false

        visited += node
        recStack += node
        for (neighbor <- graph(node)) {
          if (dfs(neighbor)) return true
        }
        recStack -= node
        false
      }

      val nodes = edges.flatMap { case (u, v) => Seq(u, v) }.toSet
      for (node <- nodes) {
        if (!visited.contains(node)) {
          if (dfs(node)) return true
        }
      }
      false
    }
  }
}

// object CustomLayeredRouting {
//   def apply() = (topo: PhysicalTopology) => topo match {
//     case topo: CustomTopology => new RoutingRelation(topo) {
//       val edgeList = topo.edgeList.map(e => (e.src, e.dst))
//       val graph = new CustomGraph(topo.nNodes, edgeList)
//       val allSSPs = graph.generateSSPs()

//       allSSPs.foreach { case ((src, dst), path) =>
//         println(s"[DEBUG] Path from $src to $dst: ${path.mkString("->")}")
//       }

//       val deduped = graph.deduplicateSSPs(allSSPs)
//       val pruned = graph.pruneSubpaths(deduped)
//       val packed = graph.packLayersMinimal(pruned)

//       packed.zipWithIndex.foreach { case (layer, i) =>
//         val edgeSet = layer.flatMap(_._2).toSet
//         println(s"[CustomLayeredRouting] Edges used in Layer $i:")
//         edgeSet.toList.sortBy(_._1).foreach { case (u, v) =>
//           println(f"  Edge: ($u->$v)")
//         }
//       }

//       val layerMap: Map[(Int, Int), Int] = packed.zipWithIndex.flatMap { case (layer, layerIdx) =>
//         layer.map { case ((src, dst), _) => ((src, dst), layerIdx) }
//       }.toMap
      
//       val flows: Set[(Int, Int)] = layerMap.keySet

//       println("[CustomLayeredRouting] All edges: " + edgeList.map(e => s"(${e._1},${e._2})").mkString(", "))

//       packed.zipWithIndex.foreach { case (layer, i) =>
//         println(s"[CustomLayeredRouting] Layer $i:")
//         layer.foreach { case ((src, dst), path) =>
//           println(f"  Flow ($src->$dst) uses path: ${path.mkString("->")}")
//         }
//       }
//       println(s"[CustomLayeredRouting] Required VCs: ${packed.length}")

//       val flowToVC: Map[(Int, Int), Int] = packed.zipWithIndex.flatMap { case (layer, vc) =>
//         layer.map { case ((src, dst), _) => (src, dst) -> vc }
//       }.toMap

//       flowToVC.foreach { case ((src, dst), vc) =>
//         println(s"[CustomLayeredRouting] Flow ($src->$dst) assigned to logical VC $vc")
//       }

//       val vcToEdges = packed.zipWithIndex.map { case (layer, _) =>
//         layer.flatMap(_._2).toSet
//       }

//       def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Boolean = {
//         val flowKey = (flow.ingressNode, flow.egressNode)

//         if (srcC.src == -1) {
//           println(s"[rel] Ingress hop for flow $flowKey: allowing VC ${nxtC.vc}")
//           return true
//         }

//         if (srcC.vc != nxtC.vc) {
//           println(s"[rel] VC switch disallowed: ${srcC.vc} -> ${nxtC.vc} for flow $flowKey")
//           return false
//         }

//         flowToVC.get(flowKey) match {
//           case Some(logicalVC) =>
//             val expectedVC = flow.vNetId * vcsPerSubnet + logicalVC
//             val allowed = nxtC.vc == expectedVC && vcToEdges(logicalVC).contains((srcC.src, nxtC.src))
//             if (!allowed) {
//               println(s"[rel] Disallowed move for flow $flowKey from ${srcC.src} -> ${nxtC.src} in VC ${nxtC.vc}")
//             }
//             allowed
//           case None =>
//             println(s"[rel] No VC assignment for flow $flowKey")
//             false
//         }
//       }

//       override def isEscape(c: ChannelRoutingInfo, v: Int): Boolean =
//         c.isIngress || c.isEgress

//       override def getNPrios(c: ChannelRoutingInfo): Int = 1
//       override def getPrio(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Int = 0
//     }
//   }
// }

// class CustomGraph(val nNodes: Int, val edges: Seq[(Int, Int)]) {

//   def generateSSPs(): List[((Int, Int), List[(Int, Int)])] = {
//     val adj = Array.fill(nNodes)(mutable.ListBuffer[Int]())
//     edges.foreach { case (u, v) => adj(u) += v }

//     val result = mutable.ListBuffer[((Int, Int), List[(Int, Int)])]()

//     for (src <- 0 until nNodes; dst <- 0 until nNodes if src != dst) {
//       val visited = Array.fill(nNodes)(false)
//       val pred = Array.fill(nNodes)(-1)
//       val q = mutable.Queue[Int](src)
//       visited(src) = true

//       var found = false
//       while (q.nonEmpty && !found) {
//         val u = q.dequeue()
//         for (v <- adj(u) if !visited(v)) {
//           visited(v) = true
//           pred(v) = u
//           if (v == dst) found = true else q.enqueue(v)
//         }
//       }

//       if (found) {
//         val path = mutable.ListBuffer[Int](dst)
//         var cur = dst
//         while (pred(cur) != -1) {
//           cur = pred(cur)
//           path.prepend(cur)
//         }

//         val edgePath: List[(Int, Int)] = path.sliding(2).toList.map { pair =>
//           pair.toSeq match {
//             case Seq(a, b) => (a, b)
//             case other => throw new RuntimeException(s"Unexpected sliding window: $other")
//           }
//         }

//         result.append(((src, dst), edgePath))
//       }
//     }

//     result.toList
//   }


//   def deduplicateSSPs(ssps: List[((Int, Int), List[(Int, Int)])]): List[((Int, Int), List[(Int, Int)])] = {
//     val seen = mutable.HashSet[Seq[(Int, Int)]]()
//     val result = mutable.ListBuffer[((Int, Int), List[(Int, Int)])]()

//     for ((label, path) <- ssps) {
//       val edgeSeq = path.toIndexedSeq  // Ordered sequence
//       if (!seen.contains(edgeSeq)) {
//         seen += edgeSeq
//         result.append((label, path))
//       } else {
//         println(s"[CustomLayeredRouting] Deduplicated: $label with path ${path.mkString("->")}")
//       }
//     }

//     result.toList
//   }


//   def pruneSubpaths(ssps: List[((Int, Int), List[(Int, Int)])]): List[((Int, Int), List[(Int, Int)])] = {
//     val edgeSets = ssps.map { case (_, path) => path.toSet }

//     val pruned = ssps.zipWithIndex.filterNot { case ((labelI, pathI), i) =>
//       val setI = pathI.toSet
//       val isSubpath = edgeSets.zipWithIndex.exists { case (setJ, j) =>
//         i != j && setI.subsetOf(setJ) && setI != setJ // strict subset only
//       }
//       if (isSubpath) {
//         println(s"[CustomLayeredRouting] Pruned subpath: $labelI with path ${pathI.mkString("->")}")
//       }
//       isSubpath
//     }.map(_._1)

//     pruned
//   }


//   def packLayersMinimal(ssps: List[((Int, Int), List[(Int, Int)])]): List[List[((Int, Int), List[(Int, Int)])]] = {
//     for (k <- 1 to ssps.length) {
//       println("[CustomLayeredRouting] === Flow Order Passed to solve() ===")
//       ssps.zipWithIndex.foreach { case (((src, dst), path), i) =>
//         println(f"  $i%02d: ($src->$dst) path: ${path.mkString("->")}")
//       }
//       val result = solve(ssps, k)
//       if (result.nonEmpty) return result
//     }
//     List()
//   }

//   def solve(ssps: List[((Int, Int), List[(Int, Int)])], k: Int): List[List[((Int, Int), List[(Int, Int)])]] = {
//     val layers = Array.fill(k)(mutable.ListBuffer[((Int, Int), List[(Int, Int)])]())
//     val dags = Array.fill(k)(mutable.Map[(Int, Int), Int]())
//     val dagGraphs = Array.fill(k)(mutable.Map[Int, mutable.Set[Int]]())

//     def isDAG(g: Map[Int, Set[Int]]): Boolean = {
//       val visited = mutable.Map[Int, Int]() // 0 = unvisited, 1 = visiting, 2 = visited

//       def dfs(u: Int): Boolean = {
//         visited.get(u) match {
//           case Some(1) => return false // cycle
//           case Some(2) => return true
//           case _ => // proceed
//         }

//         visited(u) = 1
//         for (v <- g.getOrElse(u, Set())) {
//           if (!dfs(v)) return false
//         }
//         visited(u) = 2
//         true
//       }

//       g.keys.forall(dfs)
//     }

//     def addEdges(layer: Int, path: List[(Int, Int)]): Boolean = {
//       val g = dagGraphs(layer)
//       val edgeCount = dags(layer)

//       // Tentatively add
//       val newlyAdded = mutable.ListBuffer[(Int, Int)]()
//       for ((u, v) <- path) {
//         if (!g.contains(u)) g(u) = mutable.Set()
//         val added = g(u).add(v)
//         if (added) newlyAdded += ((u, v))
//         edgeCount((u, v)) = edgeCount.getOrElse((u, v), 0) + 1
//       }

//       // Check DAG
//       val snapshot = g.map { case (k, v) => (k, v.toSet) }.toMap
//       if (!isDAG(snapshot)) {
//         // Revert
//         for ((u, v) <- newlyAdded) g(u).remove(v)
//         for ((u, v) <- path) {
//           edgeCount((u, v)) -= 1
//           if (edgeCount((u, v)) == 0) edgeCount.remove((u, v))
//         }
//         return false
//       }

//       true
//     }

//     def removeEdges(layer: Int, path: List[(Int, Int)]): Unit = {
//       val g = dagGraphs(layer)
//       val edgeCount = dags(layer)

//       for ((u, v) <- path) {
//         edgeCount((u, v)) -= 1
//         if (edgeCount((u, v)) == 0) {
//           edgeCount.remove((u, v))
//           g(u).remove(v)
//           if (g(u).isEmpty) g.remove(u)
//         }
//       }
//     }

//     def backtrack(i: Int): Boolean = {
//       if (i == ssps.length) return true
//       val (label, path) = ssps(i)

//       for (j <- 0 until k) {
//         if (addEdges(j, path)) {
//           layers(j) += ((label, path))
//           if (backtrack(i + 1)) return true
//           layers(j).remove(layers(j).length - 1)
//           removeEdges(j, path)
//         }
//       }
//       false
//     }

//     if (backtrack(0)) layers.map(_.toList).toList else List()
//   }


// }

object CustomLayeredRouting {
  
  def apply() = (topo: PhysicalTopology) => topo match {
    case topo: CustomTopology => new RoutingRelation(topo) {
      val edgeList = topo.edgeList.map(e => (e.src, e.dst))
      val graph = new CustomGraph(topo.nNodes, edgeList)
      val allSSPs = graph.generateSSPs()

      allSSPs.foreach { case ((src, dst), path) =>
        println(s"[DEBUG] Path from $src to $dst: ${path.mkString("->")}")
      }

      val deduped = graph.deduplicateSSPs(allSSPs)
      val pruned = graph.pruneSubpaths(deduped)
      val packed = graph.packLayersMinimal(pruned)

      packed.zipWithIndex.foreach { case (layer, i) =>
        val edgeSet = layer.flatMap(_._2).toSet
        println(s"[CustomLayeredRouting] Edges used in Layer $i:")
        edgeSet.toList.sortBy(_._1).foreach { case (u, v) =>
          println(f"  Edge: ($u->$v)")
        }
      }

      println("[CustomLayeredRouting] All edges: " + edgeList.map(e => s"(${e._1},${e._2})").mkString(", "))

      packed.zipWithIndex.foreach { case (layer, i) =>
        println(s"[CustomLayeredRouting] Layer $i:")
        layer.foreach { case ((src, dst), path) =>
          println(f"  Flow ($src->$dst) uses path: ${path.mkString("->")}")
        }
      }
      println(s"[CustomLayeredRouting] Required VCs: ${packed.length}")

      // === VC assignment for all flows based on first matching layer ===
      val allFlowAssignments: Map[(Int, Int), Int] = allSSPs.map { case ((src, dst), path) =>
        val assignedLayer = packed.indexWhere(layer => path.toSet.subsetOf(layer.flatMap(_._2).toSet))
        require(assignedLayer >= 0, s"[CustomLayeredRouting] ERROR: Could not find a layer covering path for flow ($src->$dst)")
        ((src, dst), assignedLayer)
      }.toMap


      allFlowAssignments.foreach { case ((src, dst), vc) =>
        println(s"[CustomLayeredRouting] Flow ($src->$dst) assigned to logical VC $vc")
      }

      // Build nextHop map per VC (layer)
      val nextHopMap: Map[Int, Map[(Int, Int), Int]] = packed.zipWithIndex.map { case (layer, vc) =>
        val nhs = mutable.Map[(Int, Int), Int]()

        for (((src, dst), path) <- layer) {
          val nodes = path.map(_._1) :+ path.last._2  // all nodes in order
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
          println(s"[rel ERROR] No VC assigned to flow $flowKey")
          return false
        })

        // Step 2: Lookup full path and check it lies entirely within assigned VC layer
        val flowPathEdges = flowPaths.getOrElse(flowKey, {
          println(s"[rel ERROR] No path found for flow $flowKey")
          return false
        }).toSet

        val assignedEdgeSet = vcToEdges(assignedVC)

        val fullPathOk = flowPathEdges.subsetOf(assignedEdgeSet)
        if (!fullPathOk) {
          println(s"[rel ERROR] Full path for flow $flowKey is NOT entirely in VC=$assignedVC")
          return false
        }

        // Step 3: Determine the current and next node
        val curNode  = if (srcC.src == -1) flow.ingressNode else srcC.dst
        val nextNode = nxtC.dst

        // Step 4: Determine if the hop is allowed
        // val edgeOk = assignedEdgeSet.contains((curNode, nextNode))
        val edgeOk = flowPathEdges.contains((curNode, nextNode))

        // val edgeOk = assignedEdgeSet.contains((curNode, nextNode)) && flowPathEdges.contains((curNode, nextNode))

        val vcOk   = nxtC.vc == assignedVC && (srcC.src == -1 || srcC.vc == assignedVC)

        val legal = edgeOk && vcOk

        if (!legal) {
          println(s"[rel BLOCKED] Flow $flowKey: hop ($curNode -> $nextNode) with VC=${nxtC.vc} invalid for assigned VC=$assignedVC")
        } else {
          println(s"[rel ALLOWED] Flow $flowKey: hop ($curNode -> $nextNode) with VC=${nxtC.vc} valid for assigned VC=$assignedVC")
        }

        legal
      }

      override def isEscape(c: ChannelRoutingInfo, v: Int): Boolean =
        c.isIngress || c.isEgress

      val guAssignedLayer = allFlowAssignments  // synonym for clarity
      val nVirtualLayers = vcToEdges.length     // number of packed layers

      override def getNPrios(c: ChannelRoutingInfo): Int = nVirtualLayers

      override def getPrio(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Int = {
        allFlowAssignments.getOrElse((flow.ingressNode, flow.egressNode), {
          println(s"[getPrio ERROR] No VC assignment for flow (${flow.ingressNode} -> ${flow.egressNode})")
          0
        })
      }

    }
  }

}

class CustomGraph(val nNodes: Int, val edges: Seq[(Int, Int)]) {

  def generateSSPs(): List[((Int, Int), List[(Int, Int)])] = {
    val adj = Array.fill(nNodes)(mutable.ListBuffer[Int]())
    edges.foreach { case (u, v) => adj(u) += v }

    val result = mutable.ListBuffer[((Int, Int), List[(Int, Int)])]()

    for (src <- 0 until nNodes; dst <- 0 until nNodes if src != dst) {
      val visited = Array.fill(nNodes)(false)
      val pred = Array.fill(nNodes)(-1)
      val q = mutable.Queue[Int](src)
      visited(src) = true

      var found = false
      while (q.nonEmpty && !found) {
        val u = q.dequeue()
        for (v <- adj(u) if !visited(v)) {
          visited(v) = true
          pred(v) = u
          if (v == dst) found = true else q.enqueue(v)
        }
      }

      if (found) {
        val path = mutable.ListBuffer[Int](dst)
        var cur = dst
        while (pred(cur) != -1) {
          cur = pred(cur)
          path.prepend(cur)
        }

        val edgePath: List[(Int, Int)] = path.sliding(2).toList.map { pair =>
          pair.toSeq match {
            case Seq(a, b) => (a, b)
            case other => throw new RuntimeException(s"Unexpected sliding window: $other")
          }
        }

        result.append(((src, dst), edgePath))
      }
    }

    result.toList
  }


  def deduplicateSSPs(ssps: List[((Int, Int), List[(Int, Int)])]): List[((Int, Int), List[(Int, Int)])] = {
    val seen = mutable.HashSet[Seq[(Int, Int)]]()
    val result = mutable.ListBuffer[((Int, Int), List[(Int, Int)])]()

    for ((label, path) <- ssps) {
      val edgeSeq = path.toIndexedSeq  // Ordered sequence
      if (!seen.contains(edgeSeq)) {
        seen += edgeSeq
        result.append((label, path))
      } else {
        println(s"[CustomLayeredRouting] Deduplicated: $label with path ${path.mkString("->")}")
      }
    }

    result.toList
  }


  def pruneSubpaths(ssps: List[((Int, Int), List[(Int, Int)])]): List[((Int, Int), List[(Int, Int)])] = {
    val edgeSets = ssps.map { case (_, path) => path.toSet }

    val pruned = ssps.zipWithIndex.filterNot { case ((labelI, pathI), i) =>
      val setI = pathI.toSet
      val isSubpath = edgeSets.zipWithIndex.exists { case (setJ, j) =>
        i != j && setI.subsetOf(setJ) && setI != setJ // strict subset only
      }
      if (isSubpath) {
        println(s"[CustomLayeredRouting] Pruned subpath: $labelI with path ${pathI.mkString("->")}")
      }
      isSubpath
    }.map(_._1)

    pruned
  }

  def packLayersMinimal(ssps: List[((Int, Int), List[(Int, Int)])]): List[List[((Int, Int), List[(Int, Int)])]] = {
    for (k <- 1 to ssps.length) {
      println("[CustomLayeredRouting] === Flow Order Passed to solve() ===")
      ssps.zipWithIndex.foreach { case (((src, dst), path), i) =>
        println(f"  $i%02d: ($src->$dst) path: ${path.mkString("->")}")
      }
      val result = solve(ssps, k)
      if (result.nonEmpty) return result
    }
    List()
  }

  def solve(ssps: List[((Int, Int), List[(Int, Int)])], k: Int): List[List[((Int, Int), List[(Int, Int)])]] = {
    val layers = Array.fill(k)(mutable.ListBuffer[((Int, Int), List[(Int, Int)])]())
    val dags = Array.fill(k)(mutable.Map[(Int, Int), Int]())
    val dagGraphs = Array.fill(k)(mutable.Map[Int, mutable.Set[Int]]())

    def isDAG(g: Map[Int, Set[Int]]): Boolean = {
      val visited = mutable.Map[Int, Int]() // 0 = unvisited, 1 = visiting, 2 = visited

      def dfs(u: Int): Boolean = {
        visited.get(u) match {
          case Some(1) => return false // cycle
          case Some(2) => return true
          case _ => // proceed
        }

        visited(u) = 1
        for (v <- g.getOrElse(u, Set())) {
          if (!dfs(v)) return false
        }
        visited(u) = 2
        true
      }

      g.keys.forall(dfs)
    }

    def addEdges(layer: Int, path: List[(Int, Int)]): Boolean = {
      val g = dagGraphs(layer)
      val edgeCount = dags(layer)

      // Tentatively add
      val newlyAdded = mutable.ListBuffer[(Int, Int)]()
      for ((u, v) <- path) {
        if (!g.contains(u)) g(u) = mutable.Set()
        val added = g(u).add(v)
        if (added) newlyAdded += ((u, v))
        edgeCount((u, v)) = edgeCount.getOrElse((u, v), 0) + 1
      }

      // Check DAG
      val snapshot = g.map { case (k, v) => (k, v.toSet) }.toMap
      if (!isDAG(snapshot)) {
        // Revert
        for ((u, v) <- newlyAdded) g(u).remove(v)
        for ((u, v) <- path) {
          edgeCount((u, v)) -= 1
          if (edgeCount((u, v)) == 0) edgeCount.remove((u, v))
        }
        return false
      }

      true
    }

    def removeEdges(layer: Int, path: List[(Int, Int)]): Unit = {
      val g = dagGraphs(layer)
      val edgeCount = dags(layer)

      for ((u, v) <- path) {
        edgeCount((u, v)) -= 1
        if (edgeCount((u, v)) == 0) {
          edgeCount.remove((u, v))
          g(u).remove(v)
          if (g(u).isEmpty) g.remove(u)
        }
      }
    }

    def backtrack(i: Int): Boolean = {
      if (i == ssps.length) return true
      val (label, path) = ssps(i)

      for (j <- 0 until k) {
        if (addEdges(j, path)) {
          layers(j) += ((label, path))
          if (backtrack(i + 1)) return true
          layers(j).remove(layers(j).length - 1)
          removeEdges(j, path)
        }
      }
      false
    }

    if (backtrack(0)) layers.map(_.toList).toList else List()
  }


}