package constellation.topology

import scala.math.{pow, cos, sin, Pi, atan2, floor}

abstract class PhysicalTopologyPlotter {
  /* Given a node ID, returns x, y position of a node. */
  def node(nodeId: Int): (Double, Double)
  /* iID: index of ingress within number of ingresses for that node
   nI: number of ingresses into node
   nodeID: node these ingresses connect to*/
  def ingress(iId: Int, nI: Int, nodeId: Int): (Double, Double)
  def egress (eId: Int, nE: Int, nodeId: Int): (Double, Double)
}


class LinePlotter(topo: PhysicalTopology) extends PhysicalTopologyPlotter {
  def node(n: Int) = (n, 0)
  def ingress(t: Int, nT: Int, n: Int) = (n - 0.5 + (t + 1).toDouble / (nT + 1).toDouble,  0.5)
  def egress (t: Int, nT: Int, n: Int) = (n - 0.5 + (t + 1).toDouble / (nT + 1).toDouble, -0.5)
}

class Torus1DPlotter(topo: Torus1DLikeTopology) extends PhysicalTopologyPlotter {
  def node(n: Int) = {
    val rad = n * 2 * Pi / topo.nNodes
    (cos(rad), sin(rad))
  }
  def ingress(t: Int, nT: Int, n: Int) = {
    val arc = 2 * Pi / topo.nNodes
    val rad = arc * (n - 0.5 + (t + 1).toDouble / (nT + 1).toDouble)
    (cos(rad) * 1.2, sin(rad) * 1.2)
  }
  def egress (t: Int, nT: Int, n: Int) = {
    val arc = 2 * Pi / topo.nNodes
    val rad = arc * (n - 0.5 + (t + 1).toDouble / (nT + 1).toDouble)
    (cos(rad) * 0.8, sin(rad) * 0.8)
  }
}

class ButterflyPlotter(topo: Butterfly) extends PhysicalTopologyPlotter {
  val height = topo.height
  def node(n: Int) = {
    val x = n / height
    val y = n % height
    (x, y)
  }
  def ingress(t: Int, nT: Int, n: Int) = (-1,
    n % height - 0.5 + (t + 1).toDouble / (nT + 1).toDouble)
  def egress (t: Int, nT: Int, n: Int) = (
    1 + n / height,
    n % height - 0.5 + (t + 1).toDouble / (nT + 1).toDouble)
}

class Mesh2DPlotter(topo: Mesh2DLikePhysicalTopology) extends PhysicalTopologyPlotter {
  def node(n: Int) = (n % topo.nX, n / topo.nX)
  def ingress(t: Int, nT: Int, n: Int) = {
    val rad =  2 * Pi * 0.25 * (t + 1).toDouble / (nT + 1).toDouble
    (cos(rad) * 0.4 + n % topo.nX, sin(rad) * 0.4 + n / topo.nX)
  }
  def egress(t: Int, nT: Int, n: Int) = {
    val rad = -2 * Pi * 0.25 * (t + 1).toDouble / (nT + 1).toDouble
    (cos(rad) * 0.4 + n % topo.nX, sin(rad) * 0.4 + n / topo.nX)
  }
}

class TreePlotter(topo: BidirectionalTree) extends PhysicalTopologyPlotter {
  val nNodes = topo.nNodes
  val dAry = topo.dAry
  val height = topo.height
  def isLeaf(node: Int) = (node >= (nNodes - 1 - (pow(dAry, height) - 1))) && (node <= nNodes - 1)

  /* Returns the total number of nodes under NODE in the tree */
  def nodesUnder(node: Int): Int = {
    if (isLeaf(node)) { 0 } else { dAry * (nodesUnder(dAry * node + 1)) + dAry }
  }

  /* Given a child node id, returns the parent node's id. */
  def parent(node: Int): Option[Int] = {
    if (node == 0) { None } else { Some( (node - 1) / dAry) }
  }

  def node(nodeId: Int) = {
    parent(nodeId) match {
      case None =>
        (0, 0)
      case Some(p) =>
        val parentCoords = node(p)
        val childIndex = nodeId - (dAry * p + 1)
        val rowLength = (nodesUnder(p) + nodesUnder(p) % 2)
        (parentCoords._1 - rowLength / 2 + rowLength * (childIndex / (dAry - 1)), parentCoords._2 - 1)
    }
  }

  def ingress(iId: Int, nI: Int, nodeId: Int) = {
    val nodeCoords = node(nodeId)
    (nodeCoords._1 - (iId + 1), nodeCoords._2)
  }

  def egress (eId: Int, nE: Int, nodeId: Int) = {
    val nodeCoords = node(nodeId)
    (nodeCoords._1 + (eId + 1), nodeCoords._2)
  }
}


class TerminalRouterPlotter(topo: TerminalRouter) extends PhysicalTopologyPlotter {
  val basePlotter = topo.base.plotter
  def node(n: Int) = {
    if (topo.isBase(n)) {
      // routing nodes
      basePlotter.node(n % topo.base.nNodes)
    } else {
      val b = basePlotter.node(n % topo.base.nNodes)
      basePlotter match {
        case _: LinePlotter => (b._1, b._2 + 0.25)
        case _: Torus1DPlotter => {
          val rad = atan2(b._2, b._1)
          (cos(rad) * 1.1, sin(rad) * 1.1)
        }
        case _: ButterflyPlotter => (b._1 - 0.5, b._2)
        case _: Mesh2DPlotter => (b._1 + 0.15, b._2 + 0.15)
        case _ => require(false, "Unsupported"); (0.0, 0.0)
      }
    }
  }
  def ingress(t: Int, nT: Int, n: Int) = {
    basePlotter.ingress(t, nT, n % topo.base.nNodes)
  }
  def egress(t: Int, nT: Int, n: Int) = {
    basePlotter.egress(t, nT, n % topo.base.nNodes)
  }
}
