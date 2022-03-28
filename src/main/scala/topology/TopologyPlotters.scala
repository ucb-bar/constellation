package constellation.topology

import scala.math.{pow, cos, sin, Pi, atan2}

class LinePlotter extends PhysicalTopologyPlotter {
  def node(n: Double) = (n, 0)
  def ingress(t: Double, nT: Double, n: Double) = (n - 0.5 + (t + 1) / (nT + 1),  0.5)
  def egress (t: Double, nT: Double, n: Double) = (n - 0.5 + (t + 1) / (nT + 1), -0.5)
}

class Torus1DPlotter(nNodes: Int) extends PhysicalTopologyPlotter {
  def node(n: Double) = {
    val rad = n * 2 * Pi / nNodes
    (cos(rad), sin(rad))
  }
  def ingress(t: Double, nT: Double, n: Double) = {
    val arc = 2 * Pi / nNodes
    val rad = arc * (n - 0.5 + (t + 1) / (nT + 1))
    (cos(rad) * 1.2, sin(rad) * 1.2)
  }
  def egress (t: Double, nT: Double, n: Double) = {
    val arc = 2 * Pi / nNodes
    val rad = arc * (n - 0.5 + (t + 1) / (nT + 1))
    (cos(rad) * 0.8, sin(rad) * 0.8)
  }
}

class ButterflyPlotter(kAry: Int, nFly: Int) extends PhysicalTopologyPlotter {
  val height = pow(kAry, nFly-1).toInt
  def node(n: Double) = {
    val x = n.toInt / height
    val y = n.toInt % height
    (x, y)
  }
  def ingress(t: Double, nT: Double, n: Double) = (-1, n.toInt % height - 0.5 + (t + 1) / (nT + 1))
  def egress (t: Double, nT: Double, n: Double) = (1 + n.toInt / height, n.toInt % height - 0.5 + (t + 1) / (nT + 1))
}

class Mesh2DPlotter(nX: Int, nY: Int) extends PhysicalTopologyPlotter {
  def node(n: Double) = (n.toInt % nX, n.toInt / nX)
  def ingress(t: Double, nT: Double, n: Double) = {
    val rad =  2 * Pi * 0.25 * (t + 1) / (nT + 1)
    (cos(rad) * 0.4 + n.toInt % nX, sin(rad) * 0.4 + n.toInt / nX)
  }
  def egress(t: Double, nT: Double, n: Double) = {
    val rad = -2 * Pi * 0.25 * (t + 1) / (nT + 1)
    (cos(rad) * 0.4 + n.toInt % nX, sin(rad) * 0.4 + n.toInt / nX)
  }
}

class TerminalPlanePlotter(base: PhysicalTopologyPlotter, baseNodes: Int) extends PhysicalTopologyPlotter {
  def node(n: Double) = {
    if (n < baseNodes) {
      // routing nodes
      base.node(n)
    } else if (n < baseNodes * 2) {
      // ingress nodes
      val b = base.node(n - baseNodes)
      base match {
        case _: LinePlotter => (b._1, b._2 + 0.25)
        case _: Torus1DPlotter => {
          val rad = atan2(b._2, b._1)
          (cos(rad) * 1.1, sin(rad) * 1.1)
        }
        case _: ButterflyPlotter => (b._1 - 0.5, b._2)
        case _: Mesh2DPlotter => (b._1 + 0.15, b._2 + 0.15)
        case _ => require(false, "Unsupported"); (0.0, 0.0)
      }
    } else {
      // egress nodes
      val b = base.node(n - 2 * baseNodes)
      base match {
        case _: LinePlotter => (b._1, b._2 - 0.25)
        case _: Torus1DPlotter => {
          val rad = atan2(b._2, b._1)
          (cos(rad) * 0.9, sin(rad) * 0.9)
        }
        case _: ButterflyPlotter => (b._1 + 0.5, b._2)
        case _: Mesh2DPlotter => (b._1 + 0.15, b._2 - 0.15)
        case _ => require(false, "Unsupported"); (0.0, 0.0)
      }
    }
  }
  def ingress(t: Double, nT: Double, n: Double) = {
    base.ingress(t, nT, n - baseNodes)
  }
  def egress(t: Double, nT: Double, n: Double) = {
    base.egress(t, nT, n - 2 * baseNodes)
  }
}
