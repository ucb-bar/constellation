package constellation.topology

import scala.math.{pow, cos, sin, Pi}

class LinePlotter extends PhysicalTopologyPlotter {
  def node(n: Double) = (n, 0)
  def ingress(t: Double, nT: Double, n: Double) = (n - 0.5 + (t + 1) / (nT + 1),  0.5)
  def egress (t: Double, nT: Double, n: Double) = (n - 0.5 + (t + 1) / (nT + 1), -0.5)
}

class Torus1DPlotter(nNodes: Double) extends PhysicalTopologyPlotter {
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

