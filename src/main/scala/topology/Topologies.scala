package constellation.topology

import scala.math.{pow, cos, sin, Pi}

/* See topology/package.scala for a description of PhysicalTopology and the topo method. */

/** A 2-node network where an ingress node has a unidrectional channel connecting to an egress node. */
class UnidirectionalLine(n: Int) extends PhysicalTopology(n) {
  def topo(src: Int, dest: Int) = dest - src == 1
  val plotter = new LinePlotter
}

/** A 2-node network with a bidirectional channel connecting both nodes. */
class BidirectionalLine(n: Int) extends PhysicalTopology(n) {
  def topo(src: Int, dest: Int) = (dest - src).abs == 1
  val plotter = new LinePlotter
}

/** An n-node (nodes are [0, n-1]) unidrectional network shaped like a torus. 
 *  Node i has a channel to node i+1 with the exception of node-1 which has a channel
 *  to node 0.
 */
class UnidirectionalTorus1D(n: Int) extends PhysicalTopology(n) {
  def topo(src: Int, dest: Int) = dest - src == 1 || (dest == 0 && src == nNodes - 1)
  val plotter = new Torus1DPlotter(nNodes)
}

/** An n-node (nodes are [0, n-1]) network shaped like a torus. 
 *  Node i has a channel to nodes (i+1)%n and (i-1)%n with the exception of 
 */
class BidirectionalTorus1D(n: Int) extends PhysicalTopology(n) {
  def topo(src: Int, dest: Int) = (dest + nNodes - src) % nNodes == 1 || (src + nNodes - dest) % nNodes == 1
  val plotter = new Torus1DPlotter(nNodes)
}

class Butterfly(kAry: Int, nFly: Int) extends PhysicalTopology(pow(kAry, nFly-1).toInt * nFly) {
  require(kAry >= 2 && nFly >= 2)
  val height = pow(kAry, nFly-1).toInt
  def digitsToNum(dig: Seq[Int]) = dig.zipWithIndex.map { case (d,i) => d * pow(kAry,i).toInt }.sum
  val table = (0 until pow(kAry, nFly).toInt).map { i =>
    (0 until nFly).map { n => (i / pow(kAry, n).toInt) % kAry }
  }
  val channels = (1 until nFly).map { i =>
    table.map { e => (digitsToNum(e.drop(1)), digitsToNum(e.updated(i, e(0)).drop(1))) }
  }

  def topo(src: Int, dest: Int) = {
    val (srcX, srcY) = (src / height, src % height)
    val (destX, destY) = (dest / height, dest % height)
    if (srcX < nFly - 1 && destX == srcX + 1) {
      val connected = channels(srcX).contains((srcY, destY))
      connected
    } else {
      false
    }
  }
  val plotter = new ButterflyPlotter(kAry, nFly)
}

class Mesh2D(nX: Int, nY: Int) extends PhysicalTopology(nX * nY) {
  def topo(src: Int, dst: Int) = {
    val (srcX, srcY) = (src % nX, src / nX)
    val (dstX, dstY) = (dst % nX, dst / nX)
      (srcX == dstX && (srcY - dstY).abs == 1) || (srcY == dstY && (srcX - dstX).abs == 1)
  }
  val plotter = new Mesh2DPlotter(nX, nY)
}

class UnidirectionalTorus2D(nX: Int, nY: Int) extends PhysicalTopology(nX * nY) {
  def topo(src: Int, dst: Int) = {
    val (srcX, srcY) = (src % nX, src / nX)
    val (dstX, dstY) = (dst % nX, dst / nX)
    ((srcY == dstY && new UnidirectionalTorus1D(nX).topo(srcX, dstX)) ||
      (srcX == dstX && new UnidirectionalTorus1D(nY).topo(srcY, dstY)))
  }
  val plotter = new Mesh2DPlotter(nX, nY)
}

class BidirectionalTorus2D(nX: Int, nY: Int) extends PhysicalTopology(nX * nY) {
  def topo(src: Int, dst: Int) = {
    val (srcX, srcY) = (src % nX, src / nX)
    val (dstX, dstY) = (dst % nX, dst / nX)
    ((srcY == dstY && new BidirectionalTorus1D(nX).topo(srcX, dstX)) ||
      (srcX == dstX && new BidirectionalTorus1D(nY).topo(srcY, dstY)))
  }
  val plotter = new Mesh2DPlotter(nX, nY)
}
