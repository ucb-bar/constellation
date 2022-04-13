package constellation.topology

import scala.math.{pow, cos, sin, Pi}

/** A network where sequential nodes are connected unidirectionally */
class UnidirectionalLine(n: Int) extends PhysicalTopology(n) {
  def topo(src: Int, dest: Int) = dest - src == 1
  val plotter = new LinePlotter
}

/** A network where sequential nodes are connected bidirectionally */
class BidirectionalLine(n: Int) extends PhysicalTopology(n) {
  def topo(src: Int, dest: Int) = (dest - src).abs == 1
  val plotter = new LinePlotter
}

/** An n-node network shaped like a torus, with clockwise channels */
class UnidirectionalTorus1D(n: Int) extends PhysicalTopology(n) {
  def topo(src: Int, dest: Int) = dest - src == 1 || (dest == 0 && src == nNodes - 1)
  val plotter = new Torus1DPlotter(nNodes)
}

/** An n-node network shaped like a torus, with bidirectional channels */
class BidirectionalTorus1D(n: Int) extends PhysicalTopology(n) {
  def topo(src: Int, dest: Int) = (dest + nNodes - src) % nNodes == 1 || (src + nNodes - dest) % nNodes == 1
  val plotter = new Torus1DPlotter(nNodes)
}

/** A k-ary n-fly butterfly topology. This network has n stages of nodes; each node connects to k
  * nodes in the next stage.
  *
  * @param kAry number of channels of a node that connect to the previous and/or next stage
  * @param nFly number of stages in network
  */
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

/** An dary**height tree topology.
  */
class BidirectionalTree(val height: Int, val dAry: Int = 2)
  extends PhysicalTopology(((dAry * pow(dAry, height) - 1) / (dAry - 1)).toInt) {
  require(dAry > 1)

  def topo(src: Int, dest: Int) = {
    val dstChildOfSrc = List.range(1, dAry + 1).map((c: Int) => src * dAry + c).contains(dest)
    val srcChildOfDst = List.range(1, dAry + 1).map((c: Int) => dest * dAry + c).contains(src)

    srcChildOfDst || dstChildOfSrc
  }

  val plotter = new TreePlotter(height, dAry)
}

/** A 2D mesh network with nX * nY nodes. Bidirectional channels exist between nodes that are a
 *  Manhattan distance of 1 away from each other. Node i can be thought of as being located
 *  at euclidean coordinate (i % nX, i / nX) where nX is as described below.
 *
 *  @param nX maximum x-coordinate of a node
 *  @param nY maximum y-coordinate of a node
 */
class Mesh2D(nX: Int, nY: Int) extends PhysicalTopology(nX * nY) {
  def topo(src: Int, dst: Int) = {
    val (srcX, srcY) = (src % nX, src / nX)
    val (dstX, dstY) = (dst % nX, dst / nX)
      (srcX == dstX && (srcY - dstY).abs == 1) || (srcY == dstY && (srcX - dstX).abs == 1)
  }
  val plotter = new Mesh2DPlotter(nX, nY)
}

/** A 2D unidirectional torus network with nX * nY nodes.
 *
 *  @param nX maximum x-coordinate of a node
 *  @param nY maximum y-coordinate of a node
 */
class UnidirectionalTorus2D(nX: Int, nY: Int) extends PhysicalTopology(nX * nY) {
  def topo(src: Int, dst: Int) = {
    val (srcX, srcY) = (src % nX, src / nX)
    val (dstX, dstY) = (dst % nX, dst / nX)
    ((srcY == dstY && new UnidirectionalTorus1D(nX).topo(srcX, dstX)) ||
      (srcX == dstX && new UnidirectionalTorus1D(nY).topo(srcY, dstY)))
  }
  val plotter = new Mesh2DPlotter(nX, nY)
}

/** A 2D bidirectional torus network with nX * nY nodes.
 *
 *  @param nX maximum x-coordinate of a node
 *  @param nY maximum y-coordinate of a node
 */
class BidirectionalTorus2D(nX: Int, nY: Int) extends PhysicalTopology(nX * nY) {
  def topo(src: Int, dst: Int) = {
    val (srcX, srcY) = (src % nX, src / nX)
    val (dstX, dstY) = (dst % nX, dst / nX)
    ((srcY == dstY && new BidirectionalTorus1D(nX).topo(srcX, dstX)) ||
      (srcX == dstX && new BidirectionalTorus1D(nY).topo(srcY, dstY)))
  }
  val plotter = new Mesh2DPlotter(nX, nY)
}

class TerminalPlaneTopology(val base: PhysicalTopology) extends PhysicalTopology(3 * base.nNodes) {
  def topo(src: Int, dst: Int) = {
    def isBase(n: Int) = n < base.nNodes
    def isIngress(n: Int) = !isEgress(n) && !isBase(n)
    def isEgress(n: Int) = n >= 2 * base.nNodes

    if (isBase(src) && isBase(dst)) {
      base.topo(src, dst)
    } else {
      def connected(lower: Int, upper: Int): Boolean = {
        if (lower > upper) {
          connected(upper, lower)
        } else {
          val toIngress = isIngress(upper) && upper - base.nNodes == lower
          val toEgress  =  isEgress(upper) && upper - 2 * base.nNodes == lower
          isBase(lower) && (toIngress || toEgress)
        }
      }
      connected(src, dst)
    }
  }
  val plotter = new TerminalPlanePlotter(base.plotter, base.nNodes)
}
