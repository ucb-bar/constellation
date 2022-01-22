package constellation.topology

import scala.math.{pow, cos, sin, Pi}

class UnidirectionalLine extends PhysicalTopology {
  def topo(src: Int, dest: Int) = dest - src == 1
  val plotter = new LinePlotter
}

class BidirectionalLine extends PhysicalTopology {
  def topo(src: Int, dest: Int) = (dest - src).abs == 1
  val plotter = new LinePlotter
}

class UnidirectionalTorus1D(nNodes: Int) extends PhysicalTopology {
  def topo(src: Int, dest: Int) = dest - src == 1 || (dest == 0 && src == nNodes - 1)
  val plotter = new Torus1DPlotter(nNodes)
}

class BidirectionalTorus1D(nNodes: Int) extends PhysicalTopology {
  def topo(src: Int, dest: Int) = (dest + nNodes - src) % nNodes == 1 || (src + nNodes - dest) % nNodes == 1
  val plotter = new Torus1DPlotter(nNodes)
}

class Butterfly(kAry: Int, nFly: Int) extends PhysicalTopology {
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

class Mesh2D(nX: Int, nY: Int) extends PhysicalTopology {
  def topo(src: Int, dst: Int) = {
    val (srcX, srcY) = (src % nX, src / nX)
    val (dstX, dstY) = (dst % nX, dst / nX)
      (srcX == dstX && (srcY - dstY).abs == 1) || (srcY == dstY && (srcX - dstX).abs == 1)
  }
  val plotter = new Mesh2DPlotter(nX, nY)
}

class UnidirectionalTorus2D(nX: Int, nY: Int) extends PhysicalTopology {
  def topo(src: Int, dst: Int) = {
    val (srcX, srcY) = (src % nX, src / nX)
    val (dstX, dstY) = (dst % nX, dst / nX)
    ((srcY == dstY && new UnidirectionalTorus1D(nX).topo(srcX, dstX)) ||
      (srcX == dstX && new UnidirectionalTorus1D(nY).topo(srcY, dstY)))
  }
  val plotter = new Mesh2DPlotter(nX, nY)
}

class BidirectionalTorus2D(nX: Int, nY: Int) extends PhysicalTopology {
  def topo(src: Int, dst: Int) = {
    val (srcX, srcY) = (src % nX, src / nX)
    val (dstX, dstY) = (dst % nX, dst / nX)
    ((srcY == dstY && new BidirectionalTorus1D(nX).topo(srcX, dstX)) ||
      (srcX == dstX && new BidirectionalTorus1D(nY).topo(srcY, dstY)))
  }
  val plotter = new Mesh2DPlotter(nX, nY)
}
