package constellation.topology

import scala.math.pow

object Topologies {
  val unidirectionalLine: PhysicalTopology = (src: Int, dest: Int) => dest - src == 1
  val bidirectionalLine: PhysicalTopology = (src: Int, dest: Int) => (dest - src).abs == 1

  def unidirectionalTorus1D(nNodes: Int): PhysicalTopology = (src: Int, dest: Int) => {
    dest - src == 1 || (dest == 0 && src == nNodes - 1)
  }
  def bidirectionalTorus1D(nNodes: Int): PhysicalTopology = (src: Int, dest: Int) => {
    (dest + nNodes - src) % nNodes == 1 || (src + nNodes - dest) % nNodes == 1
  }

  def butterfly(kAry: Int, nFly: Int): PhysicalTopology = {
    require(kAry >= 2 && nFly >= 2)
    val height = pow(kAry, nFly-1).toInt
    def digitsToNum(dig: Seq[Int]) = dig.zipWithIndex.map { case (d,i) => d * pow(kAry,i).toInt }.sum
    val table = (0 until pow(kAry, nFly).toInt).map { i =>
      (0 until nFly).map { n => (i / pow(kAry, n).toInt) % kAry }
    }
    val channels = (1 until nFly).map { i =>
      table.map { e => (digitsToNum(e.drop(1)), digitsToNum(e.updated(i, e(0)).drop(1))) }
    }
    (src: Int, dest: Int) => {
      val (srcX, srcY) = (src / height, src % height)
      val (destX, destY) = (dest / height, dest % height)
      if (srcX < nFly - 1 && destX == srcX + 1) {
        val connected = channels(srcX).contains((srcY, destY))
        connected
      } else {
        false
      }
    }
  }

  def mesh2D(nX: Int, nY: Int): PhysicalTopology = (src: Int, dst: Int) => {
    val (srcX, srcY) = (src % nX, src / nX)
    val (dstX, dstY) = (dst % nX, dst / nX)
    (srcX == dstX && (srcY - dstY).abs == 1) || (srcY == dstY && (srcX - dstX).abs == 1)
  }

  def unidirectionalTorus2D(nX: Int, nY: Int): PhysicalTopology = (src: Int, dst: Int) => {
    val (srcX, srcY) = (src % nX, src / nX)
    val (dstX, dstY) = (dst % nX, dst / nX)
    (srcY == dstY && unidirectionalTorus1D(nX)(srcX, dstX)) || (srcX == dstX && unidirectionalTorus1D(nY)(srcY, dstY))
  }

  def bidirectionalTorus2D(nX: Int, nY: Int): PhysicalTopology = (src: Int, dst: Int) => {
    val (srcX, srcY) = (src % nX, src / nX)
    val (dstX, dstY) = (dst % nX, dst / nX)
    (srcY == dstY && bidirectionalTorus1D(nX)(srcX, dstX)) || (srcX == dstX && bidirectionalTorus1D(nY)(srcY, dstY))
  }
}
