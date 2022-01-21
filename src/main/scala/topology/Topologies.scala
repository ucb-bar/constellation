package constellation.topology

import scala.math.{pow, cos, sin, Pi}

object Topologies {
  val unidirectionalLine = new PhysicalTopology((src, dest) => dest - src == 1, n => (n, 0))
  val bidirectionalLine = new PhysicalTopology((src, dest) => (dest - src).abs == 1, n => (n, 0))

  def unidirectionalTorus1D(nNodes: Int) = new PhysicalTopology(
    (src, dest) => dest - src == 1 || (dest == 0 && src == nNodes - 1),
    nodeId => {
      val rad = nodeId.toFloat * 2 * Pi / nNodes
      val r = nNodes.toFloat / (2 * Pi)
      ((cos(rad) * r).toInt, (sin(rad) * r).toInt)
    }
  )

  def bidirectionalTorus1D(nNodes: Int) = new PhysicalTopology(
    (src, dest) => (dest + nNodes - src) % nNodes == 1 || (src + nNodes - dest) % nNodes == 1,
    nodeId => {
      val rad = nodeId.toFloat * 2 * Pi / nNodes
      val r = nNodes.toFloat / (2 * Pi)
      ((cos(rad) * r).toInt, (sin(rad) * r).toInt)
    }
  )

  def butterfly(kAry: Int, nFly: Int) = {
    require(kAry >= 2 && nFly >= 2)
    val height = pow(kAry, nFly-1).toInt
    def digitsToNum(dig: Seq[Int]) = dig.zipWithIndex.map { case (d,i) => d * pow(kAry,i).toInt }.sum
    val table = (0 until pow(kAry, nFly).toInt).map { i =>
      (0 until nFly).map { n => (i / pow(kAry, n).toInt) % kAry }
    }
    val channels = (1 until nFly).map { i =>
      table.map { e => (digitsToNum(e.drop(1)), digitsToNum(e.updated(i, e(0)).drop(1))) }
    }
    new PhysicalTopology(
      (src, dest) => {
        val (srcX, srcY) = (src / height, src % height)
        val (destX, destY) = (dest / height, dest % height)
        if (srcX < nFly - 1 && destX == srcX + 1) {
          val connected = channels(srcX).contains((srcY, destY))
          connected
        } else {
          false
        }
      },
      nodeId => (nodeId / height, nodeId % height)
    )
  }

  def mesh2D(nX: Int, nY: Int) = new PhysicalTopology(
    (src, dst) => {
      val (srcX, srcY) = (src % nX, src / nX)
      val (dstX, dstY) = (dst % nX, dst / nX)
      (srcX == dstX && (srcY - dstY).abs == 1) || (srcY == dstY && (srcX - dstX).abs == 1)
    },
    nodeId => (nodeId % nX, nodeId / nX)
  )

  def unidirectionalTorus2D(nX: Int, nY: Int) = new PhysicalTopology(
    (src, dst) => {
      val (srcX, srcY) = (src % nX, src / nX)
      val (dstX, dstY) = (dst % nX, dst / nX)
      (srcY == dstY && unidirectionalTorus1D(nX)(srcX, dstX)) || (srcX == dstX && unidirectionalTorus1D(nY)(srcY, dstY))
    },
    nodeId => (nodeId % nX, nodeId / nX)
  )

  def bidirectionalTorus2D(nX: Int, nY: Int) = new PhysicalTopology(
    (src, dst) => {
      val (srcX, srcY) = (src % nX, src / nX)
      val (dstX, dstY) = (dst % nX, dst / nX)
      (srcY == dstY && bidirectionalTorus1D(nX)(srcX, dstX)) || (srcX == dstX && bidirectionalTorus1D(nY)(srcY, dstY))
    },
    nodeId => (nodeId % nX, nodeId / nX)
  )
}
