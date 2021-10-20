package constellation.topology

import scala.math.pow

object RoutingAlgorithms {
  def crazy(nodeId: Int)(lastId: Int, destId: Int, nextId: Int, prio: Int) = true

  def bidirectionalLine(nodeId: Int)(lastId: Int, destId: Int, nextId: Int, prio: Int) = {
    (if (nodeId < nextId) destId >= nextId else destId <= nextId) && nextId != lastId
  }

  def bidirectionalTorus1DShortest(nNodes: Int)(nodeId: Int)(lastId: Int, destId: Int, nextId: Int, prio: Int) = {
    val cwDist = (destId + nNodes - nodeId) % nNodes
    val ccwDist = (nodeId + nNodes - destId) % nNodes
    if (cwDist < ccwDist) {
      (nextId + nNodes - nodeId) % nNodes == 1
    } else if (cwDist > ccwDist) {
      (nodeId + nNodes - nextId) % nNodes == 1
    } else {
      true
    }
  }
  def bidirectionalTorus1DRandom(nNodes: Int)(nodeId: Int)(lastId: Int, destId: Int, nextId: Int, prio: Int) = {
    if (lastId == -1) {
      true
    } else if ((nodeId + nNodes - lastId) % nNodes == 1) {
      (nextId + nNodes - nodeId) % nNodes == 1
    } else {
      (nodeId + nNodes - nextId) % nNodes == 1
    }
  }

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

    (nodeId: Int) => (lastId: Int, destId: Int, nextId: Int, prio: Int) => {
      val (nextX, nextY) = (nextId / height, nextId % height)
      val (nodeX, nodeY) = (nodeId / height, nodeId % height)
      val (destX, destY) = (destId / height, destId % height)
      if (destX <= nodeX) {
        false
      } else if (nodeX == nFly-1) {
        true
      } else {
        val dests = (nextX until nFly-1).foldRight((0 until height).map { i => Seq(i) }) {
          case (i,l) => (0 until height).map { s => channels(i).filter(_._1 == s).map { case (_,d) =>
            l(d)
          }.flatten }
        }
        dests(nextY).contains(destId % height)
      }
    }
  }

  def mesh2DDimensionOrdered(nX: Int, nY: Int)(nodeId: Int)(lastId: Int, destId: Int, nextId: Int, prio: Int) = {
    val (nextX, nextY) = (nextId / nX, nextId % nX)
    val (nodeX, nodeY) = (nodeId / nX, nodeId % nX)
    val (destX, destY) = (destId / nX, destId % nX)
    val (lastX, lastY) = (lastId / nX, lastId % nX)

    if (destX != nodeX) {
      (if (nodeX < nextX) destX >= nextX else destX <= nextX) && nextY == nodeY
    } else {
      (if (nodeY < nextY) destY >= nextY else destY <= nextY) && nextX == nodeX
    }
  }

  // Minimal routing. Causes deadlocks.
  def mesh2DMinimal(nX: Int, nY: Int)(nodeId: Int)(lastId: Int, destId: Int, nextId: Int, prio: Int) = {
    val (nextX, nextY) = (nextId / nX, nextId % nX)
    val (nodeX, nodeY) = (nodeId / nX, nodeId % nX)
    val (destX, destY) = (destId / nX, destId % nX)
    val (lastX, lastY) = (lastId / nX, lastId % nX)

    val xR = (if (nodeX < nextX) destX >= nextX else if (nodeX > nextX) destX <= nextX else nodeX == nextX)
    val yR = (if (nodeY < nextY) destY >= nextY else if (nodeY > nextY) destY <= nextY else nodeY == nextY)
    xR && yR
  }

  def mesh2DWestFirst(nX: Int, nY: Int)(nodeId: Int)(lastId: Int, destId: Int, nextId: Int, prio: Int) = {
    val (nextX, nextY) = (nextId / nX, nextId % nX)
    val (nodeX, nodeY) = (nodeId / nX, nodeId % nX)
    val (destX, destY) = (destId / nX, destId % nX)
    val (lastX, lastY) = (lastId / nX, lastId % nX)

    if (destX < nodeX) {
      nextX == nodeX - 1
    } else {
      mesh2DMinimal(nX, nY)(nodeId)(lastId, destId, nextId, prio)
    }
  }

  def mesh2DNorthLast(nX: Int, nY: Int)(nodeId: Int)(lastId: Int, destId: Int, nextId: Int, prio: Int) = {
    val (nextX, nextY) = (nextId / nX, nextId % nX)
    val (nodeX, nodeY) = (nodeId / nX, nodeId % nX)
    val (destX, destY) = (destId / nX, destId % nX)
    val (lastX, lastY) = (lastId / nX, lastId % nX)

    if (destY > nodeY && destX != nodeX) {
      mesh2DMinimal(nX, nY)(nodeId)(lastId, destId, nextId, prio) && nextY != nodeY + 1
    } else if (destY > nodeY) {
      nextY == nodeY + 1
    } else {
      mesh2DMinimal(nX, nY)(nodeId)(lastId, destId, nextId, prio)
    }
  }


  def dimensionOrderedUnidirectionalTorus2D(nX: Int, nY: Int)(nodeId: Int)(lastId: Int, destId: Int, nextId: Int, prio: Int) = {
    val (nextX, nextY) = (nextId / nX, nextId % nX)
    val (nodeX, nodeY) = (nodeId / nX, nodeId % nX)
    val (destX, destY) = (destId / nX, destId % nX)
    val (lastX, lastY) = (lastId / nX, lastId % nX)

    if (destX != nodeX) {
      nextY == nodeY
    } else {
      nextX == nodeX
    }
  }

  def dimensionOrderedBidirectionalTorus2D(nX: Int, nY: Int)(nodeId: Int)(lastId: Int, destId: Int, nextId: Int, prio: Int) = {
    val (nextX, nextY) = (nextId / nX, nextId % nX)
    val (nodeX, nodeY) = (nodeId / nX, nodeId % nX)
    val (destX, destY) = (destId / nX, destId % nX)
    val (lastX, lastY) = (lastId / nX, lastId % nX)

    if (destX != nodeX) {
      bidirectionalTorus1DShortest(nX)(nodeX)(lastX, destX, nextX, prio)
    } else {
      bidirectionalTorus1DShortest(nY)(nodeY)(lastY, destY, nextY, prio)
    }
  }
}
