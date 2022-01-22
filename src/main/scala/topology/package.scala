package constellation

package object topology {

  abstract class PhysicalTopology {
    def topo(src: Int, dst: Int): Boolean
    def toXY(n: Int): (Int, Int)
  }
}


