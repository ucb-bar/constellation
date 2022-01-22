package constellation

package object topology {
  // srcNodeId, destNodeId => Bool
  class PhysicalTopology(
    val topology: (Int, Int) => Boolean,
    val toXY: Int => (Int, Int)) {

    def apply(src: Int, dst: Int) = topology(src, dst)
  }
}


