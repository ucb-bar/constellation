package constellation

package object topology {
  // srcNodeId, destNodeId => Bool
  class PhysicalTopology(
    topology: (Int, Int) => Boolean,
    toXY: Int => (Int, Int)) {

    def apply(src: Int, dst: Int) = topology(src, dst)
    def xy(n: Int) = toXY(n)
  }
}


