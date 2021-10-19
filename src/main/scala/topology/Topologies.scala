package constellation.topology


object Topologies {
  def unidirectionalLine(src: Int, dest: Int) = dest - src == 1
  def bidirectionalLine(src: Int, dest: Int) = (dest - src).abs == 1

  def unidirectionalTorus1D(nNodes: Int)(src: Int, dest: Int) = {
    dest - src == 1 || (dest == 0 && src == nNodes - 1)
  }
  def bidirectionalTorus1D(nNodes: Int)(src: Int, dest: Int) = {
    (dest + nNodes - src) % nNodes == 1 || (src + nNodes - dest) % nNodes == 1
  }
}
