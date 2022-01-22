package constellation

package object topology {

  abstract class PhysicalTopologyPlotter {
    def node(nodeId: Int): (Double, Double) = node(nodeId.toDouble)
    def ingress(iId: Int, nI: Int, nodeId: Int): (Double, Double) = ingress(iId.toDouble, nI.toDouble, nodeId.toDouble)
    def egress (eId: Int, nE: Int, nodeId: Int): (Double, Double) =  egress(eId.toDouble, nE.toDouble, nodeId.toDouble)
    def node(nodeId: Double): (Double, Double)
    def ingress(iId: Double, nI: Double, nodeId: Double): (Double, Double)
    def egress (eId: Double, nE: Double, nodeId: Double): (Double, Double)
  }

  abstract class PhysicalTopology {
    def topo(src: Int, dst: Int): Boolean
    val plotter: PhysicalTopologyPlotter
  }
}


