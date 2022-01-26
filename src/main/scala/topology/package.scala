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
  
  /** Abstract class for the network topology. See Topologies.scala for concrete network topologies.
   * 
   *  @param nNodes number of nodes in the network.
   */
  abstract class PhysicalTopology(val nNodes: Int) {
    /** Method that describes the particular topology represented by the concrete class. Returns true
     *  if the two nodes SRC and DST can be connected via a channel in this topology and false if they cannot.
     *
     *  @param src source point
     *  @param dst destination point
     */
    def topo(src: Int, dst: Int): Boolean
    
    /** Plotter from TopologyPlotters.scala. Helps construct diagram of a concrete topology. */
    val plotter: PhysicalTopologyPlotter
  }
}


