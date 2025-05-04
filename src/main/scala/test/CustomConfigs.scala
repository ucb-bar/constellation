package constellation.test

import constellation.noc._
import constellation.routing._
import constellation.topology._
import constellation.channel._
import org.chipsalliance.cde.config.Config
import constellation.topology.{PhysicalTopology, CustomTopology}

class CustomTestConfigBase(
  topology: PhysicalTopology,
  routing: RoutingRelation,
  name: String,
  nVC: Int = 4
) extends NoCTesterConfig(
  NoCTesterParams(
    NoCParams(
      topology = topology,
      channelParamGen = (a, b) => UserChannelParams(Seq.fill(20) { UserVirtualChannelParams(nVC) }),
      ingresses = (0 until topology.nNodes).map(UserIngressParams(_)),
      egresses = (0 until topology.nNodes).map(UserEgressParams(_)),
      flows = Seq.tabulate(topology.nNodes, topology.nNodes) { (s, d) => FlowParams(s, d, 0) }.flatten,
      routingRelation = routing
    )
  )
)

class CustomTLTestConfigBase(
  topology: PhysicalTopology,
  routing: RoutingRelation,
  name: String,
  nVC: Int = 4
) extends TLNoCTesterConfig(
  TLNoCTesterParams(
    inNodeMapping = (0 until topology.nNodes by 2),
    outNodeMapping = (1 until topology.nNodes by 2),
    nocParams = NoCParams(
      topology = topology,
      channelParamGen = (a, b) => UserChannelParams(Seq.fill(20) { UserVirtualChannelParams(nVC) }),
      vNetBlocking = (a, b) => true,
      routingRelation = routing
    )
  )
)

object CustomTopologies {
  val n = 8

  val uniRing = CustomTopology(n, (0 until n).map(i => TopologyEdge(i, (i + 1) % n)))

  val biRing = CustomTopology(n, (0 until n).flatMap(i => Seq(
    TopologyEdge(i, (i + 1) % n),
    TopologyEdge(i, (i - 1 + n) % n)
  )))

  val bypass = CustomTopology(n, (0 until n).flatMap(i => Seq(
    TopologyEdge(i, (i + 1) % n),
    TopologyEdge(i, (i - 1 + n) % n)
  )) ++ Seq((0, 5), (5, 0), (1, 4), (4, 1)).map { case (a, b) => TopologyEdge(a, b) })

  val crossbar = CustomTopology(n, (0 until n).flatMap(i => Seq(
    TopologyEdge(i, (i + 1) % n),
    TopologyEdge(i, (i - 1 + n) % n)
  )) ++ Seq((0, 4), (4, 0), (1, 4), (4, 1), (0, 5), (5, 0), (1, 5), (5, 1)).map {
    case (a, b) => TopologyEdge(a, b)
  })

  val fullMesh = CustomTopology(n, for {
    i <- 0 until n; j <- 0 until n if i != j
  } yield TopologyEdge(i, j))

  def routes = Seq(
    ("GD", ShortestPathGeneralizedDatelineRouting()),
    ("CL", CustomLayeredRouting())
  )
}

// import CustomTopologies._

class CustomTestConfigUniRingGD extends CustomTestConfigBase(uniRing, NonblockingVirtualSubnetworksRouting(ShortestPathGeneralizedDatelineRouting(), 5, 4), "UniRingGD")
class CustomTestConfigUniRingCL extends CustomTestConfigBase(uniRing, NonblockingVirtualSubnetworksRouting(CustomLayeredRouting(), 5, 8), "UniRingCL")

class CustomTestConfigBiRingGD extends CustomTestConfigBase(biRing, NonblockingVirtualSubnetworksRouting(ShortestPathGeneralizedDatelineRouting(), 5, 4), "BiRingGD")
class CustomTestConfigBiRingCL extends CustomTestConfigBase(biRing, NonblockingVirtualSubnetworksRouting(CustomLayeredRouting(), 5, 4), "BiRingCL")
class CustomTestConfigBypassGD extends CustomTestConfigBase(bypass, NonblockingVirtualSubnetworksRouting(ShortestPathGeneralizedDatelineRouting(), 5, 4), "BypassGD")
class CustomTestConfigBypassCL extends CustomTestConfigBase(bypass, NonblockingVirtualSubnetworksRouting(CustomLayeredRouting(), 5, 4), "BypassCL")
class CustomTestConfigCrossbarGD extends CustomTestConfigBase(crossbar, NonblockingVirtualSubnetworksRouting(ShortestPathGeneralizedDatelineRouting(), 5, 4), "CrossbarGD")
class CustomTestConfigCrossbarCL extends CustomTestConfigBase(crossbar, NonblockingVirtualSubnetworksRouting(CustomLayeredRouting(), 5, 4), "CrossbarCL")
class CustomTestConfigMeshGD extends CustomTestConfigBase(fullMesh, NonblockingVirtualSubnetworksRouting(ShortestPathGeneralizedDatelineRouting(), 5, 4), "MeshGD")
class CustomTestConfigMeshCL extends CustomTestConfigBase(fullMesh, NonblockingVirtualSubnetworksRouting(CustomLayeredRouting(), 5, 4), "MeshCL")

class CustomTLTestConfigUniRingGD extends CustomTLTestConfigBase(uniRing, NonblockingVirtualSubnetworksRouting(ShortestPathGeneralizedDatelineRouting(), 5, 4), "TLUniRingGD")
class CustomTLTestConfigUniRingCL extends CustomTLTestConfigBase(uniRing, NonblockingVirtualSubnetworksRouting(CustomLayeredRouting(), 5, 4), "TLUniRingCL")
class CustomTLTestConfigBiRingGD extends CustomTLTestConfigBase(biRing, NonblockingVirtualSubnetworksRouting(ShortestPathGeneralizedDatelineRouting(), 5, 4), "TLBiRingGD")
class CustomTLTestConfigBiRingCL extends CustomTLTestConfigBase(biRing, NonblockingVirtualSubnetworksRouting(CustomLayeredRouting(), 5, 4), "TLBiRingCL")
class CustomTLTestConfigBypassGD extends CustomTLTestConfigBase(bypass, NonblockingVirtualSubnetworksRouting(ShortestPathGeneralizedDatelineRouting(), 5, 4), "TLBypassGD")
class CustomTLTestConfigBypassCL extends CustomTLTestConfigBase(bypass, NonblockingVirtualSubnetworksRouting(CustomLayeredRouting(), 5, 4), "TLBypassCL")
class CustomTLTestConfigCrossbarGD extends CustomTLTestConfigBase(crossbar, NonblockingVirtualSubnetworksRouting(ShortestPathGeneralizedDatelineRouting(), 5, 4), "TLCrossbarGD")
class CustomTLTestConfigCrossbarCL extends CustomTLTestConfigBase(crossbar, NonblockingVirtualSubnetworksRouting(CustomLayeredRouting(), 5, 4), "TLCrossbarCL")
class CustomTLTestConfigMeshGD extends CustomTLTestConfigBase(fullMesh, NonblockingVirtualSubnetworksRouting(ShortestPathGeneralizedDatelineRouting(), 5, 4), "TLMeshGD")
class CustomTLTestConfigMeshCL extends CustomTLTestConfigBase(fullMesh, NonblockingVirtualSubnetworksRouting(CustomLayeredRouting(), 5, 4), "TLMeshCL")
