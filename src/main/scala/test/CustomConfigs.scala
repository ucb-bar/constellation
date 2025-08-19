package constellation.test

import constellation.noc._
import constellation.routing._
import constellation.topology._
import constellation.channel._
import org.chipsalliance.cde.config.Config

class CustomTestConfigBase(
  topology: PhysicalTopology,
  routingFn: PhysicalTopology => RoutingRelation,
  name: String,
  nSubnets: Int = 5,
  nDedicated: Int = 3
) extends NoCTesterConfig(
  NoCTesterParams(
    NoCParams(
      topology = topology,
      channelParamGen = (a, b) => UserChannelParams(Seq.fill(nSubnets*nDedicated) { UserVirtualChannelParams(4) }),
      ingresses = (0 until topology.nNodes).map(UserIngressParams(_)),
      egresses = (0 until topology.nNodes).map(UserEgressParams(_)),
      flows = Seq.tabulate(topology.nNodes, topology.nNodes) { (s, d) => FlowParams(s, d, 0) }.flatten,
      routingRelation = NonblockingVirtualSubnetworksRouting(routingFn, nSubnets, nDedicated)
    )
  )
)

class CustomTLTestConfigBase(
  topology: PhysicalTopology,
  routingFn: PhysicalTopology => RoutingRelation,
  name: String,
  nSubnets: Int = 5,
  nDedicated: Int = 3
) extends TLNoCTesterConfig(
  TLNoCTesterParams(
    inNodeMapping = (0 until topology.nNodes by 2),
    outNodeMapping = (1 until topology.nNodes by 2),
    nocParams = {
      val inNodes = (0 until topology.nNodes by 2)
      val outNodes = (1 until topology.nNodes by 2)
      val numChannels = nSubnets * nDedicated

      NoCParams(
        topology = topology,
        channelParamGen = (a, b) => UserChannelParams(Seq.fill(numChannels) { UserVirtualChannelParams(4) }),
        ingresses = inNodes.map(UserIngressParams(_)),
        egresses = outNodes.map(UserEgressParams(_)),
        flows = for {
          src <- inNodes
          dst <- outNodes
        } yield FlowParams(src, dst, 0),
        vNetBlocking = (a, b) => true,
        routingRelation = NonblockingVirtualSubnetworksRouting(routingFn, nSubnets, nDedicated)
      )
    }
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

}

import CustomTopologies._

// Unidirectional Ring
class TestConfigUniRingGD extends CustomTestConfigBase(
  uniRing,
  ShortestPathGeneralizedDatelineRouting(),
  "UniRingGD",
  nSubnets = 3,
  nDedicated = 2
)

class TLTestConfigUniRingGD extends CustomTLTestConfigBase(
  uniRing,
  ShortestPathGeneralizedDatelineRouting(),
  "TLUniRingGD",
  nSubnets = 5,
  nDedicated = 2
)

class TestConfigUniRingCL extends CustomTestConfigBase(
  uniRing,
  CustomLayeredRouting(),
  "UniRingCL",
  nSubnets = 3,
  nDedicated = 8
)	

class TLTestConfigUniRingCL extends CustomTLTestConfigBase(
  uniRing,
  CustomLayeredRouting(),
  "TLUniRingCL",
  nSubnets = 5,
  nDedicated = 8
)

// Bidirectional Ring
class TestConfigBiRingGD extends CustomTestConfigBase(
  biRing,
  ShortestPathGeneralizedDatelineRouting(),
  "BiRingGD",
  nSubnets = 3,
  nDedicated = 2
)	

class TLTestConfigBiRingGD extends CustomTLTestConfigBase(
  biRing,
  ShortestPathGeneralizedDatelineRouting(),
  "TLBiRingGD",
  nSubnets = 5,
  nDedicated = 2
)	

class TestConfigBiRingCL extends CustomTestConfigBase(
  biRing,
  CustomLayeredRouting(),
  "BiRingCL",
  nSubnets = 3,
  nDedicated = 4
)	

class TLTestConfigBiRingCL extends CustomTLTestConfigBase(
  biRing,
  CustomLayeredRouting(),
  "TLBiRingCL",
  nSubnets = 5,
  nDedicated = 4
)	

// Bypass NoC
class TestConfigBypassGD extends CustomTestConfigBase(
  bypass,
  ShortestPathGeneralizedDatelineRouting(),
  "BypassGD",
  nSubnets = 3,
  nDedicated = 3
)	

class TLTestConfigBypassGD extends CustomTLTestConfigBase(
  bypass,
  ShortestPathGeneralizedDatelineRouting(useStaticVC0=true),
  "TLBypassGD",
  nSubnets = 5,
  nDedicated = 3
)	

class TestConfigBypassCL extends CustomTestConfigBase(
  bypass,
  CustomLayeredRouting(),
  "BypassCL",
  nSubnets = 3,
  nDedicated = 6
)	

class TLTestConfigBypassCL extends CustomTLTestConfigBase(
  bypass,
  CustomLayeredRouting(),
  "TLBypassCL",
  nSubnets = 5,
  nDedicated = 6
)	

// Crossbar

class TestConfigCrossbarGD extends CustomTestConfigBase(
  crossbar,
  ShortestPathGeneralizedDatelineRouting(),
  "CrossbarGD",
  nSubnets = 3,
  nDedicated = 3
)

class TLTestConfigCrossbarGD extends CustomTLTestConfigBase(
  crossbar,
  ShortestPathGeneralizedDatelineRouting(),
  "TLCrossbarGD",
  nSubnets = 5,
  nDedicated = 3
)	

class TestConfigCrossbarCL extends CustomTestConfigBase(
  crossbar,
  CustomLayeredRouting(),
  "CrossbarCL",
  nSubnets = 3,
  nDedicated = 5
)

class TLTestConfigCrossbarCL extends CustomTLTestConfigBase(
  crossbar,
  CustomLayeredRouting(),
  "TLCrossbarCL",
  nSubnets = 5,
  nDedicated = 5
)	

// Mesh

class TestConfigMeshGD extends CustomTestConfigBase(
  fullMesh,
  ShortestPathGeneralizedDatelineRouting(),
  "MeshGD",
  nSubnets = 3,
  nDedicated = 1
)	

class TLTestConfigMeshGD extends CustomTLTestConfigBase(
  fullMesh,
  ShortestPathGeneralizedDatelineRouting(),
  "TLMeshGD",
  nSubnets = 5,
  nDedicated = 1
)		

class TestConfigMeshCL extends CustomTestConfigBase(
  fullMesh,
  CustomLayeredRouting(),
  "MeshCL",
  nSubnets = 3,
  nDedicated = 2
)	

class TLTestConfigMeshCL extends CustomTLTestConfigBase(
  fullMesh,
  CustomLayeredRouting(),
  "TLMeshCL",
  nSubnets = 5,
  nDedicated = 2
)	
