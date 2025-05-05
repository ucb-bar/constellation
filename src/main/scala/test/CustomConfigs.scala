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
  nVC: Int = 4
) extends NoCTesterConfig(
  NoCTesterParams(
    NoCParams(
      topology = topology,
      channelParamGen = (a, b) => UserChannelParams(Seq.fill(20) { UserVirtualChannelParams(nVC) }),
      ingresses = (0 until topology.nNodes).map(UserIngressParams(_)),
      egresses = (0 until topology.nNodes).map(UserEgressParams(_)),
      flows = Seq.tabulate(topology.nNodes, topology.nNodes) { (s, d) => FlowParams(s, d, 0) }.flatten,
      routingRelation = routingFn
    )
  )
)

class CustomTLTestConfigBase(
  topology: PhysicalTopology,
  routingFn: PhysicalTopology => RoutingRelation,
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
      routingRelation = routingFn
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

  def makeRouting(f: PhysicalTopology => RoutingRelation, n: Int = 5, nDedicated: Int = 4): PhysicalTopology => RoutingRelation = {
    topo => NonblockingVirtualSubnetworksRouting(f, n, nDedicated)(topo)
  }

  def makeEscapeRouting(f: PhysicalTopology => RoutingRelation, n: Int = 5, nDedicated: Int = 4): PhysicalTopology => RoutingRelation = {
    topo => NonblockingVirtualSubnetworksRouting(
      EscapeChannelRouting(
        escapeRouter = f,
        normalRouter = ShortestPathRouting(maxVCs = 1),
        nEscapeChannels = 3
      ),
      n, nDedicated
    )(topo)
  }
}

import CustomTopologies._

class CustomTestConfigUniRingGD extends CustomTestConfigBase(uniRing, makeRouting(ShortestPathGeneralizedDatelineRouting()), "UniRingGD")
class CustomTestConfigUniRingCL extends CustomTestConfigBase(uniRing, makeRouting(CustomLayeredRouting()), "UniRingCL")
class CustomTestConfigBiRingGD extends CustomTestConfigBase(biRing, makeRouting(ShortestPathGeneralizedDatelineRouting()), "BiRingGD")
class CustomTestConfigBiRingCL extends CustomTestConfigBase(biRing, makeRouting(CustomLayeredRouting()), "BiRingCL")

class CustomTestConfigBypassGD extends CustomTestConfigBase(bypass, makeRouting(ShortestPathGeneralizedDatelineRouting()), "BypassGD")
class CustomTestConfigBypassCL extends CustomTestConfigBase(bypass, makeRouting(CustomLayeredRouting()), "BypassCL")
class CustomTestConfigBypassSP extends CustomTestConfigBase(bypass, makeRouting(ShortestPathRouting(maxVCs=4)), "BypassCL")


class CustomTestConfigCrossbarGD extends CustomTestConfigBase(crossbar, makeRouting(ShortestPathGeneralizedDatelineRouting()), "CrossbarGD")
class CustomTestConfigCrossbarCL extends CustomTestConfigBase(crossbar, makeRouting(CustomLayeredRouting()), "CrossbarCL")
class CustomTestConfigMeshGD extends CustomTestConfigBase(fullMesh, makeRouting(ShortestPathGeneralizedDatelineRouting()), "MeshGD")
class CustomTestConfigMeshCL extends CustomTestConfigBase(fullMesh, makeRouting(CustomLayeredRouting()), "MeshCL")


class CustomTLTestConfigUniRingGD extends CustomTLTestConfigBase(uniRing, makeRouting(ShortestPathGeneralizedDatelineRouting()), "TLUniRingGD")
class CustomTLTestConfigUniRingCL extends CustomTLTestConfigBase(uniRing, makeRouting(CustomLayeredRouting()), "TLUniRingCL")

class CustomTLTestConfigBiRingGD extends CustomTLTestConfigBase(biRing, makeRouting(ShortestPathGeneralizedDatelineRouting()), "TLBiRingGD")
class CustomTLTestConfigBiRingCL extends CustomTLTestConfigBase(biRing, makeRouting(CustomLayeredRouting()), "TLBiRingCL")
class CustomTLTestConfigBypassGD extends CustomTLTestConfigBase(bypass, makeRouting(ShortestPathGeneralizedDatelineRouting()), "TLBypassGD")
class CustomTLTestConfigBypassCL extends CustomTLTestConfigBase(bypass, makeRouting(CustomLayeredRouting()), "TLBypassCL")
class CustomTLTestConfigCrossbarGD extends CustomTLTestConfigBase(crossbar, makeRouting(ShortestPathGeneralizedDatelineRouting()), "TLCrossbarGD")
class CustomTLTestConfigCrossbarCL extends CustomTLTestConfigBase(crossbar, makeRouting(CustomLayeredRouting()), "TLCrossbarCL")
class CustomTLTestConfigMeshGD extends CustomTLTestConfigBase(fullMesh, makeRouting(ShortestPathGeneralizedDatelineRouting()), "TLMeshGD")
class CustomTLTestConfigMeshCL extends CustomTLTestConfigBase(fullMesh, makeRouting(CustomLayeredRouting()), "TLMeshCL")


class TLTestCustomConfig00 extends TLNoCTesterConfig(TLNoCTesterParams(
  inNodeMapping = Seq(8),
  outNodeMapping = Seq(8),
  nocParams = NoCParams(
    topology        = uniRing,
    channelParamGen = (a, b) => UserChannelParams(Seq.fill(20) { UserVirtualChannelParams(4) }),
    vNetBlocking    = (blocker, blockee) => true,
    routingRelation = NonblockingVirtualSubnetworksRouting(CustomLayeredRouting(), 5, 8)
  )
))

class CustomTestConfigCrossbarEGD extends CustomTestConfigBase(crossbar, makeEscapeRouting(ShortestPathGeneralizedDatelineRouting()), "CrossbarCL")
class CustomTestConfigCrossbarECL extends CustomTestConfigBase(crossbar, makeEscapeRouting(CustomLayeredRouting()), "CrossbarCL")
