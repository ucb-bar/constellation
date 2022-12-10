package constellation.test

import freechips.rocketchip.config.{Field, Parameters, Config}
import constellation.routing._
import constellation.topology._
import constellation.noc.{NoCParams}
import constellation.channel._
import constellation.router._
import scala.collection.immutable.ListMap
import scala.math.{floor, log10, pow, max}


class NoCTesterConfig(p: NoCTesterParams) extends Config((site, here, up) => {
  case NoCTesterKey => p
})

class TLNoCTesterConfig(p: TLNoCTesterParams) extends Config((site, here, up) => {
  case TLNoCTesterKey => p
})

class AXI4NoCTesterConfig(p: AXI4NoCTesterParams) extends Config((site, here, up) => {
  case AXI4NoCTesterKey => p
})

class NoCEvalConfig(p: NoCEvalParams) extends Config((site, here, up) => {
  case NoCEvalKey => p
})

class TestConfig00 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = UnidirectionalLine(2),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(3) { UserVirtualChannelParams(3) }),
  ingresses       = Seq(0).map { i => UserIngressParams(i) },
  egresses        = Seq(1).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(1, 1) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = UnidirectionalLineRouting()
)))
class TestConfig01 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = UnidirectionalLine(2),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(3) { UserVirtualChannelParams(3) }),
  ingresses       = Seq(0).map { i => UserIngressParams(i) },
  egresses        = Seq(1, 1).map { i => UserEgressParams(1) },
  flows           = Seq.tabulate(1, 2) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = UnidirectionalLineRouting()
)))
class TestConfig02 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = UnidirectionalLine(2),
  channelParamGen = (a, b) => UserChannelParams(
    virtualChannelParams = Seq.fill(3) { UserVirtualChannelParams(3) },
    channelGen = (u: Parameters) => {
      implicit val p: Parameters = u
      ChannelBuffer(2) := _
    }
  ),
  ingresses       = Seq(0, 0).map { i => UserIngressParams(i) },
  egresses        = Seq(1, 1).map { i => UserEgressParams(1) },
  flows           = Seq.tabulate(2, 2) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = UnidirectionalLineRouting()
)))
class TestConfig03 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = UnidirectionalLine(2),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(3) { UserVirtualChannelParams(3) }),
  ingresses       = Seq(0, 0).map { i => UserIngressParams(i) },
  egresses        = Seq(0, 1, 1).map { i => UserEgressParams(1) },
  flows           = Seq.tabulate(2, 3) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = UnidirectionalLineRouting()
)))
class TestConfig04 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = UnidirectionalLine(3, Seq((0, 2))),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(3) { UserVirtualChannelParams(3) }),
  ingresses       = Seq(0, 0).map { i => UserIngressParams(i) },
  egresses        = Seq(0, 1, 1, 2, 2).map { i => UserEgressParams(1) },
  flows           = Seq.tabulate(2, 5) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = UnidirectionalLineRouting()
)))
class TestConfig05 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = UnidirectionalLine(3),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(3) { UserVirtualChannelParams(3) }),
  ingresses       = Seq(0, 1, 1).map { i => UserIngressParams(i) },
  egresses        = Seq(1, 1, 2).map { i => UserEgressParams(1) },
  flows           = Seq.tabulate(3, 3) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = UnidirectionalLineRouting()
)))
class TestConfig06 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = UnidirectionalLine(3),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
  routerParams    = (i) => UserRouterParams(coupleSAVA=true),
  ingresses       = Seq(0).map { i => UserIngressParams(i) },
  egresses        = Seq(1).map { i => UserEgressParams(1) },
  flows           = Seq.tabulate(1, 1) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = UnidirectionalLineRouting()
)))
class TestConfig07 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = UnidirectionalLine(2),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
  routerParams    = (i) => UserRouterParams(coupleSAVA=true),
  ingresses       = Seq(0).map { i => UserIngressParams(i) },
  egresses        = Seq(1).map { i => UserEgressParams(1) },
  flows           = Seq.tabulate(1, 1) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = UnidirectionalLineRouting(),
  hasCtrl         = true
)))
class TestConfig08 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = UnidirectionalLine(7, Seq((0, 2), (2, 4), (4, 6))),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
  routerParams    = (i) => UserRouterParams(
    vcAllocator = (vP) => (p) => new PrioritizingSingleVCAllocator(vP)(p),
    coupleSAVA=true),
  ingresses       = Seq(0).map { i => UserIngressParams(i) },
  egresses        = Seq(1, 2, 3, 4, 5).map { i => UserEgressParams(1) },
  flows           = Seq.tabulate(1, 5) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = UnidirectionalLineRouting()
)))
class TestConfig09 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = BidirectionalLine(2),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(3) { UserVirtualChannelParams(3) }),
  ingresses       = (0 until 2).map { i => UserIngressParams(i) },
  egresses        = (0 until 2).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(2, 2) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = BidirectionalLineRouting()
)))
class TestConfig10 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = BidirectionalLine(3),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(3) { UserVirtualChannelParams(3) }),
  ingresses       = (0 until 2).map { i => UserIngressParams(i) },
  egresses        = (0 until 3).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(2, 3) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = BidirectionalLineRouting()
)))
class TestConfig11 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = BidirectionalLine(4),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(3) { UserVirtualChannelParams(3) }),
  ingresses       = (1 until 3).map { i => UserIngressParams(i) },
  egresses        = (0 until 4).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(2, 4) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = BidirectionalLineRouting()
)))
class TestConfig12 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = BidirectionalLine(4),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(3) { UserVirtualChannelParams(3) }),
  ingresses       = Seq(1, 1, 2, 2).map { i => UserIngressParams(i) },
  egresses        = Seq(0, 0, 1, 1, 2, 2, 3, 3).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(4, 8) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = BidirectionalLineRouting()
)))
class TestConfig13 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = UnidirectionalTorus1D(2),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
  ingresses       = Seq(0).map { i => UserIngressParams(i) },
  egresses        = Seq(1).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(1, 1) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = UnidirectionalTorus1DDatelineRouting()
)))
class TestConfig14 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = UnidirectionalTorus1D(4),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
  ingresses       = Seq(0, 2).map { i => UserIngressParams(i) },
  egresses        = Seq(1, 3).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(2, 2) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = UnidirectionalTorus1DDatelineRouting()
)))
class TestConfig15 extends NoCTesterConfig(NoCTesterParams(
  NoCParams(
    topology        = UnidirectionalTorus1D(10),
    channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
    ingresses       = (0 until 10 by 2).map { i => UserIngressParams(i) },
    egresses        = (1 until 10 by 2).map { i => UserEgressParams(i) },
    flows           = Seq.tabulate(5, 5) { (s, d) => FlowParams(s, d, 0) }.flatten,
    routingRelation = UnidirectionalTorus1DDatelineRouting()
  ),
  inputPacketStallProbability = 0.9
))
class TestConfig16 extends NoCTesterConfig(NoCTesterParams(
  NoCParams(
    topology        = UnidirectionalTorus1D(10),
    channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
    routerParams    = (i) => UserRouterParams(
      vcAllocator = (vP) => (p) => new PrioritizingSingleVCAllocator(vP)(p)
    ),
    ingresses       = (0 until 10).map { i => UserIngressParams(i) },
    egresses        = (0 until 10).map { i => UserEgressParams(i) },
    flows           = Seq.tabulate(10, 10) { (s, d) => FlowParams(s, d, 0) }.flatten,
    routingRelation = UnidirectionalTorus1DDatelineRouting()
  ),
  inputPacketStallProbability = 0.95
))
class TestConfig17 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = BidirectionalTorus1D(2),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
  ingresses       = (0 until 2).map { i => UserIngressParams(i) },
  egresses        = (0 until 2).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(2, 2) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = BidirectionalTorus1DShortestRouting()
)))
class TestConfig18 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = BidirectionalTorus1D(4),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
  ingresses       = (0 until 4 by 2).map { i => UserIngressParams(i) },
  egresses        = (1 until 4 by 2).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(2, 2) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = BidirectionalTorus1DShortestRouting()
)))
class TestConfig19 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = BidirectionalTorus1D(10),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
  ingresses       = (0 until 10).map { i => UserIngressParams(i) },
  egresses        = (0 until 10).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(10, 10) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = BidirectionalTorus1DShortestRouting()
)))
class TestConfig20 extends NoCTesterConfig(NoCTesterParams(
  NoCParams(
    topology        = BidirectionalTorus1D(10),
    channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
    ingresses       = (0 until 10).map { i => UserIngressParams(i) },
    egresses        = (0 until 10).map { i => UserEgressParams(i) },
    flows           = Seq.tabulate(10, 10) { (s, d) => FlowParams(s, d, 0) }.flatten,
    routingRelation = BidirectionalTorus1DRandomRouting()
  ),
  inputFlitStallProbability = 0.9
))
class TestConfig21 extends NoCTesterConfig(NoCTesterParams(
  NoCParams(
    topology        = BidirectionalTorus1D(10),
    channelParamGen = (a, b) => UserChannelParams(
      virtualChannelParams = Seq.fill(4) { UserVirtualChannelParams(5) },
      srcSpeedup = 2,
      destSpeedup = 2
    ),
    ingresses       = (0 until 20).map { i => UserIngressParams(i % 10) },
    egresses        = (0 until 20).map { i => UserEgressParams(i % 10) },
    flows           = Seq.tabulate(20, 20) { (s, d) => FlowParams(s, d, 0) }.flatten,
    routingRelation = BidirectionalTorus1DRandomRouting()
  ),
  inputPacketStallProbability = 0.95
))
class TestConfig22 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = Butterfly(2, 2),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(1) { UserVirtualChannelParams(5) }),
  ingresses       = (0 until 4).map { i => UserIngressParams(i % 2) },
  egresses        = (0 until 4).map { i => UserEgressParams((i % 2) + 2 * 1) },
  flows           = Seq.tabulate(4, 4) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = ButterflyRouting()
)))
class TestConfig23 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = Butterfly(2, 3),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(1) { UserVirtualChannelParams(5) }),
  ingresses       = (0 until 8).map { i => UserIngressParams(i % 4) },
  egresses        = (0 until 8).map { i => UserEgressParams((i % 4) + 4 * 2) },
  flows           = Seq.tabulate(8, 8) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = ButterflyRouting()
)))
class TestConfig24 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = Butterfly(2, 4),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(1) { UserVirtualChannelParams(5) }),
  ingresses       = (0 until 16).map { i => UserIngressParams(i % 8) },
  egresses        = (0 until 16).map { i => UserEgressParams((i % 8) + 8 * 3) },
  flows           = Seq.tabulate(16, 16) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = ButterflyRouting()
)))
class TestConfig25 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = Butterfly(3, 2),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(1) { UserVirtualChannelParams(5) }),
  ingresses       = (0 until 6).map { i => UserIngressParams(i % 3) },
  egresses        = (0 until 6).map { i => UserEgressParams((i % 3) + 3 * 1) },
  flows           = Seq.tabulate(6, 6) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = ButterflyRouting()
)))
class TestConfig26 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = Butterfly(3, 3),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(1) { UserVirtualChannelParams(5) }),
  ingresses       = (0 until 18).map { i => UserIngressParams(i % 9) },
  egresses        = (0 until 18).map { i => UserEgressParams((i % 9) + 9 * 2) },
  flows           = Seq.tabulate(18, 18) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = ButterflyRouting()
)))
class TestConfig27 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = BidirectionalTree(3, 2),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(1) { UserVirtualChannelParams(3) }),
  ingresses       = (0 until 6 by 2).map { i => UserIngressParams(i) },
  egresses        = (1 until 6 by 2).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(3, 3) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = BidirectionalTreeRouting()
)))
class TestConfig28 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = BidirectionalTree(3, 2),
  channelParamGen = (a, b) => {
    val height = 3
    val dAry = 2
    def level(i: Int) = floor(log10(i + 1) / log10(dAry))
    val mult = pow(2, height - max(level(a), level(b))).toInt
    UserChannelParams(
      Seq.fill(mult) { UserVirtualChannelParams(3) },
      srcSpeedup = mult,
      destSpeedup = mult
    )
  },
  ingresses       = (0 until 6).map { i => UserIngressParams(i) },
  egresses        = (0 until 6).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(6, 6) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = BidirectionalTreeRouting()
)))
class TestConfig29 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = BidirectionalTree(3, 3),
  channelParamGen = (a, b) => {
    val height = 3
    val dAry = 3
    def level(i: Int) = floor(log10(i + 1) / log10(dAry))
    val mult = pow(2, height - max(level(a), level(b))).toInt
    UserChannelParams(
      Seq.fill(mult) { UserVirtualChannelParams(3) },
      srcSpeedup = mult,
      destSpeedup = mult
    )
  },
  ingresses       = (13 until 40).map { i => UserIngressParams(i) },
  egresses        = (13 until 40).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(27, 27) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = BidirectionalTreeRouting()
)))
class TestConfig30 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = Mesh2D(3, 3),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
  ingresses       = (0 until 9).map { i => UserIngressParams(i) },
  egresses        = (0 until 9).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(9, 9) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = Mesh2DDimensionOrderedRouting()
)))
class TestConfig31 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = Mesh2D(3, 6),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
  ingresses       = (0 until 18).map { i => UserIngressParams(i) },
  egresses        = (0 until 18).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(18, 18) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = Mesh2DDimensionOrderedRouting()
)))
class TestConfig32 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = Mesh2D(5, 5),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
  ingresses       = (0 until 25).map { i => UserIngressParams(i) },
  egresses        = (0 until 25).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(25, 25) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = Mesh2DDimensionOrderedRouting()
)))
class TestConfig33 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = Mesh2D(5, 5),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
  ingresses       = (0 until 25).map { i => UserIngressParams(i) },
  egresses        = (0 until 25).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(25, 25) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = Mesh2DDimensionOrderedRouting(firstDim=1)
)))
class TestConfig34 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = Mesh2D(5, 5),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
  routerParams    = (i) => UserRouterParams(
    vcAllocator = (vP) => (p) => new PrioritizingSingleVCAllocator(vP)(p)
  ),
  ingresses       = (0 until 25).map { i => UserIngressParams(i) },
  egresses        = (0 until 25).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(25, 25) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = Mesh2DEscapeRouting()
)))
class TestConfig35 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = Mesh2D(5, 5),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
  routerParams    = (i) => UserRouterParams(
    vcAllocator = (vP) => (p) => new PIMMultiVCAllocator(vP)(p)
  ),
  ingresses       = (0 until 25).map { i => UserIngressParams(i) },
  egresses        = (0 until 25).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(25, 25) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = Mesh2DEscapeRouting()
)))
class TestConfig36 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = Mesh2D(3, 3),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(2) { UserVirtualChannelParams(2) }),
  ingresses       = (0 until 9).map { i => UserIngressParams(i) },
  egresses        = (0 until 9).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(9, 9) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = Mesh2DEscapeRouting()
)))
class TestConfig37 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = Mesh2D(3, 3),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(2) { UserVirtualChannelParams(2) }),
  routerParams    = (i) => UserRouterParams(combineRCVA=true),
  ingresses       = (0 until 9).map { i => UserIngressParams(i) },
  egresses        = (0 until 9).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(9, 9) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = Mesh2DEscapeRouting()
)))
class TestConfig38 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = Mesh2D(3, 3),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(2) { UserVirtualChannelParams(2) }),
  routerParams    = (i) => UserRouterParams(combineSAST=true),
  ingresses       = (0 until 9).map { i => UserIngressParams(i) },
  egresses        = (0 until 9).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(9, 9) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = Mesh2DEscapeRouting()
)))
class TestConfig39 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = Mesh2D(3, 3),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(2) { UserVirtualChannelParams(2) }),
  routerParams    = (i) => UserRouterParams(combineSAST=true, combineRCVA=true),
  ingresses       = (0 until 9).map { i => UserIngressParams(i) },
  egresses        = (0 until 9).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(9, 9) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = Mesh2DEscapeRouting()
)))
class TestConfig40 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = Mesh2D(3, 3),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(2) { UserVirtualChannelParams(2) }),
  routerParams    = (i) => UserRouterParams(
    vcAllocator = (vP) => (p) => new ISLIPMultiVCAllocator(vP)(p)
  ),
  ingresses       = (0 until 9).map { i => UserIngressParams(i) },
  egresses        = (0 until 9).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(9, 9) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = Mesh2DEscapeRouting()
)))
class TestConfig41 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = Mesh2D(4, 4),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(2) { UserVirtualChannelParams(2) }),
  ingresses       = (0 until 16).map { i => UserIngressParams(i) },
  egresses        = (0 until 16).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(16, 16) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = Mesh2DEscapeRouting()
)))
class TestConfig42 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = Mesh2D(4, 4),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(2) { UserVirtualChannelParams(2) },
    useOutputQueues = false),
  routerParams    = (i) => UserRouterParams(combineRCVA=true, combineSAST=true),
  ingresses       = (0 until 16).map { i => UserIngressParams(i) },
  egresses        = (0 until 16).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(16, 16) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = Mesh2DEscapeRouting()
)))
class TestConfig43 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = Mesh2D(4, 4),
  channelParamGen = (a, b) => UserChannelParams(
    Seq.fill(2) { UserVirtualChannelParams(2) },
    destSpeedup = 2
  ),
  ingresses       = (0 until 16).map { i => UserIngressParams(i) },
  egresses        = (0 until 16).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(16, 16) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = Mesh2DEscapeRouting()
)))
class TestConfig44 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = Mesh2D(4, 4),
  channelParamGen = (a, b) => UserChannelParams(
    Seq.fill(2) { UserVirtualChannelParams(2) },
    srcSpeedup = 2
  ),
  ingresses       = (0 until 16).map { i => UserIngressParams(i) },
  egresses        = (0 until 16).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(16, 16) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = Mesh2DEscapeRouting()
)))
class TestConfig45 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = Mesh2D(4, 4),
  channelParamGen = (a, b) => UserChannelParams(
    Seq.fill(2) { UserVirtualChannelParams(2) },
    srcSpeedup = 2,
    destSpeedup = 2
  ),
  ingresses       = (0 until 16).map { i => UserIngressParams(i) },
  egresses        = (0 until 16).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(16, 16) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = Mesh2DEscapeRouting()
)))
class TestConfig46 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = Mesh2D(5, 5),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(1) { UserVirtualChannelParams(1) }),
  ingresses       = (0 until 25).map { i => UserIngressParams(i) },
  egresses        = (0 until 25).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(25, 25) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = Mesh2DDimensionOrderedRouting()
)))
class TestConfig47 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = Mesh2D(5, 5),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(1) { UserVirtualChannelParams(1) }),
  ingresses       = (0 until 25).map { i => UserIngressParams(i) },
  egresses        = (0 until 25).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(25, 25) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = Mesh2DWestFirstRouting(),
)))
class TestConfig48 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = Mesh2D(5, 5),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(1) { UserVirtualChannelParams(1) }),
  ingresses       = (0 until 25).map { i => UserIngressParams(i) },
  egresses        = (0 until 25).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(25, 25) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = Mesh2DNorthLastRouting()
)))
class TestConfig49 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = Mesh2D(3, 3),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(3) }),
  ingresses       = (0 until 9).map { i => UserIngressParams(i) },
  egresses        = (0 until 9).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(9, 9) { (s, d) =>
    if (s % 4 == (d + 2) % 4) Some(FlowParams(s, d, s % 4)) else None
  }.flatten.flatten,
  routingRelation = BlockingVirtualSubnetworksRouting(Mesh2DEscapeRouting(), 4, 1),
  vNetBlocking    = (blocker, blockee) => blocker < blockee
)))
class TestConfig50 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = Mesh2D(3, 3),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(3) },
    useOutputQueues = false),
  ingresses       = (0 until 9).map { i => UserIngressParams(i) },
  egresses        = (0 until 9).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(9, 9) { (s, d) =>
    if (s % 4 == (d + 2) % 4) Some(FlowParams(s, d, s % 4)) else None
  }.flatten.flatten,
  routingRelation = BlockingVirtualSubnetworksRouting(Mesh2DDimensionOrderedRouting(), 4, 1),
  vNetBlocking    = (blocker, blockee) => blocker < blockee
)))
class TestConfig51 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = Mesh2D(3, 3),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(3) }),
  ingresses       = (0 until 9).map { i => UserIngressParams(i) },
  egresses        = (0 until 9).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(9, 9) { (s, d) =>
    if (s % 4 == (d + 2) % 4) Some(FlowParams(s, d, s % 4)) else None
  }.flatten.flatten,
  routingRelation = NonblockingVirtualSubnetworksRouting(Mesh2DEscapeRouting(), 4, 1),
  vNetBlocking    = (blocker, blockee) => true
)))
class TestConfig52 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = UnidirectionalTorus2D(3, 3),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(2) { UserVirtualChannelParams(1) }),
  ingresses       = (0 until 9).map { i => UserIngressParams(i) },
  egresses        = (0 until 9).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(9, 9) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = DimensionOrderedUnidirectionalTorus2DDatelineRouting()
)))
class TestConfig53 extends NoCTesterConfig(NoCTesterParams(
  NoCParams(
    topology        = UnidirectionalTorus2D(3, 3),
    channelParamGen = (a, b) => UserChannelParams(Seq.fill(3) { UserVirtualChannelParams(4) }),
    ingresses       = (0 until 9).map { i => UserIngressParams(i) },
    egresses        = (0 until 9).map { i => UserEgressParams(i) },
    flows           = Seq.tabulate(9, 9) { (s, d) => FlowParams(s, d, 0) }.flatten,
    routingRelation = DimensionOrderedUnidirectionalTorus2DDatelineRouting()
  ),
  inputPacketStallProbability = 0.5
))
class TestConfig54 extends NoCTesterConfig(NoCTesterParams(
  NoCParams(
    topology        = UnidirectionalTorus2D(5, 5),
    channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(4) },
      useOutputQueues = false),
    ingresses       = (0 until 25).map { i => UserIngressParams(i) },
    egresses        = (0 until 25).map { i => UserEgressParams(i) },
    flows           = Seq.tabulate(25, 25) { (s, d) => FlowParams(s, d, 0) }.flatten,
    routingRelation = DimensionOrderedUnidirectionalTorus2DDatelineRouting()
  ),
  inputPacketStallProbability = 0.8
))
class TestConfig55 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = BidirectionalTorus2D(3, 3),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(2) { UserVirtualChannelParams(2) }),
  ingresses       = (0 until 9).map { i => UserIngressParams(i) },
  egresses        = (0 until 9).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(9, 9) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = DimensionOrderedBidirectionalTorus2DDatelineRouting()
)))
class TestConfig56 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = TerminalRouter(UnidirectionalLine(3)),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(4) }),
  ingresses       = Seq(0, 1, 1).map { i => UserIngressParams(i) },
  egresses        = Seq(1, 1, 2).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(3, 3) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = TerminalRouterRouting(AllLegalRouting())
)))
class TestConfig57 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = TerminalRouter(BidirectionalLine(4)),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(3) { UserVirtualChannelParams(3) }),
  ingresses       = Seq(1, 1, 2, 2).map { i => UserIngressParams(i) },
  egresses        = Seq(0, 0, 1, 1, 2, 2, 3, 3).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(4, 8) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = TerminalRouterRouting(BidirectionalLineRouting())
)))
class TestConfig58 extends NoCTesterConfig(NoCTesterParams(
  NoCParams(
    topology        = TerminalRouter(UnidirectionalTorus1D(6)),
    channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
    routerParams    = (i) => UserRouterParams(
      vcAllocator   = (vP) => (p) => new PrioritizingSingleVCAllocator(vP)(p)),
    ingresses       = (0 until 6).map { i => UserIngressParams(i) },
    egresses        = (0 until 6).map { i => UserEgressParams(i) },
    flows           = Seq.tabulate(6, 6) { (s, d) => FlowParams(s, d, 0) }.flatten,
    routingRelation = TerminalRouterRouting(UnidirectionalTorus1DDatelineRouting())
  ),
  inputPacketStallProbability = 0.9,
))
class TestConfig59 extends NoCTesterConfig(NoCTesterParams(
  NoCParams(
    topology        = TerminalRouter(BidirectionalTorus1D(6)),
    channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
    ingresses       = (0 until 12).map { i => UserIngressParams(i % 6) },
    egresses        = (0 until 12).map { i => UserEgressParams(i % 6) },
    flows           = Seq.tabulate(12, 12) { (s, d) => FlowParams(s, d, 0) }.flatten,
    routingRelation = TerminalRouterRouting(BidirectionalTorus1DShortestRouting())
  ),
  inputPacketStallProbability = 0.90
))
class TestConfig60 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = TerminalRouter(Mesh2D(5, 5)),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
  ingresses       = (0 until 25).map { i => UserIngressParams(i) },
  egresses        = (0 until 25).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(25, 25) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = TerminalRouterRouting(Mesh2DEscapeRouting())
)))
class TestConfig61 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = TerminalRouter(Mesh2D(5, 5)),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
  ingresses       = (0 until 25).map { i => UserIngressParams(i) },
  egresses        = (0 until 25).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(25, 25) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = TerminalRouterRouting(Mesh2DDimensionOrderedRouting())
)))
class TestConfig62 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = TerminalRouter(Mesh2D(4, 4)),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
  ingresses       = (0 until 16).map { i => UserIngressParams(i) },
  egresses        = (0 until 16).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(16, 16) { (s, d) =>
    if (s % 3 == ((d + 2) % 3)) Some(FlowParams(s, d, s % 3)) else None
  }.flatten.flatten,
  routingRelation = BlockingVirtualSubnetworksRouting(TerminalRouterRouting(Mesh2DEscapeRouting()), 4),
  vNetBlocking    = (blocker, blockee) => blocker < blockee
)))
class TestConfig63 extends NoCTesterConfig(NoCTesterParams(
  NoCParams(
    topology        = TerminalRouter(Mesh2D(4, 4)),
    channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
    routerParams    = (i) => UserRouterParams(coupleSAVA=true),
    ingresses       = (0 until 4).map { i => UserIngressParams(i) },
    egresses        = (4 until 16).map { i => UserEgressParams(i) },
    flows           = Seq.tabulate(4, 12) { (s, d) => FlowParams(s, d, 0) }.flatten,
    routingRelation = TerminalRouterRouting(Mesh2DEscapeRouting())
  ),
  inputFlitStallProbability = 0.9
))
class TestConfig64 extends NoCTesterConfig(NoCTesterParams(
  NoCParams(
    topology        = TerminalRouter(Mesh2D(4, 4)),
    channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) },
      useOutputQueues = false,
    ),
    routerParams    = (i) => UserRouterParams(coupleSAVA=true),
    ingresses       = (0 until 4).map { i => UserIngressParams(i) },
    egresses        = (4 until 16).map { i => UserEgressParams(i) },
    flows           = Seq.tabulate(4, 12) { (s, d) => FlowParams(s, d, 0) }.flatten,
    routingRelation = TerminalRouterRouting(Mesh2DEscapeRouting()),
    hasCtrl         = true
  ),
  inputFlitStallProbability = 0.9
))
class TestConfig65 extends NoCTesterConfig(NoCTesterParams(
  NoCParams(
    topology        = TerminalRouter(Mesh2D(4, 4)),
    channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
    routerParams    = (i) => UserRouterParams(coupleSAVA=true),
    ingresses       = (0 until 16).map { i => UserIngressParams(i) },
    egresses        = (0 until 16).map { i => UserEgressParams(i) },
    flows           = Seq.tabulate(16, 16) { (s, d) => FlowParams(s, d, 0) }.flatten,
    routingRelation = TerminalRouterRouting(Mesh2DEscapeRouting()),
    hasCtrl         = true
  ),
  inputFlitStallProbability = 0.9
))
class TestConfig66 extends NoCTesterConfig(NoCTesterParams(
  NoCParams(
    topology        = HierarchicalTopology(
      base = BidirectionalLine(4),
      children = Seq(
        HierarchicalSubTopology(0, 1, BidirectionalLine(3)),
        HierarchicalSubTopology(3, 0, BidirectionalLine(2)),
        HierarchicalSubTopology(1, 1, BidirectionalLine(3)),
        HierarchicalSubTopology(1, 2, BidirectionalLine(4))
      )
    ),
    channelParamGen = (a, b) => UserChannelParams(Seq.fill(2) { UserVirtualChannelParams(5) }),
    ingresses       = (0 until 16).map { i => UserIngressParams(i) },
    egresses        = (0 until 16).map { i => UserEgressParams(i) },
    flows           = Seq.tabulate(16, 16) { (s, d) => FlowParams(s, d, 0) }.flatten,
    routingRelation = HierarchicalRouting(
      baseRouting = BidirectionalLineRouting(),
      childRouting = Seq.fill(4) { BidirectionalLineRouting() }
    )
  ),
  inputPacketStallProbability = 0.8
))
class TestConfig67 extends NoCTesterConfig(NoCTesterParams(
  NoCParams(
    topology        = HierarchicalTopology(
      base = UnidirectionalTorus1D(5),
      children = Seq(
        HierarchicalSubTopology(0, 1, Mesh2D(2, 3)),
        HierarchicalSubTopology(2, 4, Mesh2D(3, 2)),
        HierarchicalSubTopology(3, 2, BidirectionalLine(3)),
        HierarchicalSubTopology(4, 1, BidirectionalLine(3))
      )
    ),
    channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
    routerParams    = (i) => UserRouterParams(
      vcAllocator   = (vP) => (p) => new PrioritizingSingleVCAllocator(vP)(p)),
    ingresses       = (0 until 23).map { i => UserIngressParams(i) },
    egresses        = (0 until 23).map { i => UserEgressParams(i) },
    flows           = Seq.tabulate(23, 23) { (s, d) => FlowParams(s, d, 0) }.flatten,
    routingRelation = HierarchicalRouting(
      baseRouting = UnidirectionalTorus1DDatelineRouting(),
      childRouting = Seq(
        Mesh2DEscapeRouting(),
        Mesh2DDimensionOrderedRouting(),
        BidirectionalLineRouting(),
        BidirectionalLineRouting()
      )
    )
  ),
  inputPacketStallProbability = 0.97
))
class TestConfig68 extends NoCTesterConfig(NoCTesterParams(
  NoCParams(
    topology        = TerminalRouter(HierarchicalTopology(
      base = UnidirectionalTorus1D(5),
      children = Seq(
        HierarchicalSubTopology(0, 1, Mesh2D(2, 3)),
        HierarchicalSubTopology(2, 4, Mesh2D(3, 2)),
        HierarchicalSubTopology(3, 2, BidirectionalLine(3)),
        HierarchicalSubTopology(4, 1, BidirectionalLine(3))
      )
    )),
    channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
    routerParams    = (i) => UserRouterParams(
      vcAllocator   = (vP) => (p) => new PrioritizingSingleVCAllocator(vP)(p)),

    ingresses       = (0 until 23).map { i => UserIngressParams(i) },
    egresses        = (0 until 23).map { i => UserEgressParams(i) },
    flows           = Seq.tabulate(23, 23) { (s, d) => FlowParams(s, d, 0) }.flatten,
    routingRelation = TerminalRouterRouting(HierarchicalRouting(
      baseRouting = UnidirectionalTorus1DDatelineRouting(),
      childRouting = Seq(
        Mesh2DEscapeRouting(),
        Mesh2DDimensionOrderedRouting(),
        BidirectionalLineRouting(),
        BidirectionalLineRouting()
      )
    ))
  ),
  inputPacketStallProbability = 0.98
))
class TestConfig69 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = UnidirectionalLine(2),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
  routerParams    = (i) => UserRouterParams(payloadBits=32),
  ingresses       = Seq(0).map { i => UserIngressParams(i) },
  egresses        = Seq(1).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(1, 1) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = AllLegalRouting()
)))
class TestConfig70 extends NoCTesterConfig(NoCTesterParams(NoCParams(
  topology        = Mesh2D(4, 4),
  channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
  routerParams    = (i) => UserRouterParams(payloadBits=16),
  ingresses       = (0 until 16).map { i => UserIngressParams(i) },
  egresses        = (0 until 16).map { i => UserEgressParams(i) },
  flows           = Seq.tabulate(16, 16) { (s, d) => FlowParams(s, d, 0) }.flatten,
  routingRelation = Mesh2DEscapeRouting()
)))
class TestConfig71 extends NoCTesterConfig(NoCTesterParams(
  NoCParams(
    topology        = UnidirectionalLine(5),
    channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
    routerParams    = (i) => UserRouterParams(payloadBits = i match {
      case 0 | 4 => 36
      case 1 | 3 => 12
      case 2     => 24
    }),
    ingresses       = Seq(0).map { i => UserIngressParams(i, payloadBits=72) },
    egresses        = Seq(4).map { i => UserEgressParams(i, payloadBits=72) },
    flows           = Seq.tabulate(1, 1) { (s, d) => FlowParams(s, d, 0) }.flatten,
    routingRelation = AllLegalRouting()
  ),
  totalTxs = 10000
))
class TestConfig72 extends NoCTesterConfig(NoCTesterParams(
  NoCParams(
    topology        = Mesh2D(4, 4),
    channelParamGen = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
    routerParams    = (i) => UserRouterParams(payloadBits = i match {
      case 0 | 4 |  8 | 12 =>  8
      case 1 | 5 |  9 | 13 => 16
      case 2 | 6 | 10 | 14 => 32
      case 3 | 7 | 11 | 15 => 64
    }),
    ingresses       = (0 until 16).map { i => UserIngressParams(i) },
    egresses        = (0 until 16).map { i => UserEgressParams(i) },
    flows           = Seq.tabulate(16, 16) { (s, d) => FlowParams(s, d, 0) }.flatten,
    routingRelation = Mesh2DEscapeRouting()
  ),
  inputPacketStallProbability = 0.9
))

class TLTestConfig00 extends TLNoCTesterConfig(TLNoCTesterParams(
  inNodeMapping = Seq(0),
  outNodeMapping = Seq(1),
  nocParams = NoCParams(
    topology        = BidirectionalLine(2),
    channelParamGen = (a, b) => UserChannelParams(Seq.fill(5) { UserVirtualChannelParams(1) }),
    vNetBlocking    = (blocker, blockee) => true,
    routingRelation = NonblockingVirtualSubnetworksRouting(BidirectionalLineRouting(), 5, 1)
  )
))
class TLTestConfig01 extends TLNoCTesterConfig(TLNoCTesterParams(
  inNodeMapping = Seq(4, 0, 2, 5, 6, 9, 11),
  outNodeMapping = Seq(7, 1, 3, 8, 10),
  nocParams = NoCParams(
    topology        = Mesh2D(4, 3),
    channelParamGen = (a, b) => UserChannelParams(Seq.fill(7) { UserVirtualChannelParams(3) }),
    vNetBlocking    = (blocker, blockee) => true,
    routingRelation = NonblockingVirtualSubnetworksRouting(Mesh2DEscapeRouting(), 5, 1)
  )
))
class TLTestConfig02 extends TLNoCTesterConfig(TLNoCTesterParams(
  inNodeMapping = Seq(4, 0, 2, 5, 6, 9, 11),
  outNodeMapping = Seq(7, 1, 3, 8, 10),
  nocParams = NoCParams(
    topology        = Mesh2D(4, 3),
    channelParamGen = (a, b) => UserChannelParams(Seq.fill(5) { UserVirtualChannelParams(3) }),
    vNetBlocking    = (blocker, blockee) => blocker < blockee,
    routingRelation = BlockingVirtualSubnetworksRouting(Mesh2DEscapeRouting(), 5)
  )
))
class TLTestConfig03 extends TLNoCTesterConfig(TLNoCTesterParams(
  inNodeMapping = Seq(0, 1, 2),
  outNodeMapping = Seq(3, 4, 5),
  nocParams = NoCParams(
    topology        = UnidirectionalTorus1D(6),
    channelParamGen = (a, b) => UserChannelParams(Seq.fill(10) { UserVirtualChannelParams(3) }),
    vNetBlocking    = (blocker, blockee) => blocker < blockee,
    routingRelation = NonblockingVirtualSubnetworksRouting(UnidirectionalTorus1DDatelineRouting(), 5, 2)
  )
))
class TLTestConfig04 extends TLNoCTesterConfig(TLNoCTesterParams(
  inNodeMapping = Seq(4, 0, 2, 5, 6, 9, 11),
  outNodeMapping = Seq(7, 1, 3, 8, 10),
  nocParams = NoCParams(
    topology        = TerminalRouter(Mesh2D(4, 3)),
    channelParamGen = (a, b) => UserChannelParams(Seq.fill(5) { UserVirtualChannelParams(3) }),
    vNetBlocking    = (blocker, blockee) => blocker < blockee,
    routingRelation = BlockingVirtualSubnetworksRouting(TerminalRouterRouting(Mesh2DEscapeRouting()), 5)
  )
))
class TLTestConfig05 extends TLNoCTesterConfig(TLNoCTesterParams(
  inNodeMapping = Seq(4, 0, 2, 5, 6, 9, 11),
  outNodeMapping = Seq(7, 1, 3, 8, 10),
  nocParams = NoCParams(
    topology        = TerminalRouter(BidirectionalTorus2D(3, 4)),
    channelParamGen = (a, b) => UserChannelParams(Seq.fill(10) { UserVirtualChannelParams(3) }),
    vNetBlocking    = (blocker, blockee) => blocker < blockee,
    routingRelation = BlockingVirtualSubnetworksRouting(TerminalRouterRouting(DimensionOrderedBidirectionalTorus2DDatelineRouting()), 5, 2)
  )
))
class TLTestConfig06 extends TLNoCTesterConfig(TLNoCTesterParams(
  inNodeMapping = Seq(4, 0, 2, 5, 6, 9, 11),
  outNodeMapping = Seq(7, 1, 3, 8, 10),
  delay = 0.0,
  nocParams = NoCParams(
    topology        = TerminalRouter(Mesh2D(4, 3)),
    channelParamGen = (a, b) => UserChannelParams(Seq.fill(8) { UserVirtualChannelParams(7) }),
    routerParams    = (i) => UserRouterParams(
      vcAllocator = (vP) => (p) => new PrioritizingSingleVCAllocator(vP)(p)
    ),
    vNetBlocking    = (blocker, blockee) => blocker < blockee,
    routingRelation = NonblockingVirtualSubnetworksRouting(TerminalRouterRouting(Mesh2DEscapeRouting()), 5, 1)
  )
))


class AXI4TestConfig00 extends AXI4NoCTesterConfig(AXI4NoCTesterParams(
  inNodeMapping = Seq(0),
  outNodeMapping = Seq(1),
  nocParams = NoCParams(
    topology      = BidirectionalLine(2),
    channelParamGen = (a, b) => UserChannelParams(Seq.fill(5) { UserVirtualChannelParams(1) }),
    vNetBlocking    = (blocker, blockee) => true,
    routingRelation = NonblockingVirtualSubnetworksRouting(BidirectionalLineRouting(), 5, 1)
  )
))
class AXI4TestConfig01 extends AXI4NoCTesterConfig(AXI4NoCTesterParams(
  inNodeMapping = Seq(0, 2),
  outNodeMapping = Seq(1),
  nocParams = NoCParams(
    topology      = TerminalRouter(BidirectionalLine(3)),
    channelParamGen = (a, b) => UserChannelParams(Seq.fill(5) { UserVirtualChannelParams(1) }),
    vNetBlocking    = (blocker, blockee) => true,
    routingRelation = NonblockingVirtualSubnetworksRouting(TerminalRouterRouting(BidirectionalLineRouting()), 5, 1)
  )
))
class AXI4TestConfig02 extends AXI4NoCTesterConfig(AXI4NoCTesterParams(
  inNodeMapping = Seq(0, 1, 2, 3, 5, 6, 7, 8),
  outNodeMapping = Seq(4),
  nocParams = NoCParams(
    topology      = TerminalRouter(Mesh2D(3, 3)),
    channelParamGen = (a, b) => UserChannelParams(Seq.fill(10) { UserVirtualChannelParams(1) }),
    vNetBlocking    = (blocker, blockee) => true,
    routingRelation = NonblockingVirtualSubnetworksRouting(TerminalRouterRouting(Mesh2DEscapeRouting()), 5, 2)
  )
))
class AXI4TestConfig03 extends AXI4NoCTesterConfig(AXI4NoCTesterParams(
  inNodeMapping = Seq(1, 3, 5, 7),
  outNodeMapping = Seq(0, 2, 4, 6, 8),
  nocParams = NoCParams(
    topology      = TerminalRouter(Mesh2D(3, 3)),
    channelParamGen = (a, b) => UserChannelParams(Seq.fill(10) { UserVirtualChannelParams(1) }),
    vNetBlocking    = (blocker, blockee) => true,
    routingRelation = NonblockingVirtualSubnetworksRouting(TerminalRouterRouting(Mesh2DEscapeRouting()), 5, 2)
  )
))

class EvalTestConfig00 extends NoCEvalConfig(NoCEvalParams(
  requiredThroughput = 0.79,
  flows              = (s, d) => 1.0,
  nocParams = NoCParams(
    topology         = UnidirectionalLine(2),
    channelParamGen  = (a, b) => UserChannelParams(Seq.fill(3) { UserVirtualChannelParams(4) }),
    ingresses        = Seq(0).map { i => UserIngressParams(i) },
    egresses         = Seq(1).map { i => UserEgressParams(i) },
    flows            = Seq.tabulate(1, 1) { (s, d) => FlowParams(s, d, 0) }.flatten,
    routingRelation  = UnidirectionalLineRouting()
  )
))
class EvalTestConfig01 extends NoCEvalConfig(NoCEvalParams(
  requiredThroughput = 0.79,
  flows              = (s, d) => 0.25,
  nocParams = NoCParams(
    topology         = UnidirectionalLine(2),
    channelParamGen  = (a, b) => UserChannelParams(Seq.fill(3) { UserVirtualChannelParams(4) }),
    ingresses        = Seq(0, 0).map { i => UserIngressParams(i) },
    egresses         = Seq(1, 1).map { i => UserEgressParams(i) },
    flows            = Seq.tabulate(2, 2) { (s, d) => FlowParams(s, d, 0) }.flatten,
    routingRelation  = UnidirectionalLineRouting()
  )
))
class EvalTestConfig02 extends NoCEvalConfig(NoCEvalParams(
  requiredThroughput    = 0.99,
  requiredMedianLatency = 150,
  requiredMaxLatency    = 260,
  flows              = (s, d) => 1.0,
  nocParams = NoCParams(
    topology         = UnidirectionalLine(2),
    channelParamGen  = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
    routerParams     = (i) => UserRouterParams(coupleSAVA=true),
    ingresses        = Seq(0).map { i => UserIngressParams(i) },
    egresses         = Seq(1).map { i => UserEgressParams(i) },
    flows            = Seq.tabulate(1, 1) { (s, d) => FlowParams(s, d, 0) }.flatten,
    routingRelation  = UnidirectionalLineRouting()
  )
))
class EvalTestConfig03 extends NoCEvalConfig(NoCEvalParams(
  requiredThroughput    = 0.9,
  requiredMedianLatency = 30,
  requiredMaxLatency    = 175,
  flows              = (s, d) => 0.1 / 10,
  nocParams = NoCParams(
    topology         = UnidirectionalTorus1D(10),
    channelParamGen  = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(5) }),
    ingresses        = (0 until 10).map { i => UserIngressParams(i) },
    egresses         = (0 until 10).map { i => UserEgressParams(i) },
    flows            = Seq.tabulate(10, 10) { (s, d) => FlowParams(s, d, 0) }.flatten,
    routingRelation  = UnidirectionalTorus1DDatelineRouting()
  )
))
class EvalTestConfig04 extends NoCEvalConfig(NoCEvalParams(
  requiredThroughput    = 0.69,
  flows              = (s, d) => 0.05,
  nocParams = NoCParams(
    topology         = Butterfly(2, 3),
    channelParamGen  = (a, b) => UserChannelParams(Seq.fill(1) { UserVirtualChannelParams(5) }),
    ingresses        = (0 until 8).map { i => UserIngressParams(i % 4) },
    egresses         = (0 until 8).map { i => UserEgressParams((i % 4) + 4 * 2) },
    flows            = Seq.tabulate(8, 8) { (s, d) => FlowParams(s, d, 0) }.flatten,
    routingRelation  = ButterflyRouting()
  )
))
class EvalTestConfig05 extends NoCEvalConfig(NoCEvalParams(
  requiredMedianLatency = 75,
  requiredMaxLatency    = 1500,
  requiredThroughput    = 0.90,
  flows              = (s, d) => 0.5 / 25,
  nocParams = NoCParams(
    topology         = Mesh2D(5, 5),
    channelParamGen  = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(4) }),
    routerParams     = (i) => UserRouterParams(
      vcAllocator = (vP) => (p) => new RotatingSingleVCAllocator(vP)(p)
    ),
    ingresses        = (0 until 25).map { i => UserIngressParams(i) },
    egresses         = (0 until 25).map { i => UserEgressParams(i) },
    flows            = Seq.tabulate(25, 25) { (s, d) => FlowParams(s, d, 0) }.flatten,
    routingRelation  = Mesh2DEscapeRouting()
  )
))
class EvalTestConfig06 extends NoCEvalConfig(NoCEvalParams(
  requiredMedianLatency = 25,
  requiredMaxLatency    = 125,
  requiredThroughput    = 0.94,
  flows              = (s, d) => 0.2 / 16,
  nocParams = NoCParams(
    topology         = Mesh2D(4, 4),
    channelParamGen  = (a, b) => UserChannelParams(Seq.fill(1) { UserVirtualChannelParams(4) }),
    ingresses        = (0 until 16).map { i => UserIngressParams(i) },
    egresses         = (0 until 16).map { i => UserEgressParams(i) },
    flows            = Seq.tabulate(16, 16) { (s, d) => FlowParams(s, d, 0) }.flatten,
    routingRelation  = Mesh2DDimensionOrderedRouting()
  )
))
class EvalTestConfig07 extends NoCEvalConfig(NoCEvalParams(
  requiredMedianLatency = 20,
  requiredMaxLatency    = 75,
  requiredThroughput    = 0.95,
  flows              = (s, d) => 0.2 / 16,
  nocParams = NoCParams(
    topology         = Mesh2D(4, 4),
    channelParamGen  = (a, b) => UserChannelParams(Seq.fill(2) { UserVirtualChannelParams(4) }),
    ingresses        = (0 until 16).map { i => UserIngressParams(i) },
    egresses         = (0 until 16).map { i => UserEgressParams(i) },
    flows            = Seq.tabulate(16, 16) { (s, d) => FlowParams(s, d, 0) }.flatten,
    routingRelation  = Mesh2DEscapeRouting()
  )
))
class EvalTestConfig08 extends NoCEvalConfig(NoCEvalParams(
  requiredMedianLatency = 35,
  requiredMaxLatency    = 250,
  requiredThroughput    = 0.95,
  flows              = (s, d) => 0.5 / 16,
  nocParams = NoCParams(
    topology         = Butterfly(2, 4),
    channelParamGen  = (a, b) => UserChannelParams(Seq.fill(4) { UserVirtualChannelParams(4) }),
    ingresses        = (0 until 16).map { i => UserIngressParams(i % 8) },
    egresses         = (0 until 16).map { i => UserEgressParams((i % 8) + 8 * 3) },
    flows            = Seq.tabulate(16, 16) { (s, d) => FlowParams(s, d, 0) }.flatten,
    routingRelation  = ButterflyRouting()
  )
))
