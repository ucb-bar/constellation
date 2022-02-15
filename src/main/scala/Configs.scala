package constellation

import freechips.rocketchip.config.{Field, Parameters, Config}
import constellation.routing.{RoutingRelation}
import constellation.channel.{UserVirtualChannelParams}

// 1D mesh. Shared bus
class TestConfig00 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.topology.UnidirectionalLineConfig(2, Seq(0), Seq(1)))
class TestConfig01 extends Config(
  new constellation.channel.WithUniformChannelDepth(1) ++
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.topology.UnidirectionalLineConfig(2, Seq(0), Seq(1, 1)))
class TestConfig02 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.topology.UnidirectionalLineConfig(2, Seq(0, 0), Seq(1, 1)))
class TestConfig03 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.topology.UnidirectionalLineConfig(2, Seq(0, 0), Seq(0, 1, 1)))
class TestConfig04 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.topology.UnidirectionalLineConfig(3, Seq(0, 0), Seq(0, 1, 1, 2, 2)))
class TestConfig05 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.topology.UnidirectionalLineConfig(3, Seq(0, 1, 1), Seq(1, 1, 2)))
class TestConfig06 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.topology.UnidirectionalLineConfig(2, Seq(0), Seq(1)))

class TestConfig07 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.topology.BidirectionalLineConfig(2, Seq(0, 1), Seq(0, 1)))
class TestConfig08 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.topology.BidirectionalLineConfig(3, Seq(0, 1), Seq(0, 1, 2)))
class TestConfig09 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.topology.BidirectionalLineConfig(4, Seq(1, 2), Seq(0, 1, 2, 3)))
class TestConfig10 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.topology.BidirectionalLineConfig(4, Seq(1, 1, 2, 2), Seq(0, 0, 1, 1, 2, 2, 3, 3)))

// 1D Torus
class TestConfig11 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.topology.UnidirectionalTorus1DConfig(2, Seq(0), Seq(1)))
class TestConfig12 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.topology.UnidirectionalTorus1DConfig(4, Seq(0, 2), Seq(1, 3)))
class TestConfig13 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.topology.UnidirectionalTorus1DConfig(10, Seq(0, 2, 4, 6, 8), Seq(1, 3, 5, 7, 9)))
class TestConfig14 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.topology.UnidirectionalTorus1DConfig(10, 0 until 10, 0 until 10))

class TestConfig15 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.topology.BidirectionalTorus1DConfig(2, Seq(0, 1), Seq(0, 1)))
class TestConfig16 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.topology.BidirectionalTorus1DConfig(4, Seq(0, 2), Seq(1, 3)))
class TestConfig17 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.topology.BidirectionalTorus1DConfig(10, 0 until 10, 0 until 10))

class TestConfig18 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.topology.BidirectionalTorus1DConfig(10, 0 until 10, 0 until 10, randomRoute = true))
class TestConfig19 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.topology.BidirectionalTorus1DConfig(10, Seq.tabulate(20)(_ % 10), Seq.tabulate(20)(_ % 10), randomRoute = true))

// Butterfly
class TestConfig20 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(5)) ++
  new constellation.topology.ButterflyConfig(2, 2))
class TestConfig21 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(5)) ++
  new constellation.topology.ButterflyConfig(2, 3))
class TestConfig22 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(5)) ++
  new constellation.topology.ButterflyConfig(2, 4))
class TestConfig23 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(5)) ++
  new constellation.topology.ButterflyConfig(3, 2))
class TestConfig24 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(5)) ++
  new constellation.topology.ButterflyConfig(3, 3))

// 2D Mesh
class TestConfig25 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.topology.Mesh2DConfig(3, 3))
class TestConfig26 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.topology.Mesh2DConfig(3, 6))
class TestConfig27 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.topology.Mesh2DConfig(5, 5))
class TestConfig28 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.topology.Mesh2DConfig(5, 5, RoutingRelation.mesh2DAlternatingDimensionOrdered))
class TestConfig29 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.topology.Mesh2DConfig(5, 5, RoutingRelation.mesh2DEscapeRouter))

class TestConfig30 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new constellation.topology.Mesh2DConfig(3, 3, RoutingRelation.mesh2DEscapeRouter))
class TestConfig31 extends Config(
  new constellation.router.WithCombineRCVA ++
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new constellation.topology.Mesh2DConfig(3, 3, RoutingRelation.mesh2DEscapeRouter))
class TestConfig32 extends Config(
  new constellation.router.WithCombineSAST ++
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new constellation.topology.Mesh2DConfig(3, 3, RoutingRelation.mesh2DEscapeRouter))
class TestConfig33 extends Config(
  new constellation.router.WithCombineSAST ++
  new constellation.router.WithCombineRCVA ++
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new constellation.topology.Mesh2DConfig(3, 3, RoutingRelation.mesh2DEscapeRouter))
class TestConfig34 extends Config(
  new constellation.router.WithIterativeVCAllocator ++
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new constellation.topology.Mesh2DConfig(3, 3, RoutingRelation.mesh2DEscapeRouter))
class TestConfig35 extends Config(
  new constellation.router.WithEarlyRC ++
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new constellation.topology.Mesh2DConfig(3, 3, RoutingRelation.mesh2DEscapeRouter))
class TestConfig36 extends Config(
  new constellation.router.WithEarlyRC ++
  new constellation.router.WithCombineRCVA ++
  new constellation.router.WithCombineSAST ++
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new constellation.topology.Mesh2DConfig(3, 3, RoutingRelation.mesh2DEscapeRouter))
class TestConfig37 extends Config(
  new constellation.channel.WithUniformChannelDestMultiplier(2) ++
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new constellation.topology.Mesh2DConfig(3, 3, RoutingRelation.mesh2DEscapeRouter))
class TestConfig38 extends Config(
  new constellation.channel.WithUniformChannelSrcMultiplier(2) ++
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new constellation.topology.Mesh2DConfig(3, 3, RoutingRelation.mesh2DEscapeRouter))
class TestConfig39 extends Config(
  new constellation.channel.WithUniformChannelSrcMultiplier(2) ++
  new constellation.channel.WithUniformChannelDestMultiplier(2) ++
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new constellation.topology.Mesh2DConfig(3, 3, RoutingRelation.mesh2DEscapeRouter))


class TestConfig40 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(1)) ++
  new constellation.topology.Mesh2DConfig(5, 5))
class TestConfig41 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(1)) ++
  new constellation.topology.Mesh2DConfig(5, 5, RoutingRelation.mesh2DWestFirst))
class TestConfig42 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(1)) ++
  new constellation.topology.Mesh2DConfig(5, 5, RoutingRelation.mesh2DNorthLast))

class TestConfig43 extends Config(
  new constellation.channel.WithIngressVNets((i: Int) => i % 4) ++
  new constellation.routing.WithNBlockingVirtualNetworks(4) ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(3)) ++
  new constellation.topology.Mesh2DConfig(3, 3, RoutingRelation.mesh2DEscapeRouter))

class TestConfig44 extends Config(
  new constellation.channel.WithIngressVNets((i: Int) => i % 4) ++
  new constellation.routing.WithNBlockingVirtualNetworks(4) ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(3)) ++
  new constellation.topology.Mesh2DConfig(3, 3, RoutingRelation.mesh2DAlternatingDimensionOrdered))

class TestConfig45 extends Config(
  new constellation.channel.WithIngressVNets((i: Int) => i % 4) ++
  new constellation.routing.WithNNonblockingVirtualNetworks(4) ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(3)) ++
  new constellation.topology.Mesh2DConfig(3, 3, RoutingRelation.mesh2DEscapeRouter))


// 2D Torus
class TestConfig46 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(1)) ++
  new constellation.topology.UnidirectionalTorus2DConfig(3, 3))
class TestConfig47 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.topology.UnidirectionalTorus2DConfig(3, 3))
class TestConfig48 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(4)) ++
  new constellation.topology.UnidirectionalTorus2DConfig(5, 5))

class TestConfig49 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(1)) ++
  new constellation.topology.BidirectionalTorus2DConfig(3, 3))

class TLTestConfig00 extends Config(
  new WithTLNoCTesterParams(TLNoCTesterParams(Seq(0), Seq(1))) ++
  new constellation.routing.WithNNonblockingVirtualNetworks(5) ++
  new constellation.channel.WithUniformNVirtualChannels(5, UserVirtualChannelParams(1)) ++
  new constellation.topology.BidirectionalLineConfig(2))

class TLTestConfig01 extends Config(
  new WithTLNoCTesterParams(TLNoCTesterParams(Seq(4, 0, 2, 5, 6, 9, 11), Seq(7, 1, 3, 8, 10))) ++
  new constellation.routing.WithNNonblockingVirtualNetworksWithSharing(5, 2) ++
  new constellation.channel.WithUniformNVirtualChannels(7, UserVirtualChannelParams(3)) ++
  new constellation.topology.Mesh2DConfig(4, 3, RoutingRelation.mesh2DEscapeRouter))

class TLTestConfig02 extends Config(
  new WithTLNoCTesterParams(TLNoCTesterParams(Seq(4, 0, 2, 5, 6, 9, 11), Seq(7, 1, 3, 8, 10))) ++
  new constellation.routing.WithNBlockingVirtualNetworks(5) ++
  new constellation.channel.WithUniformNVirtualChannels(5, UserVirtualChannelParams(3)) ++
  new constellation.topology.Mesh2DConfig(4, 3, RoutingRelation.mesh2DEscapeRouter))

class TLTestConfig03 extends Config(
  new WithTLNoCTesterParams(TLNoCTesterParams(Seq(0, 1, 2), Seq(3, 4, 5))) ++
  new constellation.routing.WithNNonblockingVirtualNetworks(5) ++
  new constellation.channel.WithUniformNVirtualChannels(10, UserVirtualChannelParams(3)) ++
  new constellation.topology.UnidirectionalTorus1DConfig(6))
