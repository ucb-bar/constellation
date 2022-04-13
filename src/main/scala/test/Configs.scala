package constellation.test

import freechips.rocketchip.config.{Field, Parameters, Config}
import constellation.routing.{RoutingRelation}
import constellation.channel.{UserVirtualChannelParams}

class WithConstPacketSize(sZ: Int = 9) extends Config((site, here, up) => {
  case NoCTesterKey => up(NoCTesterKey).copy(constPacketSize = true, maxFlits = sZ)
})

class WithInputStallProbability(prob: Double) extends Config((site, here, up) => {
  case NoCTesterKey => up(NoCTesterKey).copy(inputStallProbability = prob)
})

// 1D mesh. Shared bus
class TestConfig00 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(Seq(0)) ++
  new constellation.channel.WithEgresses(Seq(1)) ++
  new constellation.topology.WithUnidirectionalLineTopology(2))
class TestConfig01 extends Config(
  new constellation.channel.WithUniformChannelDepth(1) ++
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(Seq(0)) ++
  new constellation.channel.WithEgresses(Seq(1, 1)) ++
  new constellation.topology.WithUnidirectionalLineTopology(2))
class TestConfig02 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(Seq(0, 0)) ++
  new constellation.channel.WithEgresses(Seq(1, 1)) ++
  new constellation.topology.WithUnidirectionalLineTopology(2))
class TestConfig03 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(Seq(0, 0)) ++
  new constellation.channel.WithEgresses (Seq(0, 1, 1)) ++
  new constellation.topology.WithUnidirectionalLineTopology(2))
class TestConfig04 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(Seq(0, 0)) ++
  new constellation.channel.WithEgresses(Seq(0, 1, 1, 2, 2)) ++
  new constellation.topology.WithUnidirectionalLineTopology(3))
class TestConfig05 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(Seq(0, 1, 1)) ++
  new constellation.channel.WithEgresses(Seq(1, 1, 2)) ++
  new constellation.topology.WithUnidirectionalLineTopology(3))
class TestConfig06 extends Config(
  new constellation.router.WithCoupleSAVA ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(Seq(0)) ++
  new constellation.channel.WithEgresses(Seq(1)) ++
  new constellation.topology.WithUnidirectionalLineTopology(2))
class TestConfig07 extends Config(
  new constellation.noc.WithCtrl ++
  new constellation.router.WithCoupleSAVA ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(Seq(0)) ++
  new constellation.channel.WithEgresses(Seq(1)) ++
  new constellation.topology.WithUnidirectionalLineTopology(2))

class TestConfig08 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 2) ++
  new constellation.channel.WithEgresses(0 until 2) ++
  new constellation.topology.WithBidirectionalLineTopology(2))
class TestConfig09 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 2) ++
  new constellation.channel.WithEgresses(0 until 3) ++
  new constellation.topology.WithBidirectionalLineTopology(3))
class TestConfig10 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(1 until 3) ++
  new constellation.channel.WithEgresses(0 until 4) ++
  new constellation.topology.WithBidirectionalLineTopology(4))
class TestConfig11 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(Seq(1, 1, 2, 2)) ++
  new constellation.channel.WithEgresses(Seq(0, 0, 1, 1, 2, 2, 3, 3)) ++
  new constellation.topology.WithBidirectionalLineTopology(4))

// 1D Torus
class TestConfig12 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(Seq(0)) ++
  new constellation.channel.WithEgresses(Seq(1)) ++
  new constellation.topology.WithUnidirectionalTorus1DTopology(2))
class TestConfig13 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(Seq(0, 2)) ++
  new constellation.channel.WithEgresses(Seq(1, 3)) ++
  new constellation.topology.WithUnidirectionalTorus1DTopology(4))
class TestConfig14 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 10 by 2) ++
  new constellation.channel.WithEgresses(1 until 10 by 2) ++
  new constellation.topology.WithUnidirectionalTorus1DTopology(10))
class TestConfig15 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 10) ++
  new constellation.channel.WithEgresses(0 until 10) ++
  new constellation.topology.WithUnidirectionalTorus1DTopology(10))

class TestConfig16 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 2) ++
  new constellation.channel.WithEgresses(0 until 2) ++
  new constellation.topology.WithBidirectionalTorus1DTopology(2))
class TestConfig17 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 4 by 2) ++
  new constellation.channel.WithEgresses(1 until 4 by 2) ++
  new constellation.topology.WithBidirectionalTorus1DTopology(4))
class TestConfig18 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 10) ++
  new constellation.channel.WithEgresses(0 until 10) ++
  new constellation.topology.WithBidirectionalTorus1DTopology(10))

class TestConfig19 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses((0 until 10)) ++
  new constellation.channel.WithEgresses((0 until 10)) ++
  new constellation.topology.WithBidirectionalTorus1DTopology(10, randomRoute = true))
class TestConfig20 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses((0 until 10) ++ (0 until 10)) ++
  new constellation.channel.WithEgresses((0 until 10) ++ (0 until 10)) ++
  new constellation.topology.WithBidirectionalTorus1DTopology(10, randomRoute = true))

// Butterfly
class TestConfig21 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses((0 until 2) ++ (0 until 2)) ++
  new constellation.channel.WithEgresses(((0 until 2) ++ (0 until 2)).map(_ + 2*1)) ++
  new constellation.topology.WithButterflyTopology(2, 2))
class TestConfig22 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses((0 until 4) ++ (0 until 4)) ++
  new constellation.channel.WithEgresses(((0 until 4) ++ (0 until 4)).map(_ + 4*2)) ++
  new constellation.topology.WithButterflyTopology(2, 3))
class TestConfig23 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses((0 until 8) ++ (0 until 8)) ++
  new constellation.channel.WithEgresses(((0 until 8) ++ (0 until 8)).map(_ + 8*3)) ++
  new constellation.topology.WithButterflyTopology(2, 4))
class TestConfig24 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses((0 until 3) ++ (0 until 3)) ++
  new constellation.channel.WithEgresses(((0 until 3) ++ (0 until 3)).map(_ + 3*1)) ++
  new constellation.topology.WithButterflyTopology(3, 2))
class TestConfig25 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses((0 until 9) ++ (0 until 9)) ++
  new constellation.channel.WithEgresses(((0 until 9) ++ (0 until 9)).map(_ + 9*2)) ++
  new constellation.topology.WithButterflyTopology(3, 3))

// Tree Topologies
class TestConfig26 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 6 by 2) ++
  new constellation.channel.WithEgresses(1 until 6 by 2) ++
  new constellation.topology.WithBidirectionalTreeTopology(3, 2))

class TestConfig27 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 6) ++
  new constellation.channel.WithEgresses(0 until 6) ++
  new constellation.channel.WithFatTreeChannels(1) ++
  new constellation.topology.WithBidirectionalTreeTopology(3, 2))

class TestConfig28 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(13 until 40) ++
  new constellation.channel.WithEgresses(13 until 40) ++
  new constellation.channel.WithFatTreeChannels(1) ++
  new constellation.topology.WithBidirectionalTreeTopology(3, 3))

// 2D Mesh
class TestConfig29 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.topology.WithMesh2DTopology(3, 3))
class TestConfig30 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 18) ++
  new constellation.channel.WithEgresses(0 until 18) ++
  new constellation.topology.WithMesh2DTopology(3, 6))
class TestConfig31 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 25) ++
  new constellation.channel.WithEgresses(0 until 25) ++
  new constellation.topology.WithMesh2DTopology(5, 5))
class TestConfig32 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 25) ++
  new constellation.channel.WithEgresses(0 until 25) ++
  new constellation.topology.WithMesh2DTopology(5, 5, RoutingRelation.mesh2DAlternatingDimensionOrdered))
class TestConfig33 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 25) ++
  new constellation.channel.WithEgresses(0 until 25) ++
  new constellation.topology.WithMesh2DTopology(5, 5, RoutingRelation.mesh2DEscapeRouter))

class TestConfig34 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.topology.WithMesh2DTopology(3, 3, RoutingRelation.mesh2DEscapeRouter))
class TestConfig35 extends Config(
  new constellation.router.WithCombineRCVA ++
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.topology.WithMesh2DTopology(3, 3, RoutingRelation.mesh2DEscapeRouter))
class TestConfig36 extends Config(
  new constellation.router.WithCombineSAST ++
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.topology.WithMesh2DTopology(3, 3, RoutingRelation.mesh2DEscapeRouter))
class TestConfig37 extends Config(
  new constellation.router.WithCombineSAST ++
  new constellation.router.WithCombineRCVA ++
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.topology.WithMesh2DTopology(3, 3, RoutingRelation.mesh2DEscapeRouter))
class TestConfig38 extends Config(
  new constellation.router.WithSimpleVCAllocator ++
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.topology.WithMesh2DTopology(3, 3, RoutingRelation.mesh2DEscapeRouter))
class TestConfig39 extends Config(
  new constellation.router.WithEarlyRC ++
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.topology.WithMesh2DTopology(3, 3, RoutingRelation.mesh2DEscapeRouter))
class TestConfig40 extends Config(
  new constellation.router.WithEarlyRC ++
  new constellation.router.WithCombineRCVA ++
  new constellation.router.WithCombineSAST ++
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.topology.WithMesh2DTopology(3, 3, RoutingRelation.mesh2DEscapeRouter))
class TestConfig41 extends Config(
  new constellation.channel.WithUniformChannelDestMultiplier(2) ++
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.topology.WithMesh2DTopology(3, 3, RoutingRelation.mesh2DEscapeRouter))
class TestConfig42 extends Config(
  new constellation.channel.WithUniformChannelSrcMultiplier(2) ++
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.topology.WithMesh2DTopology(3, 3, RoutingRelation.mesh2DEscapeRouter))
class TestConfig43 extends Config(
  new constellation.channel.WithUniformChannelSrcMultiplier(2) ++
  new constellation.channel.WithUniformChannelDestMultiplier(2) ++
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.topology.WithMesh2DTopology(3, 3, RoutingRelation.mesh2DEscapeRouter))


class TestConfig44 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(1)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 25) ++
  new constellation.channel.WithEgresses(0 until 25) ++
  new constellation.topology.WithMesh2DTopology(5, 5))
class TestConfig45 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(1)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 25) ++
  new constellation.channel.WithEgresses(0 until 25) ++
  new constellation.topology.WithMesh2DTopology(5, 5, RoutingRelation.mesh2DWestFirst))
class TestConfig46 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(1)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 25) ++
  new constellation.channel.WithEgresses(0 until 25) ++
  new constellation.topology.WithMesh2DTopology(5, 5, RoutingRelation.mesh2DNorthLast))

class TestConfig47 extends Config(
  new constellation.channel.WithIngressVNets((i: Int) => i % 4) ++
  new constellation.routing.WithNBlockingVirtualNetworks(4) ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.topology.WithMesh2DTopology(3, 3, RoutingRelation.mesh2DEscapeRouter))

class TestConfig48 extends Config(
  new constellation.channel.WithIngressVNets((i: Int) => i % 4) ++
  new constellation.routing.WithNBlockingVirtualNetworks(4) ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.topology.WithMesh2DTopology(3, 3, RoutingRelation.mesh2DAlternatingDimensionOrdered))

class TestConfig49 extends Config(
  new constellation.channel.WithIngressVNets((i: Int) => i % 4) ++
  new constellation.routing.WithNNonblockingVirtualNetworks(4) ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.topology.WithMesh2DTopology(3, 3, RoutingRelation.mesh2DEscapeRouter))


// 2D Torus
class TestConfig50 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(1)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.topology.WithUnidirectionalTorus2DTopology(3, 3))
class TestConfig51 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.topology.WithUnidirectionalTorus2DTopology(3, 3))
class TestConfig52 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(4)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 25) ++
  new constellation.channel.WithEgresses(0 until 25) ++
  new constellation.topology.WithUnidirectionalTorus2DTopology(5, 5))

class TestConfig53 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(1)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.topology.WithBidirectionalTorus2DTopology(3, 3))

// topologies which put the ingress/egress points on a separate "plane" of nodes
class TestConfig54 extends Config(
  new constellation.noc.WithTerminalPlane ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(Seq(0, 1, 1)) ++
  new constellation.channel.WithEgresses(Seq(1, 1, 2)) ++
  new constellation.topology.WithUnidirectionalLineTopology(3))
class TestConfig55 extends Config(
  new constellation.noc.WithTerminalPlane ++
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(Seq(1, 1, 2, 2)) ++
  new constellation.channel.WithEgresses(Seq(0, 0, 1, 1, 2, 2, 3, 3)) ++
  new constellation.topology.WithBidirectionalLineTopology(4))
class TestConfig56 extends Config(
  new constellation.noc.WithTerminalPlane ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses((0 until 6) ++ (0 until 6)) ++
  new constellation.channel.WithEgresses((0 until 6) ++ (0 until 6)) ++
  new constellation.topology.WithUnidirectionalTorus1DTopology(6))
class TestConfig57 extends Config(
  new constellation.noc.WithTerminalPlane ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses((0 until 6) ++ (0 until 6)) ++
  new constellation.channel.WithEgresses((0 until 6) ++ (0 until 6)) ++
  new constellation.topology.WithBidirectionalTorus1DTopology(6))
class TestConfig58 extends Config(
  new constellation.noc.WithTerminalPlane ++
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses((0 until 9) ++ (0 until 9)) ++
  new constellation.channel.WithEgresses(((0 until 9) ++ (0 until 9)).map(_ + 9*2)) ++
  new constellation.topology.WithButterflyTopology(3, 3))
class TestConfig59 extends Config(
  new constellation.noc.WithTerminalPlane ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses((0 until 25)) ++
  new constellation.channel.WithEgresses((0 until 25)) ++
  new constellation.topology.WithMesh2DTopology(5, 5, RoutingRelation.mesh2DEscapeRouter))
class TestConfig60 extends Config(
  new constellation.routing.WithNBlockingVirtualNetworks(4) ++
  new constellation.noc.WithTerminalPlane ++
  new constellation.channel.WithIngressVNets((i: Int) => i % 4) ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 16) ++
  new constellation.channel.WithEgresses(0 until 16) ++
  new constellation.topology.WithMesh2DTopology(4, 4, RoutingRelation.mesh2DEscapeRouter))
class TestConfig61 extends Config(
  new constellation.test.WithInputStallProbability(0.7) ++
  new constellation.router.WithSafeCoupleSAVA ++
  new constellation.noc.WithTerminalPlane ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(4 until 16) ++
  new constellation.channel.WithEgresses(0 until 4) ++
  new constellation.topology.WithMesh2DTopology(4, 4, RoutingRelation.mesh2DEscapeRouter))
class TestConfig62 extends Config(
  new constellation.test.WithInputStallProbability(0.7) ++
  new constellation.router.WithSafeCoupleSAVA ++
  new constellation.noc.WithCtrl ++
  new constellation.noc.WithTerminalPlane ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(4 until 16) ++
  new constellation.channel.WithEgresses(0 until 4) ++
  new constellation.topology.WithMesh2DTopology(4, 4, RoutingRelation.mesh2DEscapeRouter))
class TestConfig63 extends Config(
  new constellation.noc.WithTerminalPlane ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 16) ++
  new constellation.channel.WithEgresses(0 until 16) ++
  new constellation.topology.WithMesh2DTopology(4, 4, RoutingRelation.mesh2DEscapeRouter))

// test configs for TL network
class TLTestConfig00 extends Config(
  new WithTLNoCTesterParams(TLNoCTesterParams(Seq(0), Seq(1))) ++
  new constellation.routing.WithNNonblockingVirtualNetworks(5) ++
  new constellation.channel.WithUniformNVirtualChannels(5, UserVirtualChannelParams(1)) ++
  new constellation.topology.WithBidirectionalLineTopology(2))

class TLTestConfig01 extends Config(
  new WithTLNoCTesterParams(TLNoCTesterParams(Seq(4, 0, 2, 5, 6, 9, 11), Seq(7, 1, 3, 8, 10))) ++
  new constellation.routing.WithNNonblockingVirtualNetworksWithSharing(5, 2) ++
  new constellation.channel.WithUniformNVirtualChannels(7, UserVirtualChannelParams(3)) ++
  new constellation.topology.WithMesh2DTopology(4, 3, RoutingRelation.mesh2DEscapeRouter))

class TLTestConfig02 extends Config(
  new WithTLNoCTesterParams(TLNoCTesterParams(Seq(4, 0, 2, 5, 6, 9, 11), Seq(7, 1, 3, 8, 10))) ++
  new constellation.routing.WithNBlockingVirtualNetworks(5) ++
  new constellation.channel.WithUniformNVirtualChannels(5, UserVirtualChannelParams(3)) ++
  new constellation.topology.WithMesh2DTopology(4, 3, RoutingRelation.mesh2DEscapeRouter))

class TLTestConfig03 extends Config(
  new WithTLNoCTesterParams(TLNoCTesterParams(Seq(0, 1, 2), Seq(3, 4, 5))) ++
  new constellation.routing.WithNNonblockingVirtualNetworks(5) ++
  new constellation.channel.WithUniformNVirtualChannels(10, UserVirtualChannelParams(3)) ++
  new constellation.topology.WithUnidirectionalTorus1DTopology(6))

class TLTestConfig04 extends Config(
  new WithTLNoCTesterParams(TLNoCTesterParams(Seq(4, 0, 2, 5, 6, 9, 11), Seq(7, 1, 3, 8, 10))) ++
  new constellation.routing.WithNBlockingVirtualNetworks(5) ++
  new constellation.noc.WithTerminalPlane ++
  new constellation.channel.WithUniformNVirtualChannels(5, UserVirtualChannelParams(3)) ++
  new constellation.topology.WithMesh2DTopology(4, 3, RoutingRelation.mesh2DEscapeRouter))
