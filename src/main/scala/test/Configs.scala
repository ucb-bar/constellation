package constellation.test

import freechips.rocketchip.config.{Field, Parameters, Config}
import constellation.routing._
import constellation.topology._
import constellation.noc.{NoCKey}
import constellation.channel.{UserVirtualChannelParams, UserChannelParams, FlowParams}
import scala.collection.immutable.ListMap

class WithConstPacketSize(sZ: Int = 9) extends Config((site, here, up) => {
  case NoCTesterKey => up(NoCTesterKey).copy(constPacketSize = true, maxFlits = sZ)
})

class WithInputFlitStallProbability(prob: Double) extends Config((site, here, up) => {
  case NoCTesterKey => up(NoCTesterKey).copy(inputFlitStallProbability = prob)
})

class WithInputPacketStallProbability(prob: Double) extends Config((site, here, up) => {
  case NoCTesterKey => up(NoCTesterKey).copy(inputPacketStallProbability = prob)
})

class WithTotalTxs(t: Int) extends Config((site, here, up) => {
  case NoCTesterKey => up(NoCTesterKey).copy(totalTxs = t)
})

class WithEvalFlow(ingress_id: Int, egress_id: Int, rate: Double) extends Config((site, here, up) => {
  case NoCEvalKey => up(NoCEvalKey).copy(
    flows = up(NoCEvalKey).flows + ((ingress_id, egress_id) -> rate)
  )
})
class WithEvalUniformFlow(rate: Double) extends Config((site, here, up) => {
  case NoCEvalKey => {
    val flows = site(NoCKey).flows
    val counts = (0 until site(NoCKey).ingresses.size).map { i =>
      flows.filter(_.ingressId == i).size
    }
    up(NoCEvalKey).copy(
      flows = ListMap(flows.map {
        case FlowParams(i,e,_) => ((i, e) -> rate / counts(i))
      }:_*)
    )
  }
})

class WithEvalRequiredThroughput(t: Double) extends Config((site, here, up) => {
  case NoCEvalKey => up(NoCEvalKey).copy(requiredThroughput=t)
})

class WithEvalRequiredMedianLatency(t: Int) extends Config((site, here, up) => {
  case NoCEvalKey => up(NoCEvalKey).copy(requiredMedianLatency=t)
})
class WithEvalRequiredMaxLatency(t: Int) extends Config((site, here, up) => {
  case NoCEvalKey => up(NoCEvalKey).copy(requiredMaxLatency=t)
})


// 1D mesh. Shared bus
class TestConfig00 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(Seq(0)) ++
  new constellation.channel.WithEgresses(Seq(1)) ++
  new constellation.routing.WithRoutingRelation(new UnidirectionalLineRouting) ++
  new constellation.topology.WithTopology(UnidirectionalLine(2)))
class TestConfig01 extends Config(
  new constellation.channel.WithUniformChannelDepth(1) ++
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(Seq(0)) ++
  new constellation.channel.WithEgresses(Seq(1, 1)) ++
  new constellation.routing.WithRoutingRelation(new UnidirectionalLineRouting) ++
  new constellation.topology.WithTopology(UnidirectionalLine(2)))
class TestConfig02 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(Seq(0, 0)) ++
  new constellation.channel.WithEgresses(Seq(1, 1)) ++
  new constellation.routing.WithRoutingRelation(new UnidirectionalLineRouting) ++
  new constellation.topology.WithTopology(UnidirectionalLine(2)))
class TestConfig03 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(Seq(0, 0)) ++
  new constellation.channel.WithEgresses (Seq(0, 1, 1)) ++
  new constellation.routing.WithRoutingRelation(new UnidirectionalLineRouting) ++
  new constellation.topology.WithTopology(UnidirectionalLine(2)))
class TestConfig04 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(Seq(0, 0)) ++
  new constellation.channel.WithEgresses(Seq(0, 1, 1, 2, 2)) ++
  new constellation.routing.WithRoutingRelation(new UnidirectionalLineRouting) ++
  new constellation.topology.WithTopology(UnidirectionalLine(3, Seq((0, 2)))))
class TestConfig05 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(Seq(0, 1, 1)) ++
  new constellation.channel.WithEgresses(Seq(1, 1, 2)) ++
  new constellation.routing.WithRoutingRelation(new UnidirectionalLineRouting) ++
  new constellation.topology.WithTopology(UnidirectionalLine(3)))
class TestConfig06 extends Config(
  new constellation.router.WithCoupleSAVA ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(Seq(0)) ++
  new constellation.channel.WithEgresses(Seq(1)) ++
  new constellation.routing.WithRoutingRelation(new UnidirectionalLineRouting) ++
  new constellation.topology.WithTopology(UnidirectionalLine(2)))
class TestConfig07 extends Config(
  new constellation.noc.WithCtrl ++
  new constellation.router.WithCoupleSAVA ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(Seq(0)) ++
  new constellation.channel.WithEgresses(Seq(1)) ++
  new constellation.routing.WithRoutingRelation(new UnidirectionalLineRouting) ++
  new constellation.topology.WithTopology(UnidirectionalLine(2)))
class TestConfig08 extends Config(
  new constellation.router.WithPrioritizingSingleVCAllocator ++
  new constellation.router.WithCoupleSAVA ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(Seq(0)) ++
  new constellation.channel.WithEgresses(Seq(1, 2, 3, 4, 5)) ++
  new constellation.routing.WithRoutingRelation(new UnidirectionalLineRouting) ++
  new constellation.topology.WithTopology(UnidirectionalLine(7, Seq((0, 2), (2, 4), (4, 6)))))



class TestConfig09 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 2) ++
  new constellation.channel.WithEgresses(0 until 2) ++
  new constellation.routing.WithRoutingRelation(new BidirectionalLineRouting) ++
  new constellation.topology.WithTopology(BidirectionalLine(2)))
class TestConfig10 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 2) ++
  new constellation.channel.WithEgresses(0 until 3) ++
  new constellation.routing.WithRoutingRelation(new BidirectionalLineRouting) ++
  new constellation.topology.WithTopology(BidirectionalLine(3)))
class TestConfig11 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(1 until 3) ++
  new constellation.channel.WithEgresses(0 until 4) ++
  new constellation.routing.WithRoutingRelation(new BidirectionalLineRouting) ++
  new constellation.topology.WithTopology(BidirectionalLine(4)))
class TestConfig12 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(Seq(1, 1, 2, 2)) ++
  new constellation.channel.WithEgresses(Seq(0, 0, 1, 1, 2, 2, 3, 3)) ++
  new constellation.routing.WithRoutingRelation(new BidirectionalLineRouting) ++
  new constellation.topology.WithTopology(BidirectionalLine(4)))

// 1D Torus
class TestConfig13 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(Seq(0)) ++
  new constellation.channel.WithEgresses(Seq(1)) ++
  new constellation.routing.WithRoutingRelation(new UnidirectionalTorus1DDatelineRouting(2)) ++
  new constellation.topology.WithTopology(UnidirectionalTorus1D(2)))
class TestConfig14 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(Seq(0, 2)) ++
  new constellation.channel.WithEgresses(Seq(1, 3)) ++
  new constellation.routing.WithRoutingRelation(new UnidirectionalTorus1DDatelineRouting(4)) ++
  new constellation.topology.WithTopology(UnidirectionalTorus1D(4)))
class TestConfig15 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 10 by 2) ++
  new constellation.channel.WithEgresses(1 until 10 by 2) ++
  new constellation.routing.WithRoutingRelation(new UnidirectionalTorus1DDatelineRouting(10)) ++
  new constellation.topology.WithTopology(UnidirectionalTorus1D(10)))
class TestConfig16 extends Config(
  new WithInputPacketStallProbability(0.8) ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 10) ++
  new constellation.channel.WithEgresses(0 until 10) ++
  new constellation.routing.WithRoutingRelation(new UnidirectionalTorus1DDatelineRouting(10)) ++
  new constellation.topology.WithTopology(UnidirectionalTorus1D(10)))

class TestConfig17 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 2) ++
  new constellation.channel.WithEgresses(0 until 2) ++
  new constellation.routing.WithRoutingRelation(new BidirectionalTorus1DShortestRouting(2)) ++
  new constellation.topology.WithTopology(BidirectionalTorus1D(2)))
class TestConfig18 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 4 by 2) ++
  new constellation.channel.WithEgresses(1 until 4 by 2) ++
  new constellation.routing.WithRoutingRelation(new BidirectionalTorus1DShortestRouting(4)) ++
  new constellation.topology.WithTopology(BidirectionalTorus1D(4)))
class TestConfig19 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 10) ++
  new constellation.channel.WithEgresses(0 until 10) ++
  new constellation.routing.WithRoutingRelation(new BidirectionalTorus1DShortestRouting(10)) ++
  new constellation.topology.WithTopology(BidirectionalTorus1D(10)))

class TestConfig20 extends Config(
  new WithInputFlitStallProbability(0.8) ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses((0 until 10)) ++
  new constellation.channel.WithEgresses((0 until 10)) ++
  new constellation.routing.WithRoutingRelation(new BidirectionalTorus1DRandomRouting(10)) ++
  new constellation.topology.WithTopology(BidirectionalTorus1D(10)))
class TestConfig21 extends Config(
  new WithInputPacketStallProbability(0.9) ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithUniformChannelSrcMultiplier(2) ++
  new constellation.channel.WithUniformChannelDestMultiplier(2) ++
  new constellation.channel.WithIngresses((0 until 10) ++ (0 until 10)) ++
  new constellation.channel.WithEgresses((0 until 10) ++ (0 until 10)) ++
  new constellation.routing.WithRoutingRelation(new BidirectionalTorus1DRandomRouting(10)) ++
  new constellation.topology.WithTopology(BidirectionalTorus1D(10)))

// Butterfly
class TestConfig22 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses((0 until 2) ++ (0 until 2)) ++
  new constellation.channel.WithEgresses(((0 until 2) ++ (0 until 2)).map(_ + 2*1)) ++
  new constellation.routing.WithRoutingRelation(new ButterflyRouting(2, 2)) ++
  new constellation.topology.WithTopology(Butterfly(2, 2)))
class TestConfig23 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses((0 until 4) ++ (0 until 4)) ++
  new constellation.channel.WithEgresses(((0 until 4) ++ (0 until 4)).map(_ + 4*2)) ++
  new constellation.routing.WithRoutingRelation(new ButterflyRouting(2, 3)) ++
  new constellation.topology.WithTopology(Butterfly(2, 3)))
class TestConfig24 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses((0 until 8) ++ (0 until 8)) ++
  new constellation.channel.WithEgresses(((0 until 8) ++ (0 until 8)).map(_ + 8*3)) ++
  new constellation.routing.WithRoutingRelation(new ButterflyRouting(2, 4)) ++
  new constellation.topology.WithTopology(Butterfly(2, 4)))
class TestConfig25 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses((0 until 3) ++ (0 until 3)) ++
  new constellation.channel.WithEgresses(((0 until 3) ++ (0 until 3)).map(_ + 3*1)) ++
  new constellation.routing.WithRoutingRelation(new ButterflyRouting(3, 2)) ++
  new constellation.topology.WithTopology(Butterfly(3, 2)))
class TestConfig26 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses((0 until 9) ++ (0 until 9)) ++
  new constellation.channel.WithEgresses(((0 until 9) ++ (0 until 9)).map(_ + 9*2)) ++
  new constellation.routing.WithRoutingRelation(new ButterflyRouting(3, 3)) ++
  new constellation.topology.WithTopology(Butterfly(3, 3)))

// Tree Topologies
class TestConfig27 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 6 by 2) ++
  new constellation.channel.WithEgresses(1 until 6 by 2) ++
  new constellation.routing.WithRoutingRelation(new BidirectionalTreeRouting(2)) ++
  new constellation.topology.WithTopology(BidirectionalTree(3, 2)))
class TestConfig28 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 6) ++
  new constellation.channel.WithEgresses(0 until 6) ++
  new constellation.channel.WithFatTreeChannels(1) ++
  new constellation.routing.WithRoutingRelation(new BidirectionalTreeRouting(2)) ++
  new constellation.topology.WithTopology(BidirectionalTree(3, 2)))
class TestConfig29 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(13 until 40) ++
  new constellation.channel.WithEgresses(13 until 40) ++
  new constellation.channel.WithFatTreeChannels(1) ++
  new constellation.routing.WithRoutingRelation(new BidirectionalTreeRouting(3)) ++
  new constellation.topology.WithTopology(BidirectionalTree(3, 3)))

// 2D Mesh
class TestConfig30 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.routing.WithRoutingRelation(new Mesh2DDimensionOrderedRouting(3, 3)) ++
  new constellation.topology.WithTopology(Mesh2D(3, 3)))
class TestConfig31 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 18) ++
  new constellation.channel.WithEgresses(0 until 18) ++
  new constellation.routing.WithRoutingRelation(new Mesh2DDimensionOrderedRouting(3, 6)) ++
  new constellation.topology.WithTopology(Mesh2D(3, 6)))
class TestConfig32 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 25) ++
  new constellation.channel.WithEgresses(0 until 25) ++
  new constellation.routing.WithRoutingRelation(new Mesh2DDimensionOrderedRouting(5, 5)) ++
  new constellation.topology.WithTopology(Mesh2D(5, 5)))
class TestConfig33 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 25) ++
  new constellation.channel.WithEgresses(0 until 25) ++
  new constellation.routing.WithRoutingRelation(new Mesh2DDimensionOrderedRouting(5, 5, 1)) ++
  new constellation.topology.WithTopology(Mesh2D(5, 5)))
class TestConfig34 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 25) ++
  new constellation.channel.WithEgresses(0 until 25) ++
  new constellation.router.WithRotatingSingleVCAllocator ++
  new constellation.routing.WithRoutingRelation(new Mesh2DEscapeRouting(5, 5)) ++
  new constellation.topology.WithTopology(Mesh2D(5, 5)))
class TestConfig35 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 25) ++
  new constellation.channel.WithEgresses(0 until 25) ++
  new constellation.router.WithPrioritizingSingleVCAllocator ++
  new constellation.routing.WithRoutingRelation(new Mesh2DEscapeRouting(5, 5)) ++
  new constellation.topology.WithTopology(Mesh2D(5, 5)))

class TestConfig36 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.routing.WithRoutingRelation(new Mesh2DEscapeRouting(3, 3)) ++
  new constellation.topology.WithTopology(Mesh2D(3, 3)))
class TestConfig37 extends Config(
  new constellation.router.WithCombineRCVA ++
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.routing.WithRoutingRelation(new Mesh2DEscapeRouting(3, 3)) ++
  new constellation.topology.WithTopology(Mesh2D(3, 3)))
class TestConfig38 extends Config(
  new constellation.router.WithCombineSAST ++
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.routing.WithRoutingRelation(new Mesh2DEscapeRouting(3, 3)) ++
  new constellation.topology.WithTopology(Mesh2D(3, 3)))
class TestConfig39 extends Config(
  new constellation.router.WithCombineSAST ++
  new constellation.router.WithCombineRCVA ++
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.routing.WithRoutingRelation(new Mesh2DEscapeRouting(3, 3)) ++
  new constellation.topology.WithTopology(Mesh2D(3, 3)))
class TestConfig40 extends Config(
  new constellation.router.WithISLIPMultiVCAllocator ++
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.routing.WithRoutingRelation(new Mesh2DEscapeRouting(3, 3)) ++
  new constellation.topology.WithTopology(Mesh2D(3, 3)))
class TestConfig41 extends Config(
  new constellation.router.WithEarlyRC ++
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.routing.WithRoutingRelation(new Mesh2DEscapeRouting(3, 3)) ++
  new constellation.topology.WithTopology(Mesh2D(3, 3)))
class TestConfig42 extends Config(
  new constellation.router.WithEarlyRC ++
  new constellation.router.WithCombineRCVA ++
  new constellation.router.WithCombineSAST ++
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.routing.WithRoutingRelation(new Mesh2DEscapeRouting(3, 3)) ++
  new constellation.topology.WithTopology(Mesh2D(3, 3)))
class TestConfig43 extends Config(
  new constellation.channel.WithUniformChannelDestMultiplier(2) ++
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.routing.WithRoutingRelation(new Mesh2DEscapeRouting(3, 3)) ++
  new constellation.topology.WithTopology(Mesh2D(3, 3)))
class TestConfig44 extends Config(
  new constellation.channel.WithUniformChannelSrcMultiplier(2) ++
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.routing.WithRoutingRelation(new Mesh2DEscapeRouting(3, 3)) ++
  new constellation.topology.WithTopology(Mesh2D(3, 3)))
class TestConfig45 extends Config(
  new constellation.channel.WithUniformChannelSrcMultiplier(2) ++
  new constellation.channel.WithUniformChannelDestMultiplier(2) ++
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.routing.WithRoutingRelation(new Mesh2DEscapeRouting(3, 3)) ++
  new constellation.topology.WithTopology(Mesh2D(3, 3)))


class TestConfig46 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(1)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 25) ++
  new constellation.channel.WithEgresses(0 until 25) ++
  new constellation.routing.WithRoutingRelation(new Mesh2DDimensionOrderedRouting(5, 5)) ++
  new constellation.topology.WithTopology(Mesh2D(5, 5)))
class TestConfig47 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(1)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 25) ++
  new constellation.channel.WithEgresses(0 until 25) ++
  new constellation.routing.WithRoutingRelation(new Mesh2DWestFirstRouting(5, 5)) ++
  new constellation.topology.WithTopology(Mesh2D(5, 5)))
class TestConfig48 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(1)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 25) ++
  new constellation.channel.WithEgresses(0 until 25) ++
  new constellation.routing.WithRoutingRelation(new Mesh2DNorthLastRouting(5, 5)) ++
  new constellation.topology.WithTopology(Mesh2D(5, 5)))

class TestConfig49 extends Config(
  new constellation.routing.WithNBlockingVirtualNetworks(4) ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngressVNets((i: Int) => i % 4) ++
  new constellation.channel.WithEgressVNets ((i: Int) => (i + 2) % 4) ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.routing.WithRoutingRelation(new Mesh2DEscapeRouting(3, 3)) ++
  new constellation.topology.WithTopology(Mesh2D(3, 3)))

class TestConfig50 extends Config(
  new constellation.routing.WithNBlockingVirtualNetworks(4) ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngressVNets((i: Int) => i % 4) ++
  new constellation.channel.WithEgressVNets ((i: Int) => (i + 2) % 4) ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.routing.WithRoutingRelation(new Mesh2DDimensionOrderedRouting(3, 3)) ++
  new constellation.topology.WithTopology(Mesh2D(3, 3)))

class TestConfig51 extends Config(
  new constellation.routing.WithNNonblockingVirtualNetworks(4) ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngressVNets((i: Int) => i % 4) ++
  new constellation.channel.WithEgressVNets ((i: Int) => (i + 2) % 4) ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.routing.WithRoutingRelation(new Mesh2DNorthLastRouting(3, 3)) ++
  new constellation.topology.WithTopology(Mesh2D(3, 3)))

// 2D Torus
class TestConfig52 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(1)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.routing.WithRoutingRelation(new DimensionOrderedUnidirectionalTorus2DDatelineRouting(3, 3)) ++
  new constellation.topology.WithTopology(UnidirectionalTorus2D(3, 3)))
class TestConfig53 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.routing.WithRoutingRelation(new DimensionOrderedUnidirectionalTorus2DDatelineRouting(3, 3)) ++
  new constellation.topology.WithTopology(UnidirectionalTorus2D(3, 3)))
class TestConfig54 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(4)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 25) ++
  new constellation.channel.WithEgresses(0 until 25) ++
  new constellation.routing.WithRoutingRelation(new DimensionOrderedUnidirectionalTorus2DDatelineRouting(5, 5)) ++
  new constellation.topology.WithTopology(UnidirectionalTorus2D(5, 5)))

class TestConfig55 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(1)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 9) ++
  new constellation.channel.WithEgresses(0 until 9) ++
  new constellation.routing.WithRoutingRelation(new DimensionOrderedBidirectionalTorus2DDatelineRouting(3, 3)) ++
  new constellation.topology.WithTopology(BidirectionalTorus2D(3, 3)))

// topologies which put the ingress/egress points on a separate "plane" of nodes
class TestConfig56 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithTerminalPlaneIngressEgress ++
  new constellation.channel.WithIngresses(Seq(0, 1, 1)) ++
  new constellation.channel.WithEgresses(Seq(1, 1, 2)) ++
  new constellation.routing.WithTerminalPlaneRouting ++
  new constellation.routing.WithRoutingRelation(new AllLegalRouting) ++
  new constellation.topology.WithTerminalPlane ++
  new constellation.topology.WithTopology(UnidirectionalLine(3)))
class TestConfig57 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithTerminalPlaneIngressEgress ++
  new constellation.channel.WithIngresses(Seq(1, 1, 2, 2)) ++
  new constellation.channel.WithEgresses(Seq(0, 0, 1, 1, 2, 2, 3, 3)) ++
  new constellation.routing.WithTerminalPlaneRouting ++
  new constellation.routing.WithRoutingRelation(new BidirectionalLineRouting) ++
  new constellation.topology.WithTerminalPlane ++
  new constellation.topology.WithTopology(BidirectionalLine(4)))
class TestConfig58 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithTerminalPlaneIngressEgress ++
  new constellation.channel.WithIngresses((0 until 6)) ++
  new constellation.channel.WithEgresses((0 until 6)) ++
  new constellation.routing.WithTerminalPlaneRouting ++
  new constellation.routing.WithRoutingRelation(new UnidirectionalTorus1DDatelineRouting(6)) ++
  new constellation.topology.WithTerminalPlane ++
  new constellation.topology.WithTopology(UnidirectionalTorus1D(6)))
class TestConfig59 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithTerminalPlaneIngressEgress ++
  new constellation.channel.WithIngresses((0 until 6) ++ (0 until 6)) ++
  new constellation.channel.WithEgresses((0 until 6) ++ (0 until 6)) ++
  new constellation.routing.WithTerminalPlaneRouting ++
  new constellation.routing.WithRoutingRelation(new BidirectionalTorus1DShortestRouting(6)) ++
  new constellation.topology.WithTerminalPlane ++
  new constellation.topology.WithTopology(BidirectionalTorus1D(6)))
class TestConfig60 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithTerminalPlaneIngressEgress ++
  new constellation.channel.WithIngresses((0 until 25)) ++
  new constellation.channel.WithEgresses ((0 until 25)) ++
  new constellation.routing.WithTerminalPlaneRouting ++
  new constellation.routing.WithRoutingRelation(new Mesh2DEscapeRouting(5, 5)) ++
  new constellation.topology.WithTerminalPlane ++
  new constellation.topology.WithTopology(Mesh2D(5, 5)))
class TestConfig61 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithTerminalPlaneIngressEgress ++
  new constellation.channel.WithIngresses((0 until 25)) ++
  new constellation.channel.WithEgresses ((0 until 25)) ++
  new constellation.routing.WithTerminalPlaneRouting ++
  new constellation.routing.WithRoutingRelation(new Mesh2DEscapeRouting(5, 5)) ++
  new constellation.topology.WithTerminalPlane ++
  new constellation.topology.WithTopology(Mesh2D(5, 5)))
class TestConfig62 extends Config(
  new constellation.routing.WithNBlockingVirtualNetworks(4) ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithTerminalPlaneIngressEgress ++
  new constellation.channel.WithIngressVNets((i: Int) => i % 4) ++
  new constellation.channel.WithEgressVNets((i: Int) => (i + 1) % 4) ++
  new constellation.channel.WithIngresses(0 until 16) ++
  new constellation.channel.WithEgresses (0 until 16) ++
  new constellation.routing.WithTerminalPlaneRouting ++
  new constellation.routing.WithRoutingRelation(new Mesh2DEscapeRouting(4, 4)) ++
  new constellation.topology.WithTerminalPlane ++
  new constellation.topology.WithTopology(Mesh2D(4, 4)))
class TestConfig63 extends Config(
  new constellation.test.WithInputFlitStallProbability(0.9) ++
  new constellation.router.WithSafeCoupleSAVA ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithTerminalPlaneIngressEgress ++
  new constellation.channel.WithIngresses(4 until 16) ++
  new constellation.channel.WithEgresses(0 until 4) ++
  new constellation.routing.WithTerminalPlaneRouting ++
  new constellation.routing.WithRoutingRelation(new Mesh2DEscapeRouting(4, 4)) ++
  new constellation.topology.WithTerminalPlane ++
  new constellation.topology.WithTopology(Mesh2D(4, 4)))
class TestConfig64 extends Config(
  new constellation.test.WithInputFlitStallProbability(0.9) ++
  new constellation.router.WithSafeCoupleSAVA ++
  new constellation.noc.WithCtrl ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithTerminalPlaneIngressEgress ++
  new constellation.channel.WithIngresses(4 until 16) ++
  new constellation.channel.WithEgresses(0 until 4) ++
  new constellation.routing.WithTerminalPlaneRouting ++
  new constellation.routing.WithRoutingRelation(new Mesh2DEscapeRouting(4, 4)) ++
  new constellation.topology.WithTerminalPlane ++
  new constellation.topology.WithTopology(Mesh2D(4, 4)))
class TestConfig65 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithTerminalPlaneIngressEgress ++
  new constellation.channel.WithIngresses(0 until 16) ++
  new constellation.channel.WithEgresses(0 until 16) ++
  new constellation.routing.WithTerminalPlaneRouting ++
  new constellation.routing.WithRoutingRelation(new Mesh2DEscapeRouting(4, 4)) ++
  new constellation.topology.WithTerminalPlane ++
  new constellation.topology.WithTopology(Mesh2D(4, 4)))

// test configs for channel width adapters
class TestConfig66 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(Seq(0)) ++
  new constellation.channel.WithEgresses(Seq(0)) ++
  new constellation.router.WithUniformPayloadBits(32) ++
  new constellation.routing.WithRoutingRelation(new AllLegalRouting) ++
  new constellation.topology.WithTopology(UnidirectionalLine(1)))
class TestConfig67 extends Config(
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 16) ++
  new constellation.channel.WithEgresses(0 until 16) ++
  new constellation.router.WithUniformPayloadBits(16) ++
  new constellation.routing.WithRoutingRelation(new Mesh2DEscapeRouting(4, 4)) ++
  new constellation.topology.WithTopology(Mesh2D(4, 4)))
class TestConfig68 extends Config(
  new WithTotalTxs(10000) ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngressPayloadBits(72) ++
  new constellation.channel.WithEgressPayloadBits(72) ++
  new constellation.channel.WithIngresses(Seq(0)) ++
  new constellation.channel.WithEgresses(Seq(4)) ++
  new constellation.router.WithPayloadBits(36, Seq(4)) ++
  new constellation.router.WithPayloadBits(12, Seq(3)) ++
  new constellation.router.WithPayloadBits(24, Seq(2)) ++
  new constellation.router.WithPayloadBits(12, Seq(1)) ++
  new constellation.router.WithPayloadBits(36, Seq(0)) ++
  new constellation.routing.WithRoutingRelation(new AllLegalRouting) ++
  new constellation.topology.WithTopology(UnidirectionalLine(5)))
class TestConfig69 extends Config(
  new WithInputPacketStallProbability(0.9) ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(4)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses((0 until 16)) ++
  new constellation.channel.WithEgresses((0 until 16)) ++
  new constellation.router.WithPrioritizingSingleVCAllocator ++
  new constellation.router.WithPayloadBits(8,  (3 until 16 by 4)) ++
  new constellation.router.WithPayloadBits(16, (2 until 16 by 4)) ++
  new constellation.router.WithPayloadBits(32, (1 until 16 by 4)) ++
  new constellation.router.WithPayloadBits(64, (0 until 16 by 4)) ++
  new constellation.routing.WithRoutingRelation(new Mesh2DEscapeRouting(4, 4)) ++
  new constellation.topology.WithTopology(Mesh2D(4, 4)))


// test configs for TL network
class TLTestConfig00 extends Config(
  new WithTLNoCTesterParams(TLNoCTesterParams(Seq(0), Seq(1))) ++
  new constellation.routing.WithNNonblockingVirtualNetworks(5) ++
  new constellation.channel.WithUniformNVirtualChannels(5, UserVirtualChannelParams(1)) ++
  new constellation.routing.WithRoutingRelation(new BidirectionalLineRouting) ++
  new constellation.topology.WithTopology(BidirectionalLine(2)))

class TLTestConfig01 extends Config(
  new WithTLNoCTesterParams(TLNoCTesterParams(Seq(4, 0, 2, 5, 6, 9, 11), Seq(7, 1, 3, 8, 10))) ++
  new constellation.routing.WithNNonblockingVirtualNetworksWithSharing(5, 2) ++
  new constellation.channel.WithUniformNVirtualChannels(7, UserVirtualChannelParams(3)) ++
  new constellation.routing.WithRoutingRelation(new Mesh2DEscapeRouting(4, 3)) ++
  new constellation.topology.WithTopology(Mesh2D(4, 3)))

class TLTestConfig02 extends Config(
  new WithTLNoCTesterParams(TLNoCTesterParams(Seq(4, 0, 2, 5, 6, 9, 11), Seq(7, 1, 3, 8, 10))) ++
  new constellation.routing.WithNBlockingVirtualNetworks(5) ++
  new constellation.channel.WithUniformNVirtualChannels(5, UserVirtualChannelParams(3)) ++
  new constellation.routing.WithRoutingRelation(new Mesh2DEscapeRouting(4, 3)) ++
  new constellation.topology.WithTopology(Mesh2D(4, 3)))

class TLTestConfig03 extends Config(
  new WithTLNoCTesterParams(TLNoCTesterParams(Seq(0, 1, 2), Seq(3, 4, 5))) ++
  new constellation.routing.WithNNonblockingVirtualNetworks(5) ++
  new constellation.channel.WithUniformNVirtualChannels(10, UserVirtualChannelParams(3)) ++
  new constellation.routing.WithRoutingRelation(new UnidirectionalTorus1DDatelineRouting(6)) ++
  new constellation.topology.WithTopology(UnidirectionalTorus1D(6)))

class TLTestConfig04 extends Config(
  new WithTLNoCTesterParams(TLNoCTesterParams(Seq(4, 0, 2, 5, 6, 9, 11), Seq(7, 1, 3, 8, 10))) ++
  new constellation.routing.WithNBlockingVirtualNetworks(5) ++
  new constellation.channel.WithUniformNVirtualChannels(5, UserVirtualChannelParams(3)) ++
  new constellation.routing.WithTerminalPlaneRouting ++
  new constellation.routing.WithRoutingRelation(new Mesh2DEscapeRouting(4, 3)) ++
  new constellation.topology.WithTerminalPlane ++
  new constellation.topology.WithTopology(Mesh2D(4, 3)))

class TLTestConfig05 extends Config(
  new WithTLNoCTesterParams(TLNoCTesterParams(Seq(4, 0, 2, 5, 6, 9, 11), Seq(7, 1, 3, 8, 10),
    explicitPayloadWidth=Some(32))) ++
  new constellation.routing.WithNBlockingVirtualNetworks(5) ++
  new constellation.channel.WithUniformNVirtualChannels(5, UserVirtualChannelParams(3)) ++
  new constellation.routing.WithTerminalPlaneRouting ++
  new constellation.routing.WithRoutingRelation(new Mesh2DEscapeRouting(4, 3)) ++
  new constellation.topology.WithTerminalPlane ++
  new constellation.topology.WithTopology(Mesh2D(4, 3)))

class TLTestConfig06 extends Config(
  new WithTLNoCTesterParams(TLNoCTesterParams(Seq(4, 0, 2, 5, 6, 9, 11), Seq(7, 1, 3, 8, 10), delay=0.0)) ++
  new constellation.router.WithPrioritizingSingleVCAllocator ++
  new constellation.routing.WithNNonblockingVirtualNetworksWithSharing(5, 10) ++
  new constellation.channel.WithUniformNVirtualChannels(15, UserVirtualChannelParams(7)) ++
  new constellation.routing.WithTerminalPlaneRouting ++
  new constellation.routing.WithRoutingRelation(new Mesh2DEscapeRouting(4, 3)) ++
  new constellation.topology.WithTerminalPlane ++
  new constellation.topology.WithTopology(Mesh2D(4, 3)))


// test configs for AXI4 network
class AXI4TestConfig00 extends Config(
  new WithAXI4NoCTesterParams(AXI4NoCTesterParams(Seq(0), Seq(1))) ++
  new constellation.routing.WithNNonblockingVirtualNetworks(5) ++
  new constellation.channel.WithUniformNVirtualChannels(5, UserVirtualChannelParams(5)) ++
  new constellation.routing.WithRoutingRelation(new BidirectionalLineRouting) ++
  new constellation.topology.WithTopology(BidirectionalLine(2)))
class AXI4TestConfig01 extends Config(
  new WithAXI4NoCTesterParams(AXI4NoCTesterParams(Seq(0, 2), Seq(1))) ++
  new constellation.routing.WithNNonblockingVirtualNetworks(5) ++
  new constellation.channel.WithUniformNVirtualChannels(5, UserVirtualChannelParams(5)) ++
  new constellation.routing.WithTerminalPlaneRouting ++
  new constellation.routing.WithRoutingRelation(new BidirectionalLineRouting) ++
  new constellation.topology.WithTerminalPlane ++
  new constellation.topology.WithTopology(BidirectionalLine(3)))
class AXI4TestConfig02 extends Config(
  new WithAXI4NoCTesterParams(AXI4NoCTesterParams(Seq(0, 1, 2, 3, 5, 6, 7, 8), Seq(4))) ++
  new constellation.routing.WithNNonblockingVirtualNetworks(5) ++
  new constellation.channel.WithUniformNVirtualChannels(10, UserVirtualChannelParams(3)) ++
  new constellation.routing.WithRoutingRelation(new Mesh2DEscapeRouting(3, 3)) ++
  new constellation.topology.WithTopology(Mesh2D(3, 3)))
class AXI4TestConfig03 extends Config(
  new WithAXI4NoCTesterParams(AXI4NoCTesterParams(Seq(1, 3, 5, 7), Seq(0, 2, 4, 6, 8))) ++
  new constellation.routing.WithNNonblockingVirtualNetworks(5) ++
  new constellation.channel.WithUniformNVirtualChannels(10, UserVirtualChannelParams(3)) ++
  new constellation.routing.WithTerminalPlaneRouting ++
  new constellation.routing.WithRoutingRelation(new Mesh2DEscapeRouting(3, 3)) ++
  new constellation.topology.WithTerminalPlane ++
  new constellation.topology.WithTopology(Mesh2D(3, 3)))


// Performance eval configs
class EvalTestConfig00 extends Config(
  new WithEvalRequiredThroughput(0.79) ++
  new WithEvalFlow(0, 0, 1.0) ++
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(Seq(0)) ++
  new constellation.channel.WithEgresses(Seq(1)) ++
  new constellation.routing.WithRoutingRelation(new UnidirectionalLineRouting) ++
  new constellation.topology.WithTopology(UnidirectionalLine(2)))
class EvalTestConfig01 extends Config(
  new WithEvalRequiredThroughput(0.85) ++
  new WithEvalUniformFlow(0.5) ++
  new constellation.channel.WithUniformNVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(Seq(0, 0)) ++
  new constellation.channel.WithEgresses(Seq(1, 1)) ++
  new constellation.routing.WithRoutingRelation(new UnidirectionalLineRouting) ++
  new constellation.topology.WithTopology(UnidirectionalLine(2)))
class EvalTestConfig02 extends Config(
  new WithEvalRequiredMedianLatency(150) ++
  new WithEvalRequiredMaxLatency(260) ++
  new WithEvalRequiredThroughput(0.99) ++
  new WithEvalUniformFlow(1.0) ++
  new constellation.router.WithCoupleSAVA ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(Seq(0)) ++
  new constellation.channel.WithEgresses(Seq(1)) ++
  new constellation.routing.WithRoutingRelation(new UnidirectionalLineRouting) ++
  new constellation.topology.WithTopology(UnidirectionalLine(2)))
class EvalTestConfig03 extends Config(
  new WithEvalRequiredMedianLatency(30) ++
  new WithEvalRequiredMaxLatency(175) ++
  new WithEvalRequiredThroughput(0.9) ++
  new WithEvalUniformFlow(0.1) ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 10) ++
  new constellation.channel.WithEgresses(0 until 10) ++
  new constellation.routing.WithRoutingRelation(new UnidirectionalTorus1DDatelineRouting(10)) ++
  new constellation.topology.WithTopology(UnidirectionalTorus1D(10)))
class EvalTestConfig04 extends Config(
  new WithEvalRequiredThroughput(0.69) ++
  new WithEvalUniformFlow(0.4) ++
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(5)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses((0 until 4) ++ (0 until 4)) ++
  new constellation.channel.WithEgresses(((0 until 4) ++ (0 until 4)).map(_ + 4*2)) ++
  new constellation.routing.WithRoutingRelation(new ButterflyRouting(2, 3)) ++
  new constellation.topology.WithTopology(Butterfly(2, 3)))
class EvalTestConfig05 extends Config(
  new WithEvalRequiredMedianLatency(75) ++
  new WithEvalRequiredMaxLatency(1400) ++
  new WithEvalRequiredThroughput(0.90) ++
  new WithEvalUniformFlow(0.5) ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(4)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 25) ++
  new constellation.channel.WithEgresses(0 until 25) ++
  new constellation.router.WithRotatingSingleVCAllocator ++
  new constellation.routing.WithRoutingRelation(new Mesh2DEscapeRouting(5, 5)) ++
  new constellation.topology.WithTopology(Mesh2D(5, 5)))
class EvalTestConfig06 extends Config(
  new WithEvalRequiredMedianLatency(25) ++
  new WithEvalRequiredMaxLatency(125) ++
  new WithEvalRequiredThroughput(0.94) ++
  new WithEvalUniformFlow(0.2) ++
  new constellation.channel.WithUniformNVirtualChannels(1, UserVirtualChannelParams(4)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 16) ++
  new constellation.channel.WithEgresses(0 until 16) ++
  new constellation.routing.WithRoutingRelation(new Mesh2DDimensionOrderedRouting(4, 4)) ++
  new constellation.topology.WithTopology(Mesh2D(4, 4)))
class EvalTestConfig07 extends Config(
  new WithEvalRequiredMedianLatency(20) ++
  new WithEvalRequiredMaxLatency(75) ++
  new WithEvalRequiredThroughput(0.95) ++
  new WithEvalUniformFlow(0.2) ++
  new constellation.channel.WithUniformNVirtualChannels(2, UserVirtualChannelParams(4)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(0 until 16) ++
  new constellation.channel.WithEgresses(0 until 16) ++
  new constellation.routing.WithRoutingRelation(new Mesh2DEscapeRouting(4, 4)) ++
  new constellation.topology.WithTopology(new Mesh2D(4, 4)))
class EvalTestConfig08 extends Config(
  new WithEvalRequiredMedianLatency(35) ++
  new WithEvalRequiredMaxLatency(250) ++
  new WithEvalRequiredThroughput(0.95) ++
  new WithEvalUniformFlow(0.5) ++
  new constellation.channel.WithUniformNVirtualChannels(4, UserVirtualChannelParams(4)) ++
  new constellation.channel.WithFullyConnectedIngresses ++
  new constellation.channel.WithIngresses(Seq.fill(2) { 0 until 8 }.flatten) ++
  new constellation.channel.WithEgresses((Seq.fill(2) { 0 until 8 }.flatten).map(_ + 8*3)) ++
  new constellation.routing.WithRoutingRelation(new ButterflyRouting(2, 4)) ++
  new constellation.topology.WithTopology(new Butterfly(2, 4)))
