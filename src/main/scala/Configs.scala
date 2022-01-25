package constellation

import scala.math.pow
import scala.language.implicitConversions

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters, Config}

import constellation.topology._
import constellation.routing._

class WithCombineRCVA extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(routerParams = (i: Int) =>
    up(NoCKey, site).routerParams(i).copy(combineRCVA = true)
  )
})

class WithCombineSAST extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(routerParams = (i: Int) =>
    up(NoCKey, site).routerParams(i).copy(combineSAST = true)
  )
})


class WithUniformChannelDepth(depth: Int) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(channelParamGen = (src: Int, dst: Int) => {
    up(NoCKey, site).channelParamGen(src, dst).copy(channel = (u: Parameters) => {
      implicit val p: Parameters = u
      ChannelBuffer(depth) := _
    })
  })
})

class WithUniformVirtualChannelBufferSize(size: Int) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(channelParamGen = (src: Int, dst: Int) => {
    val cp = up(NoCKey, site).channelParamGen(src, dst)
    cp.copy(virtualChannelParams = cp.virtualChannelParams.map(_.copy(bufferSize = size)))
  })
})

class WithNNonblockingVirtualNetworks(n: Int) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    routingRelation = RoutingRelations.nonblockingVirtualSubnetworks(
      up(NoCKey, site).routingRelation, n),
    channelParamGen = (src: Int, dst: Int) => {
      val cp = up(NoCKey, site).channelParamGen(src, dst)
      cp.copy(virtualChannelParams = cp.virtualChannelParams.map(c => Seq.fill(n) { c }).flatten)
    },
    nVirtualNetworks = n
  )
})

class WithNBlockingVirtualNetworks(n: Int) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    routingRelation = RoutingRelations.blockingVirtualSubnetworks(
      up(NoCKey, site).routingRelation, n),
    nVirtualNetworks = n,
    vNetBlocking = (blocker: Int, blockee: Int) => blocker < blockee
  )
})

class WithNNonblockingVirtualNetworksWithSharing(n: Int, nSharedChannels: Int = 1) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    routingRelation = RoutingRelations.sharedNonblockingVirtualSubnetworks(up(NoCKey, site).routingRelation, n, nSharedChannels),
    channelParamGen = (src: Int, dst: Int) => {
      val cp = up(NoCKey, site).channelParamGen(src, dst)
      cp.copy(virtualChannelParams = Seq.fill(n) { cp.virtualChannelParams(0) } ++ cp.virtualChannelParams)
    },
    nVirtualNetworks = n
  )
})



class WithVirtualChannels(v: Seq[UserVirtualChannelParams]) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(channelParamGen = (src: Int, dst: Int) =>
    up(NoCKey, site).channelParamGen(src, dst).copy(
      virtualChannelParams = v
    )
  )
})
class WithUniformVirtualChannels(n: Int, v: UserVirtualChannelParams) extends WithVirtualChannels(Seq.fill(n)(v))


class WithIngressVNets(f: Int => Int) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(ingresses = up(NoCKey, site).ingresses.zipWithIndex.map { case (u,i) =>
    u.copy(vNetId = f(i))
  })
})

class UnidirectionalLineConfig(
  nNodes: Int = 2,
  ingressNodes: Seq[Int] = Seq(0),
  egressNodes: Seq[Int] = Seq(1)
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    topology = new UnidirectionalLine(nNodes),
    ingresses = ingressNodes.map(i => UserIngressParams(i, (0 until egressNodes.size).toSet)),
    egresses = egressNodes.map(i => UserEgressParams(i))
  )
})

class BidirectionalLineConfig(
  nNodes: Int = 2,
  ingressNodes: Seq[Int] = Seq(0, 1),
  egressNodes: Seq[Int] = Seq(0, 1),
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    topology = new BidirectionalLine(nNodes),
    routingRelation = RoutingRelations.bidirectionalLine,
    ingresses = ingressNodes.map(i => UserIngressParams(i, (0 until egressNodes.size).toSet)),
    egresses = egressNodes.map(i => UserEgressParams(i))
  )
})

class UnidirectionalTorus1DConfig(
  nNodes: Int = 2,
  ingressNodes: Seq[Int] = Seq(0),
  egressNodes: Seq[Int] = Seq(1),
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    topology = new UnidirectionalTorus1D(nNodes),
    routingRelation = RoutingRelations.unidirectionalTorus1DDateline(nNodes),
    ingresses = ingressNodes.map(i => UserIngressParams(i, (0 until egressNodes.size).toSet)),
    egresses = egressNodes.map(i => UserEgressParams(i))
  )
})

class BidirectionalTorus1DConfig(
  nNodes: Int = 2,
  ingressNodes: Seq[Int] = Seq(0),
  egressNodes: Seq[Int] = Seq(1),
  randomRoute: Boolean = false
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    topology = new BidirectionalTorus1D(nNodes),
    routingRelation = if (randomRoute) {
      RoutingRelations.bidirectionalTorus1DRandom(nNodes)
    } else {
      RoutingRelations.bidirectionalTorus1DShortest(nNodes)
    },
    ingresses = ingressNodes.map(i => UserIngressParams(i, (0 until egressNodes.size).toSet)),
    egresses = egressNodes.map(i => UserEgressParams(i))
  )
})

class ButterflyConfig(
  kAry: Int = 2,
  nFly: Int = 2
) extends Config((site, here, up) => {
  case NoCKey => {
    val height = pow(kAry,nFly-1).toInt
    up(NoCKey, site).copy(
      topology = new Butterfly(kAry, nFly),
      routingRelation = RoutingRelations.butterfly(kAry, nFly),
      ingresses = ((0 until height) ++ (0 until height)).map(
        i => UserIngressParams(i, (0 until 2*height).toSet, 0)),
      egresses = ((0 until height) ++ (0 until height)).map(
        i => UserEgressParams(i + height*(nFly-1)))
    )
  }
})

class Mesh2DConfig(
  nX: Int = 3,
  nY: Int = 3,
  routingRelation: (Int, Int) => RoutingRelation = RoutingRelations.mesh2DDimensionOrdered()
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    topology = new Mesh2D(nX, nY),
    routingRelation = routingRelation(nX, nY),
    ingresses = (0 until nX * nY).map(i => UserIngressParams(i, (0 until nX * nY).toSet, 0)),
    egresses = (0 until nX * nY).map(i => UserEgressParams(i))
  )
})

class UnidirectionalTorus2DConfig(
  nX: Int = 3,
  nY: Int = 3,
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    topology = new UnidirectionalTorus2D(nX, nY),
    routingRelation = RoutingRelations.dimensionOrderedUnidirectionalTorus2DDateline(nX, nY),
    ingresses = (0 until nX * nY).map(i => UserIngressParams(i, (0 until nX * nY).toSet, 0)),
    egresses = (0 until nX * nY).map(i => UserEgressParams(i))
  )
})


class BidirectionalTorus2DConfig(
  nX: Int = 3,
  nY: Int = 3,
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    topology = new BidirectionalTorus2D(nX, nY),
    routingRelation = RoutingRelations.dimensionOrderedBidirectionalTorus2DDateline(nX, nY),
    ingresses = (0 until nX * nY).map(i => UserIngressParams(i, (0 until nX * nY).toSet, 0)),
    egresses = (0 until nX * nY).map(i => UserEgressParams(i))
  )
})



// 1D mesh. Shared bus
class TestConfig00 extends Config(
  new WithUniformVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new UnidirectionalLineConfig(2, Seq(0), Seq(1)))
class TestConfig01 extends Config(
  new WithUniformChannelDepth(1) ++
  new WithUniformVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new UnidirectionalLineConfig(2, Seq(0), Seq(1, 1)))
class TestConfig02 extends Config(
  new WithUniformVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new UnidirectionalLineConfig(2, Seq(0, 0), Seq(1, 1)))
class TestConfig03 extends Config(
  new WithUniformVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new UnidirectionalLineConfig(2, Seq(0, 0), Seq(0, 1, 1)))
class TestConfig04 extends Config(
  new WithUniformVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new UnidirectionalLineConfig(3, Seq(0, 0), Seq(0, 1, 1, 2, 2)))
class TestConfig05 extends Config(
  new WithUniformVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new UnidirectionalLineConfig(3, Seq(0, 1, 1), Seq(1, 1, 2)))
class TestConfig06 extends Config(
  new WithUniformVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new UnidirectionalLineConfig(2, Seq(0), Seq(1)))

class TestConfig07 extends Config(
  new WithUniformVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new BidirectionalLineConfig(2, Seq(0, 1), Seq(0, 1)))
class TestConfig08 extends Config(
  new WithUniformVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new BidirectionalLineConfig(3, Seq(0, 1), Seq(0, 1, 2)))
class TestConfig09 extends Config(
  new WithUniformVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new BidirectionalLineConfig(4, Seq(1, 2), Seq(0, 1, 2, 3)))
class TestConfig10 extends Config(
  new WithUniformVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new BidirectionalLineConfig(4, Seq(1, 1, 2, 2), Seq(0, 0, 1, 1, 2, 2, 3, 3)))

// 1D Torus
class TestConfig11 extends Config(
  new WithUniformVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new UnidirectionalTorus1DConfig(2, Seq(0), Seq(1)))
class TestConfig12 extends Config(
  new WithUniformVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new UnidirectionalTorus1DConfig(4, Seq(0, 2), Seq(1, 3)))
class TestConfig13 extends Config(
  new WithUniformVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new UnidirectionalTorus1DConfig(10, Seq(0, 2, 4, 6, 8), Seq(1, 3, 5, 7, 9)))
class TestConfig14 extends Config(
  new WithUniformVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new UnidirectionalTorus1DConfig(10, 0 until 10, 0 until 10))

class TestConfig15 extends Config(
  new WithUniformVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new BidirectionalTorus1DConfig(2, Seq(0, 1), Seq(0, 1)))
class TestConfig16 extends Config(
  new WithUniformVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new BidirectionalTorus1DConfig(4, Seq(0, 2), Seq(1, 3)))
class TestConfig17 extends Config(
  new WithUniformVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new BidirectionalTorus1DConfig(10, 0 until 10, 0 until 10))

class TestConfig18 extends Config(
  new WithUniformVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new BidirectionalTorus1DConfig(10, 0 until 10, 0 until 10, randomRoute = true))
class TestConfig19 extends Config(
  new WithUniformVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new BidirectionalTorus1DConfig(10, Seq.tabulate(20)(_ % 10), Seq.tabulate(20)(_ % 10), randomRoute = true))

// Butterfly
class TestConfig20 extends Config(
  new WithUniformVirtualChannels(1, UserVirtualChannelParams(5)) ++
  new ButterflyConfig(2, 2))
class TestConfig21 extends Config(
  new WithUniformVirtualChannels(1, UserVirtualChannelParams(5)) ++
  new ButterflyConfig(2, 3))
class TestConfig22 extends Config(
  new WithUniformVirtualChannels(1, UserVirtualChannelParams(5)) ++
  new ButterflyConfig(2, 4))
class TestConfig23 extends Config(
  new WithUniformVirtualChannels(1, UserVirtualChannelParams(5)) ++
  new ButterflyConfig(3, 2))
class TestConfig24 extends Config(
  new WithUniformVirtualChannels(1, UserVirtualChannelParams(5)) ++
  new ButterflyConfig(3, 3))

// 2D Mesh
class TestConfig25 extends Config(
  new WithUniformVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new Mesh2DConfig(3, 3))
class TestConfig26 extends Config(
  new WithUniformVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new Mesh2DConfig(10, 10))
class TestConfig27 extends Config(
  new WithUniformVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new Mesh2DConfig(5, 5))
class TestConfig28 extends Config(
  new WithUniformVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new Mesh2DConfig(5, 5, RoutingRelations.mesh2DAlternatingDimensionOrdered))
class TestConfig29 extends Config(
  new WithUniformVirtualChannels(4, UserVirtualChannelParams(5)) ++
  new Mesh2DConfig(5, 5, RoutingRelations.mesh2DEscapeRouter))

class TestConfig30 extends Config(
  new WithUniformVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new Mesh2DConfig(3, 3, RoutingRelations.mesh2DEscapeRouter))
class TestConfig31 extends Config(
  new WithCombineRCVA ++
  new WithUniformVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new Mesh2DConfig(3, 3, RoutingRelations.mesh2DEscapeRouter))
class TestConfig32 extends Config(
  new WithCombineSAST ++
  new WithUniformVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new Mesh2DConfig(3, 3, RoutingRelations.mesh2DEscapeRouter))
class TestConfig33 extends Config(
  new WithCombineSAST ++
  new WithCombineRCVA ++
  new WithUniformVirtualChannels(2, UserVirtualChannelParams(2)) ++
  new Mesh2DConfig(3, 3, RoutingRelations.mesh2DEscapeRouter))


class TestConfig34 extends Config(
  new WithUniformVirtualChannels(1, UserVirtualChannelParams(1)) ++
  new Mesh2DConfig(5, 5))
class TestConfig35 extends Config(
  new WithUniformVirtualChannels(1, UserVirtualChannelParams(1)) ++
  new Mesh2DConfig(5, 5, RoutingRelations.mesh2DWestFirst))
class TestConfig36 extends Config(
  new WithUniformVirtualChannels(1, UserVirtualChannelParams(1)) ++
  new Mesh2DConfig(5, 5, RoutingRelations.mesh2DNorthLast))

class TestConfig37 extends Config(
  new constellation.WithIngressVNets((i: Int) => i % 4) ++
  new constellation.WithNBlockingVirtualNetworks(4) ++
  new constellation.WithUniformVirtualChannels(4, UserVirtualChannelParams(3)) ++
  new constellation.Mesh2DConfig(3, 3, RoutingRelations.mesh2DEscapeRouter))

class TestConfig38 extends Config(
  new constellation.WithIngressVNets((i: Int) => i % 4) ++
  new constellation.WithNBlockingVirtualNetworks(4) ++
  new constellation.WithUniformVirtualChannels(4, UserVirtualChannelParams(3)) ++
  new constellation.Mesh2DConfig(3, 3, RoutingRelations.mesh2DAlternatingDimensionOrdered))

class TestConfig39 extends Config(
  new constellation.WithIngressVNets((i: Int) => i % 4) ++
  new constellation.WithNNonblockingVirtualNetworks(4) ++
  new constellation.WithUniformVirtualChannels(4, UserVirtualChannelParams(3)) ++
  new constellation.Mesh2DConfig(3, 3, RoutingRelations.mesh2DEscapeRouter))


// 2D Torus
class TestConfig40 extends Config(
  new WithUniformVirtualChannels(2, UserVirtualChannelParams(1)) ++
  new UnidirectionalTorus2DConfig(3, 3))
class TestConfig41 extends Config(
  new WithUniformVirtualChannels(3, UserVirtualChannelParams(3)) ++
  new UnidirectionalTorus2DConfig(3, 3))
class TestConfig42 extends Config(
  new WithUniformVirtualChannels(4, UserVirtualChannelParams(4)) ++
  new UnidirectionalTorus2DConfig(5, 5))

class TestConfig43 extends Config(
  new WithUniformVirtualChannels(2, UserVirtualChannelParams(1)) ++
  new BidirectionalTorus2DConfig(3, 3))

class TLTestConfig00 extends Config(
  new WithTLNoCTesterParams(TLNoCTesterParams(Seq(0), Seq(1))) ++
  new WithNNonblockingVirtualNetworks(5) ++
  new WithUniformVirtualChannels(1, UserVirtualChannelParams(1)) ++
  new BidirectionalLineConfig(2))

class TLTestConfig01 extends Config(
  new WithTLNoCTesterParams(TLNoCTesterParams(Seq(4, 0, 2, 5, 6, 9, 11), Seq(7, 1, 3, 8, 10))) ++
  new WithNNonblockingVirtualNetworksWithSharing(5, 2) ++
  new WithUniformVirtualChannels(2, UserVirtualChannelParams(3)) ++
  new Mesh2DConfig(4, 3, RoutingRelations.mesh2DEscapeRouter))

class TLTestConfig02 extends Config(
  new WithTLNoCTesterParams(TLNoCTesterParams(Seq(4, 0, 2, 5, 6, 9, 11), Seq(7, 1, 3, 8, 10))) ++
  new WithNBlockingVirtualNetworks(5) ++
  new WithUniformVirtualChannels(6, UserVirtualChannelParams(3)) ++
  new Mesh2DConfig(4, 3, RoutingRelations.mesh2DEscapeRouter))

