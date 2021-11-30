package constellation

import scala.math.pow
import scala.language.implicitConversions

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters, Config}

import constellation.topology._

object TopologyConverter {
  implicit def apply(topo: PhysicalTopology): ((Int, Int) => Option[ChannelParams]) = {
    (src: Int, dst: Int) => if (topo(src, dst)) Some(ChannelParams(src, dst)) else None
  }
}
import TopologyConverter._

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
  case NoCKey => up(NoCKey, site).copy(topology = (src: Int, dst: Int) =>
    up(NoCKey, site).topology(src, dst).map(_.copy(depth = depth))
  )
})

class WithUniformVirtualChannelBufferSize(size: Int) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(topology = (src: Int, dst: Int) =>
    up(NoCKey, site).topology(src, dst).map(u => u.copy(
      virtualChannelParams = u.virtualChannelParams.map(_.copy(bufferSize = size))
    ))
  )
})

class WithNNonblockingVirtualNetworks(n: Int) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    masterAllocTable = MasterAllocTables.nonblockingVirtualSubnetworks(
      up(NoCKey, site).masterAllocTable, n),
    topology = (src: Int, dst: Int) => up(NoCKey, site).topology(src, dst).map(u => u.copy(
      virtualChannelParams = u.virtualChannelParams.map(c => Seq.fill(n) { c }).flatten
    )),
    nVirtualNetworks = n
  )
})

class WithNBlockingVirtualNetworks(n: Int) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    masterAllocTable = MasterAllocTables.blockingVirtualSubnetworks(
      up(NoCKey, site).masterAllocTable, n),
    nVirtualNetworks = n,
    vNetBlocking = (blocker: Int, blockee: Int) => blocker < blockee
  )
})

class WithNNonblockingVirtualNetworksWithSharing(n: Int, nSharedChannels: Int = 1) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    masterAllocTable = MasterAllocTables.sharedNonblockingVirtualSubnetworks(up(NoCKey, site).masterAllocTable, n, nSharedChannels),
    topology = (src: Int, dst: Int) => up(NoCKey, site).topology(src, dst).map(u => u.copy(
      virtualChannelParams = Seq.fill(n) { u.virtualChannelParams(0) } ++ u.virtualChannelParams,
    )),
    nVirtualNetworks = n
  )
})


class WithUniformVirtualChannels(n: Int, v: VirtualChannelParams) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(topology = (src: Int, dst: Int) =>
    up(NoCKey, site).topology(src, dst).map(_.copy(
      virtualChannelParams = Seq.fill(n) { v }
    ))
  )
})

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
    nNodes = nNodes,
    topology = Topologies.unidirectionalLine,
    ingresses = ingressNodes.map(i => IngressChannelParams(i, (0 until egressNodes.size).toSet)),
    egresses = egressNodes.map(i => EgressChannelParams(i))
  )
})

class BidirectionalLineConfig(
  nNodes: Int = 2,
  ingressNodes: Seq[Int] = Seq(0, 1),
  egressNodes: Seq[Int] = Seq(0, 1),
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    nNodes = nNodes,
    topology = Topologies.bidirectionalLine,
    masterAllocTable = MasterAllocTables.bidirectionalLine,
    ingresses = ingressNodes.map(i => IngressChannelParams(i, (0 until egressNodes.size).toSet)),
    egresses = egressNodes.map(i => EgressChannelParams(i))
  )
})

class UnidirectionalTorus1DConfig(
  nNodes: Int = 2,
  ingressNodes: Seq[Int] = Seq(0),
  egressNodes: Seq[Int] = Seq(1),
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    nNodes = nNodes,
    topology = Topologies.unidirectionalTorus1D(nNodes),
    masterAllocTable = MasterAllocTables.unidirectionalTorus1DDateline(nNodes),
    ingresses = ingressNodes.map(i => IngressChannelParams(i, (0 until egressNodes.size).toSet)),
    egresses = egressNodes.map(i => EgressChannelParams(i))
  )
})

class BidirectionalTorus1DConfig(
  nNodes: Int = 2,
  ingressNodes: Seq[Int] = Seq(0),
  egressNodes: Seq[Int] = Seq(1),
  randomRoute: Boolean = false
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    nNodes = nNodes,
    topology = Topologies.bidirectionalTorus1D(nNodes),
    masterAllocTable = if (randomRoute) {
      MasterAllocTables.bidirectionalTorus1DRandom(nNodes)
    } else {
      MasterAllocTables.bidirectionalTorus1DShortest(nNodes)
    },
    ingresses = ingressNodes.map(i => IngressChannelParams(i, (0 until egressNodes.size).toSet)),
    egresses = egressNodes.map(i => EgressChannelParams(i))
  )
})

class ButterflyConfig(
  kAry: Int = 2,
  nFly: Int = 2
) extends Config((site, here, up) => {
  case NoCKey => {
    val height = pow(kAry,nFly-1).toInt
    up(NoCKey, site).copy(
      nNodes = height * nFly,
      topology = Topologies.butterfly(kAry, nFly),
      masterAllocTable = MasterAllocTables.butterfly(kAry, nFly),
      ingresses = ((0 until height) ++ (0 until height)).map(i => IngressChannelParams(i, (0 until 2*height).toSet)),
      egresses = ((0 until height) ++ (0 until height)).map(i => EgressChannelParams(i + height*(nFly-1)))
    )
  }
})

class Mesh2DConfig(
  nX: Int = 3,
  nY: Int = 3,
  masterAllocTable: (Int, Int) => MasterAllocTable = MasterAllocTables.mesh2DDimensionOrdered()
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    nNodes = nX * nY,
    topology = Topologies.mesh2D(nX, nY),
    masterAllocTable = masterAllocTable(nX, nY),
    ingresses = (0 until nX * nY).map(i => IngressChannelParams(i, (0 until nX * nY).toSet)),
    egresses = (0 until nX * nY).map(i => EgressChannelParams(i))
  )
})

class UnidirectionalTorus2DConfig(
  nX: Int = 3,
  nY: Int = 3,
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    nNodes = nX * nY,
    topology = Topologies.unidirectionalTorus2D(nX, nY),
    masterAllocTable = MasterAllocTables.dimensionOrderedUnidirectionalTorus2DDateline(nX, nY),
    ingresses = (0 until nX * nY).map(i => IngressChannelParams(i, (0 until nX * nY).toSet)),
    egresses = (0 until nX * nY).map(i => EgressChannelParams(i))
  )
})


class BidirectionalTorus2DConfig(
  nX: Int = 3,
  nY: Int = 3,
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    nNodes = nX * nY,
    topology = Topologies.bidirectionalTorus2D(nX, nY),
    masterAllocTable = MasterAllocTables.dimensionOrderedBidirectionalTorus2DDateline(nX, nY),
    ingresses = (0 until nX * nY).map(i => IngressChannelParams(i, (0 until nX * nY).toSet)),
    egresses = (0 until nX * nY).map(i => EgressChannelParams(i))
  )
})



// 1D mesh. Shared bus
class TestConfig00 extends Config(
  new WithUniformVirtualChannels(3, VirtualChannelParams(3)) ++
  new UnidirectionalLineConfig(2, Seq(0), Seq(1)))
class TestConfig01 extends Config(
  new WithUniformVirtualChannels(3, VirtualChannelParams(3)) ++
  new UnidirectionalLineConfig(2, Seq(0), Seq(1, 1)))
class TestConfig02 extends Config(
  new WithUniformVirtualChannels(3, VirtualChannelParams(3)) ++
  new UnidirectionalLineConfig(2, Seq(0, 0), Seq(1, 1)))
class TestConfig03 extends Config(
  new WithUniformVirtualChannels(3, VirtualChannelParams(3)) ++
  new UnidirectionalLineConfig(2, Seq(0, 0), Seq(0, 1, 1)))
class TestConfig04 extends Config(
  new WithUniformVirtualChannels(3, VirtualChannelParams(3)) ++
  new UnidirectionalLineConfig(3, Seq(0, 0), Seq(0, 1, 1, 2, 2)))
class TestConfig05 extends Config(
  new WithUniformVirtualChannels(3, VirtualChannelParams(3)) ++
  new UnidirectionalLineConfig(3, Seq(0, 1, 1), Seq(1, 1, 2)))
class TestConfig06 extends Config(
  new WithUniformVirtualChannels(4, VirtualChannelParams(5)) ++
  new UnidirectionalLineConfig(2, Seq(0), Seq(1)))

class TestConfig07 extends Config(
  new WithUniformVirtualChannels(3, VirtualChannelParams(3)) ++
  new BidirectionalLineConfig(2, Seq(0, 1), Seq(0, 1)))
class TestConfig08 extends Config(
  new WithUniformVirtualChannels(3, VirtualChannelParams(3)) ++
  new BidirectionalLineConfig(3, Seq(0, 1), Seq(0, 1, 2)))
class TestConfig09 extends Config(
  new WithUniformVirtualChannels(3, VirtualChannelParams(3)) ++
  new BidirectionalLineConfig(4, Seq(1, 2), Seq(0, 1, 2, 3)))
class TestConfig10 extends Config(
  new WithUniformVirtualChannels(3, VirtualChannelParams(3)) ++
  new BidirectionalLineConfig(4, Seq(1, 1, 2, 2), Seq(0, 0, 1, 1, 2, 2, 3, 3)))

// 1D Torus
class TestConfig11 extends Config(
  new WithUniformVirtualChannels(4, VirtualChannelParams(5)) ++
  new UnidirectionalTorus1DConfig(2, Seq(0), Seq(1)))
class TestConfig12 extends Config(
  new WithUniformVirtualChannels(4, VirtualChannelParams(5)) ++
  new UnidirectionalTorus1DConfig(4, Seq(0, 2), Seq(1, 3)))
class TestConfig13 extends Config(
  new WithUniformVirtualChannels(4, VirtualChannelParams(5)) ++
  new UnidirectionalTorus1DConfig(10, Seq(0, 2, 4, 6, 8), Seq(1, 3, 5, 7, 9)))
class TestConfig14 extends Config(
  new WithUniformVirtualChannels(4, VirtualChannelParams(5)) ++
  new UnidirectionalTorus1DConfig(10, 0 until 10, 0 until 10))

class TestConfig15 extends Config(
  new WithUniformVirtualChannels(4, VirtualChannelParams(5)) ++
  new BidirectionalTorus1DConfig(2, Seq(0, 1), Seq(0, 1)))
class TestConfig16 extends Config(
  new WithUniformVirtualChannels(4, VirtualChannelParams(5)) ++
  new BidirectionalTorus1DConfig(4, Seq(0, 2), Seq(1, 3)))
class TestConfig17 extends Config(
  new WithUniformVirtualChannels(4, VirtualChannelParams(5)) ++
  new BidirectionalTorus1DConfig(10, 0 until 10, 0 until 10))

class TestConfig18 extends Config(
  new WithUniformVirtualChannels(4, VirtualChannelParams(5)) ++
  new BidirectionalTorus1DConfig(10, 0 until 10, 0 until 10, randomRoute = true))
class TestConfig19 extends Config(
  new WithUniformVirtualChannels(4, VirtualChannelParams(5)) ++
  new BidirectionalTorus1DConfig(10, Seq.tabulate(20)(_ % 10), Seq.tabulate(20)(_ % 10), randomRoute = true))

// Butterfly
class TestConfig20 extends Config(
  new WithUniformVirtualChannels(1, VirtualChannelParams(5)) ++
  new ButterflyConfig(2, 2))
class TestConfig21 extends Config(
  new WithUniformVirtualChannels(1, VirtualChannelParams(5)) ++
  new ButterflyConfig(2, 3))
class TestConfig22 extends Config(
  new WithUniformVirtualChannels(1, VirtualChannelParams(5)) ++
  new ButterflyConfig(2, 4))
class TestConfig23 extends Config(
  new WithUniformVirtualChannels(1, VirtualChannelParams(5)) ++
  new ButterflyConfig(3, 2))
class TestConfig24 extends Config(
  new WithUniformVirtualChannels(1, VirtualChannelParams(5)) ++
  new ButterflyConfig(3, 3))

// 2D Mesh
class TestConfig25 extends Config(
  new WithUniformVirtualChannels(4, VirtualChannelParams(5)) ++
  new Mesh2DConfig(3, 3))
class TestConfig26 extends Config(
  new WithUniformVirtualChannels(4, VirtualChannelParams(5)) ++
  new Mesh2DConfig(10, 10))
class TestConfig27 extends Config(
  new WithUniformVirtualChannels(4, VirtualChannelParams(5)) ++
  new Mesh2DConfig(5, 5))
class TestConfig28 extends Config(
  new WithUniformVirtualChannels(4, VirtualChannelParams(5)) ++
  new Mesh2DConfig(5, 5, MasterAllocTables.mesh2DAlternatingDimensionOrdered))
class TestConfig29 extends Config(
  new WithUniformVirtualChannels(4, VirtualChannelParams(5)) ++
  new Mesh2DConfig(5, 5, MasterAllocTables.mesh2DDimensionOrderedHighest))

class TestConfig30 extends Config(
  new WithUniformVirtualChannels(2, VirtualChannelParams(2)) ++
  new Mesh2DConfig(3, 3, MasterAllocTables.mesh2DDimensionOrderedHighest))
class TestConfig31 extends Config(
  new WithCombineRCVA ++
  new WithUniformVirtualChannels(2, VirtualChannelParams(2)) ++
  new Mesh2DConfig(3, 3, MasterAllocTables.mesh2DDimensionOrderedHighest))
class TestConfig32 extends Config(
  new WithCombineSAST ++
  new WithUniformVirtualChannels(2, VirtualChannelParams(2)) ++
  new Mesh2DConfig(3, 3, MasterAllocTables.mesh2DDimensionOrderedHighest))
class TestConfig33 extends Config(
  new WithCombineSAST ++
  new WithCombineRCVA ++
  new WithUniformVirtualChannels(2, VirtualChannelParams(2)) ++
  new Mesh2DConfig(3, 3, MasterAllocTables.mesh2DDimensionOrderedHighest))


class TestConfig34 extends Config(
  new WithUniformVirtualChannels(1, VirtualChannelParams(1)) ++
  new Mesh2DConfig(5, 5))
class TestConfig35 extends Config(
  new WithUniformVirtualChannels(1, VirtualChannelParams(1)) ++
  new Mesh2DConfig(5, 5, MasterAllocTables.mesh2DWestFirst))
class TestConfig36 extends Config(
  new WithUniformVirtualChannels(1, VirtualChannelParams(1)) ++
  new Mesh2DConfig(5, 5, MasterAllocTables.mesh2DNorthLast))

class TestConfig37 extends Config(
  new constellation.WithIngressVNets((i: Int) => i % 4) ++
  new constellation.WithNBlockingVirtualNetworks(4) ++
  new constellation.WithUniformVirtualChannels(4, VirtualChannelParams(3)) ++
  new constellation.Mesh2DConfig(3, 3, MasterAllocTables.mesh2DDimensionOrderedHighest))

class TestConfig38 extends Config(
  new constellation.WithIngressVNets((i: Int) => i % 4) ++
  new constellation.WithNBlockingVirtualNetworks(4) ++
  new constellation.WithUniformVirtualChannels(4, VirtualChannelParams(3)) ++
  new constellation.Mesh2DConfig(3, 3, MasterAllocTables.mesh2DAlternatingDimensionOrdered))

class TestConfig39 extends Config(
  new constellation.WithIngressVNets((i: Int) => i % 4) ++
  new constellation.WithNNonblockingVirtualNetworks(4) ++
  new constellation.WithUniformVirtualChannels(4, VirtualChannelParams(3)) ++
  new constellation.Mesh2DConfig(3, 3, MasterAllocTables.mesh2DDimensionOrderedHighest))

class TestConfig40 extends Config(
  new constellation.WithIngressVNets((i: Int) => i % 4) ++
  new constellation.WithNNonblockingVirtualNetworksWithSharing(4) ++
  new constellation.WithUniformVirtualChannels(4, VirtualChannelParams(3)) ++
  new constellation.Mesh2DConfig(3, 3, MasterAllocTables.mesh2DDimensionOrderedHighest))


// 2D Torus
class TestConfig41 extends Config(
  new WithUniformVirtualChannels(2, VirtualChannelParams(1)) ++
  new UnidirectionalTorus2DConfig(3, 3))
class TestConfig42 extends Config(
  new WithUniformVirtualChannels(3, VirtualChannelParams(3)) ++
  new UnidirectionalTorus2DConfig(3, 3))
class TestConfig43 extends Config(
  new WithUniformVirtualChannels(4, VirtualChannelParams(4)) ++
  new UnidirectionalTorus2DConfig(5, 5))

class TestConfig44 extends Config(
  new WithUniformVirtualChannels(2, VirtualChannelParams(1)) ++
  new BidirectionalTorus2DConfig(3, 3))

class TLTestConfig00 extends Config(
  new WithTLNoCTesterParams(TLNoCTesterParams(Seq(4, 0, 2, 5, 6, 9, 11), Seq(7, 1, 3, 8, 10))) ++
  new WithNNonblockingVirtualNetworksWithSharing(5, 2) ++
  new WithUniformVirtualChannels(2, VirtualChannelParams(3)) ++
  new Mesh2DConfig(4, 3, MasterAllocTables.mesh2DAlternatingDimensionOrdered))

class TLTestConfig01 extends Config(
  new WithTLNoCTesterParams(TLNoCTesterParams(Seq(4, 0, 2, 5, 6, 9, 11), Seq(7, 1, 3, 8, 10))) ++
  new WithNBlockingVirtualNetworks(5) ++
  new WithUniformVirtualChannels(6, VirtualChannelParams(3)) ++
  new Mesh2DConfig(4, 3, MasterAllocTables.mesh2DAlternatingDimensionOrdered))
