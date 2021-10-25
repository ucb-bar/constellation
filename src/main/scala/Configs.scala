package constellation

import chisel3._
import chisel3.util._
import scala.math.pow
import freechips.rocketchip.config.{Field, Parameters, Config}

import constellation.topology._

object TopologyConverter {
  implicit def apply(topo: PhysicalTopology): ((Int, Int) => Option[ChannelParams]) = {
    (src: Int, dst: Int) => if (topo(src, dst)) Some(ChannelParams(src, dst)) else None
  }
}

// Why does this not make the implicit def work everywhere
// Manually call TopologyConverter for now
import TopologyConverter._

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
    masterAllocTable = MasterAllocTables.nonblockingVirtualSubnetworks(up(NoCKey, site).masterAllocTable, n),
    topology = (src: Int, dst: Int) => up(NoCKey, site).topology(src, dst).map(u => u.copy(
      virtualChannelParams = u.virtualChannelParams.map(c => Seq.fill(n) { c }).flatten
    )),
    nVirtualNetworks = n
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

class UnidirectionalLineConfig(
  nNodes: Int = 2,
  inputNodes: Seq[Int] = Seq(0),
  outputNodes: Seq[Int] = Seq(1)
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    nNodes = nNodes,
    topology = TopologyConverter(Topologies.unidirectionalLine),
    inputNodes = inputNodes,
    outputNodes = outputNodes
  )
})

class BidirectionalLineConfig(
  nNodes: Int = 2,
  inputNodes: Seq[Int] = Seq(0, 1),
  outputNodes: Seq[Int] = Seq(0, 1),
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    nNodes = nNodes,
    topology = TopologyConverter(Topologies.bidirectionalLine),
    masterAllocTable = MasterAllocTables.bidirectionalLine,
    inputNodes = inputNodes,
    outputNodes = outputNodes
  )
})

class UnidirectionalTorus1DConfig(
  nNodes: Int = 2,
  inputNodes: Seq[Int] = Seq(0),
  outputNodes: Seq[Int] = Seq(1),
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    nNodes = nNodes,
    topology = TopologyConverter(Topologies.unidirectionalTorus1D(nNodes)),
    masterAllocTable = MasterAllocTables.unidirectionalTorus1DDateline(nNodes),
    inputNodes = inputNodes,
    outputNodes = outputNodes
  )
})

class BidirectionalTorus1DConfig(
  nNodes: Int = 2,
  inputNodes: Seq[Int] = Seq(0),
  outputNodes: Seq[Int] = Seq(1),
  randomRoute: Boolean = false
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    nNodes = nNodes,
    topology = TopologyConverter(Topologies.bidirectionalTorus1D(nNodes)),
    masterAllocTable = if (randomRoute) {
      MasterAllocTables.bidirectionalTorus1DRandom(nNodes)
    } else {
      MasterAllocTables.bidirectionalTorus1DShortest(nNodes)
    },
    inputNodes = inputNodes,
    outputNodes = outputNodes
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
      topology = TopologyConverter(Topologies.butterfly(kAry, nFly)),
      masterAllocTable = MasterAllocTables.butterfly(kAry, nFly),
      inputNodes = (0 until height) ++ (0 until height),
      outputNodes = ((0 until height) ++ (0 until height)).map(_ + height*(nFly-1))
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
    topology = TopologyConverter(Topologies.mesh2D(nX, nY)),
    masterAllocTable = masterAllocTable(nX, nY),
    inputNodes = (0 until nX * nY),
    outputNodes = (0 until nX * nY)
  )
})

class UnidirectionalTorus2DConfig(
  nX: Int = 3,
  nY: Int = 3,
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    nNodes = nX * nY,
    topology = TopologyConverter(Topologies.unidirectionalTorus2D(nX, nY)),
    masterAllocTable = MasterAllocTables.dimensionOrderedUnidirectionalTorus2DDateline(nX, nY),
    inputNodes = (0 until nX * nY),
    outputNodes = (0 until nX * nY)
  )
})


class BidirectionalTorus2DConfig(
  nX: Int = 3,
  nY: Int = 3,
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    nNodes = nX * nY,
    topology = TopologyConverter(Topologies.bidirectionalTorus2D(nX, nY)),
    masterAllocTable = MasterAllocTables.dimensionOrderedBidirectionalTorus2DDateline(nX, nY),
    inputNodes = (0 until nX * nY),
    outputNodes = (0 until nX * nY)
  )
})




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
  new Mesh2DConfig(3, 4, MasterAllocTables.mesh2DAlternatingDimensionOrdered))


class TestConfig30 extends Config(
  new WithUniformVirtualChannels(1, VirtualChannelParams(1)) ++
  new Mesh2DConfig(5, 5))
class TestConfig31 extends Config(
  new WithUniformVirtualChannels(1, VirtualChannelParams(1)) ++
  new Mesh2DConfig(5, 5, MasterAllocTables.mesh2DWestFirst))
class TestConfig32 extends Config(
  new WithUniformVirtualChannels(1, VirtualChannelParams(1)) ++
  new Mesh2DConfig(5, 5, MasterAllocTables.mesh2DNorthLast))


class TestConfig33 extends Config(
  new WithUniformVirtualChannels(2, VirtualChannelParams(1)) ++
  new UnidirectionalTorus2DConfig(3, 3))
class TestConfig34 extends Config(
  new WithUniformVirtualChannels(3, VirtualChannelParams(3)) ++
  new UnidirectionalTorus2DConfig(3, 3))
class TestConfig35 extends Config(
  new WithUniformVirtualChannels(4, VirtualChannelParams(4)) ++
  new UnidirectionalTorus2DConfig(5, 5))

class TestConfig36 extends Config(
  new WithUniformVirtualChannels(2, VirtualChannelParams(1)) ++
  new BidirectionalTorus2DConfig(3, 3))
