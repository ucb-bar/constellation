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
    up(NoCKey, site).topology(src, dst).map(_.copy(
      virtualChannelParams =
        up(NoCKey, site).topology(src, dst).get.virtualChannelParams.map(_.copy(bufferSize = size))
    ))
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
    routingFunctions = RoutingAlgorithms.bidirectionalLine,
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
    virtualLegalPaths = {
      (n: Int) => (src: Int, srcV: Int, dst: Int, dstV: Int) => (prio: Int) => {
        if (src == -1)  {
          dstV != 0
        } else if (srcV == 0) {
          dstV == 0
        } else if (n == nNodes - 1) {
          dstV < srcV
        } else {
          dstV <= srcV && dstV != 0
        }
      }
    },
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
    virtualLegalPaths = {
      (n: Int) => (src: Int, srcV: Int, dst: Int, dstV: Int) => (prio: Int) => {
        if (src == -1)  {
          dstV != 0
        } else if (srcV == 0) {
          dstV == 0
        } else if ((dst + nNodes - n) % nNodes == 1) {
          if (n == nNodes - 1) {
            dstV < srcV
          } else {
            dstV <= srcV && dstV != 0
          }
        } else if ((n + nNodes - dst) % nNodes == 1) {
          if (n == 0) {
            dstV < srcV
          } else {
            dstV <= srcV && dstV != 0
          }
        } else {
          false
        }
      }
    },
    routingFunctions = if (randomRoute) {
      RoutingAlgorithms.bidirectionalTorus1DRandom(nNodes)
    } else {
      RoutingAlgorithms.bidirectionalTorus1DShortest(nNodes)
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
      routingFunctions = RoutingAlgorithms.butterfly(kAry, nFly),
      inputNodes = (0 until height) ++ (0 until height),
      outputNodes = ((0 until height) ++ (0 until height)).map(_ + height*(nFly-1))
    )
  }
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
  new BidirectionalTorus1DConfig(10, 0 until 10, 0 until 10, randomRoute=true))
class TestConfig19 extends Config(
  new WithUniformVirtualChannels(4, VirtualChannelParams(5)) ++
  new BidirectionalTorus1DConfig(10, (0 until 10) ++ (0 until 10), (0 until 10) ++ (0 until 10), randomRoute=true))

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

