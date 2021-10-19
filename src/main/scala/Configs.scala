package constellation

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters, Config}

class UnidirectionalLineConfig(
  nNodes: Int = 2,
  inputNodes: Seq[Int] = Seq(0),
  outputNodes: Seq[Int] = Seq(1),
  channelDepth: Int = 0,
  bufferSize: Int = 3,
  nVirtualChannels: Int = 3
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    nNodes = nNodes,
    nPrios = 1,
    topology = (a: Int, b: Int) => {
      if ((b-a) == 1) Seq.fill(nVirtualChannels) { VirtualChannelParams(bufferSize=bufferSize) } else Nil
    },
    channelDepths = (a: Int, b: Int) => channelDepth,
    virtualLegalPaths = {
      (n: Int) => (src: Int, srcV: Int, dst: Int, dstV: Int) => (prio: Int) => {
        true
      }
    },
    routingFunctions = (n: Int) => (src: Int, dst: Int, nxt: Int) => (prio: Int) => {
      true
    },
    inputNodes = inputNodes,
    outputNodes = outputNodes
  )
})

class BidirectionalLineConfig(
  nNodes: Int = 2,
  inputNodes: Seq[Int] = Seq(0, 1),
  outputNodes: Seq[Int] = Seq(0, 1),
  channelDepth: Int = 1
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    nNodes = nNodes,
    nPrios = 1,
    topology = (a: Int, b: Int) => {
      if ((b-a).abs == 1) Seq.fill(3) { VirtualChannelParams(bufferSize=3) } else Nil
    },
    channelDepths = (a: Int, b: Int) => channelDepth,
    virtualLegalPaths = {
      (n: Int) => (src: Int, srcV: Int, dst: Int, dstV: Int) => (prio: Int) => {
        dst != src
      }
    },
    routingFunctions = (n: Int) => (src: Int, dst: Int, nxt: Int) => (prio: Int) => {
      if (n < nxt) dst >= nxt else dst <= nxt
    },
    inputNodes = inputNodes,
    outputNodes = outputNodes
  )
})

class UnidirectionalRingConfig(
  nNodes: Int = 2,
  inputNodes: Seq[Int] = Seq(0),
  outputNodes: Seq[Int] = Seq(1),
  channelDepth: Int = 1,
  nVirtualChannels: Int = 5
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    nNodes = nNodes,
    nPrios = 1,
    topology = (a: Int, b: Int) => {
      if ((b-a) == 1 || (b == 0 && a == (nNodes-1))) Seq.fill(nVirtualChannels) { VirtualChannelParams(bufferSize=5) } else Nil
    },
    channelDepths = (a: Int, b: Int) => channelDepth,
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
    routingFunctions = (n: Int) => (src: Int, dst: Int, nxt: Int) => (prio: Int) => {
      true
    },
    inputNodes = inputNodes,
    outputNodes = outputNodes
  )
})

class BidirectionalRingConfig(
  nNodes: Int = 2,
  inputNodes: Seq[Int] = Seq(0),
  outputNodes: Seq[Int] = Seq(1),
  channelDepth: Int = 1,
  nVirtualChannels: Int = 5,
  randomRouting: Boolean = false
) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(
    nNodes = nNodes,
    nPrios = 1,
    topology = (a: Int, b: Int) => {
      if ((b + nNodes - a) % nNodes == 1 || (a + nNodes - b) % nNodes == 1)
        Seq.fill(nVirtualChannels) { VirtualChannelParams(bufferSize=5) } else Nil
    },
    channelDepths = (a: Int, b: Int) => channelDepth,
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
    routingFunctions = (n: Int) => (src: Int, dst: Int, nxt: Int) => (prio: Int) => {
      if (randomRouting) {
        if (src == -1) {
          true
        } else if ((n + nNodes - src) % nNodes == 1) {
          (nxt + nNodes - n) % nNodes == 1
        } else {
          (n + nNodes - nxt) % nNodes == 1
        }
      } else {
        val cwDist = (dst + nNodes - n) % nNodes
        val ccwDist = (n + nNodes - dst) % nNodes
        if (cwDist < ccwDist) {
          (nxt + nNodes - n) % nNodes == 1
        } else if (cwDist > ccwDist) {
          (n + nNodes - nxt) % nNodes == 1
        } else {
          true
        }
      }
    },
    inputNodes = inputNodes,
    outputNodes = outputNodes
  )
})

class TestConfig00 extends UnidirectionalLineConfig(2, Seq(0), Seq(1))
class TestConfig01 extends UnidirectionalLineConfig(2, Seq(0), Seq(1, 1))
class TestConfig02 extends UnidirectionalLineConfig(2, Seq(0, 0), Seq(1, 1))
class TestConfig03 extends UnidirectionalLineConfig(2, Seq(0, 0), Seq(0, 1, 1))
class TestConfig04 extends UnidirectionalLineConfig(3, Seq(0, 0), Seq(0, 1, 1, 2, 2))
class TestConfig05 extends UnidirectionalLineConfig(3, Seq(0, 1, 1), Seq(1, 1, 2))

class TestConfig06 extends UnidirectionalLineConfig(2, Seq(0), Seq(1), 1, 5, 4)

class TestConfig07 extends BidirectionalLineConfig(2, Seq(0, 1), Seq(0, 1))
class TestConfig08 extends BidirectionalLineConfig(3, Seq(0, 1), Seq(0, 1, 2))
class TestConfig09 extends BidirectionalLineConfig(4, Seq(1, 2), Seq(0, 1, 2, 3))
class TestConfig10 extends BidirectionalLineConfig(4, Seq(1, 1, 2, 2), Seq(0, 0, 1, 1, 2, 2, 3, 3))

class TestConfig11 extends UnidirectionalRingConfig(2, Seq(0), Seq(1))
class TestConfig12 extends UnidirectionalRingConfig(4, Seq(0, 2), Seq(1, 3))
class TestConfig13 extends UnidirectionalRingConfig(10, Seq(0, 2, 4, 6, 8), Seq(1, 3, 5, 7, 9))
class TestConfig14 extends UnidirectionalRingConfig(10, 0 until 10, 0 until 10)

class TestConfig15 extends BidirectionalRingConfig(2, Seq(0, 1), Seq(0, 1))
class TestConfig16 extends BidirectionalRingConfig(4, Seq(0, 2), Seq(1, 3))
class TestConfig17 extends BidirectionalRingConfig(10, 0 until 10, 0 until 10)

class TestConfig18 extends BidirectionalRingConfig(10, 0 until 10, 0 until 10, randomRouting=true)
class TestConfig19 extends BidirectionalRingConfig(10, (0 until 10) ++ (0 until 10), (0 until 10) ++ (0 until 10), randomRouting=true)
