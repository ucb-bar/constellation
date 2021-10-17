package astronoc

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
  case AstroNoCKey => up(AstroNoCKey, site).copy(
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
    routingFunctions = (n: Int) => (dst: Int, nxt: Int) => (prio: Int) => {
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
  case AstroNoCKey => up(AstroNoCKey, site).copy(
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
    routingFunctions = (n: Int) => (dst: Int, nxt: Int) => (prio: Int) => {
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
  channelDepth: Int = 1
) extends Config((site, here, up) => {
  case AstroNoCKey => up(AstroNoCKey, site).copy(
    nNodes = nNodes,
    nPrios = 1,
    topology = (a: Int, b: Int) => {
      if ((b-a) == 1 || (b == 0 && a == (nNodes-1))) Seq.fill(4) { VirtualChannelParams(bufferSize=4) } else Nil
    },
    channelDepths = (a: Int, b: Int) => channelDepth,
    virtualLegalPaths = {
      (n: Int) => (src: Int, srcV: Int, dst: Int, dstV: Int) => (prio: Int) => {
        true
      }
    },
    routingFunctions = (n: Int) => (dst: Int, nxt: Int) => (prio: Int) => {
      true
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

class TestConfig06 extends UnidirectionalLineConfig(2, Seq(0), Seq(1), 1, 8, 8)

class TestConfig07 extends BidirectionalLineConfig(2, Seq(0, 1), Seq(0, 1))
class TestConfig08 extends BidirectionalLineConfig(3, Seq(0, 1), Seq(0, 1, 2))
class TestConfig09 extends BidirectionalLineConfig(4, Seq(1, 2), Seq(0, 1, 2, 3))
class TestConfig10 extends BidirectionalLineConfig(4, Seq(1, 1, 2, 2), Seq(0, 0, 1, 1, 2, 2, 3, 3))

class TestConfig11 extends UnidirectionalRingConfig(2, Seq(0), Seq(1))
