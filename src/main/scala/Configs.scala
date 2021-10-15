package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters, Config}

class UnidirectionalLineConfig(nNodes: Int = 3, inputNodes: Seq[Int] = Seq(0), outputNodes: Seq[Int] = Seq(1, 2)) extends Config((site, here, up) => {
  case AstroNoCKey => up(AstroNoCKey, site).copy(
    nNodes = nNodes,
    nPrios = 1,
    topology = (a: Int, b: Int) => if ((b-a) == 1) Seq.tabulate(3) { i => VirtualChannelParams(bufferSize=i+3) } else Nil,
    virtualLegalPaths = {
      (n: Int) => (src: Int, srcV: Int, dst: Int, dstV: Int) => (prio: Int) => {
        true
      }
    },
    routingFunctions = (n: Int) => (dst: Int, nxt: Int) => (prio: Int) => {
      if (n < nxt) dst >= nxt else dst <= nxt
    },
    inputNodes = inputNodes.map { i => (1, i) },
    outputNodes = outputNodes
  )
})

class TestConfig00 extends UnidirectionalLineConfig(2, Seq(0), Seq(1))
class TestConfig01 extends UnidirectionalLineConfig(3, Seq(0), Seq(1, 2))
class TestConfig02 extends UnidirectionalLineConfig(3, Seq(0, 0), Seq(1, 2))
