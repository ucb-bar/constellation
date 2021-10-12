package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters, Config}

class LineConfig(nNodes: Int = 3, inputNodes: Seq[Int] = Seq(0), outputNodes: Seq[Int] = Seq(1, 2)) extends Config((site, here, up) => {
  case AstroNoCKey => up(AstroNoCKey, site).copy(
    nNodes = nNodes,
    topology = (a: Int, b: Int) => if ((b-a) == 1) Seq.fill(2) { VirtualChannelParams(bufferSize=3) } else Nil,
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

class TestConfig extends LineConfig(3)
