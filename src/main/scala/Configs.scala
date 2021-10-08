package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters, Config}

class LineConfig(nNodes: Int = 3) extends Config((site, here, up) => {
  case AstroNoCKey => up(AstroNoCKey, site).copy(
    nNodes = nNodes,
    topology = (a: Int, b: Int) => if ((b-a).abs == 1) Seq.fill(2) { VirtualChannelParams(bufferSize=3) } else Nil,
    virtualLegalPaths = (n: Int) => (a: Int, b: Int, c: Int, d: Int) => (p: Int) => true,
    routingFunctions = (n: Int) => (dst: Int, nxt: Int) => {
      if (n < nxt) dst >= nxt else dst <= nxt
    }
  )
})

class TestConfig extends LineConfig(3)
