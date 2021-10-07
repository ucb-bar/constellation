package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters, Config}

class LineConfig(nNodes: Int = 3) extends Config((site, here, up) => {
  case AstroNoCKey => up(AstroNoCKey, site).copy(
    nNodes = nNodes,
    topology = (a: Int, b: Int) => if ((b-a).abs == 1) Some(ChannelParams(
      virtualChannelParams=Seq.fill(2) { VirtualChannelParams(bufferSize=3) }
    )) else None,
    virtualLegalPaths = (n: Int) => (a: Int, b: Int, c: Int, d: Int) => (p: UInt) => true.B
  )
})

class TestConfig extends LineConfig(3)
