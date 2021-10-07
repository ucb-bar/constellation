package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters, Config}

class AstroNoCTestConfig extends Config((site, here, up) => {
  case AstroNoCKey => AstroNoCConfig()
})
