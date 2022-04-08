package constellation.router

import freechips.rocketchip.config.{Field, Parameters, Config}
import constellation.noc.{NoCKey}

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

class WithCoupleSAVA extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(routerParams = (i: Int) =>
    up(NoCKey, site).routerParams(i).copy(coupleSAVA = true)
  )
})

// Only couple SAVA on routers with minimal SA/VA complexity
class WithSafeCoupleSAVA extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(routerParams = (i: Int) => {
    val topo = up(NoCKey).topology
    val nInputs = (0 until topo.nNodes).filter(_ != i).map { src => topo.topo(src, i) }.size
    if (nInputs == 1)
      up(NoCKey, site).routerParams(i).copy(coupleSAVA = true)
    else
      up(NoCKey, site).routerParams(i)
  })
})

class WithEarlyRC extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(routerParams = (i: Int) =>
    up(NoCKey, site).routerParams(i).copy(earlyRC = true)
  )
})


class WithIterativeVCAllocator extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(routerParams = (i: Int) =>
    up(NoCKey, site).routerParams(i).copy(vcAllocator = (vP) => (p) => new IterativeVCAllocator(vP)(p))
  )
})

class WithSimpleVCAllocator extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(routerParams = (i: Int) =>
    up(NoCKey, site).routerParams(i).copy(vcAllocator = (vP) => (p) => new SimpleVCAllocator(vP)(p))
  )
})

class WithUniformPayloadBits(w: Int) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(routerParams = (i: Int) =>
    up(NoCKey, site).routerParams(i).copy(payloadBits=w)
  )
})
