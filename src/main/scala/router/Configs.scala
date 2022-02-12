package constellation.router

import freechips.rocketchip.config.{Field, Parameters, Config}
import constellation.{NoCKey}

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
