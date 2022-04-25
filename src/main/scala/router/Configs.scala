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


class WithVCAllocator(vc: VCAllocatorParams => Parameters => VCAllocator) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(routerParams = (i: Int) =>
    up(NoCKey, site).routerParams(i).copy(vcAllocator = vc)
  )
})

class WithPIMMultiVCAllocator extends WithVCAllocator(vP => p => new PIMMultiVCAllocator(vP)(p))
class WithISLIPMultiVCAllocator extends WithVCAllocator(vP => p => new ISLIPMultiVCAllocator(vP)(p))
class WithRotatingSingleVCAllocator extends WithVCAllocator(vP => p => new RotatingSingleVCAllocator(vP)(p))

class WithPayloadBits(w: Int, routers: Seq[Int]) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(routerParams = (i: Int) =>
    if (routers.contains(i) || routers.size == 0) {
      up(NoCKey, site).routerParams(i).copy(payloadBits=w)
    } else {
      up(NoCKey, site).routerParams(i)
    }
  )
})

class WithUniformPayloadBits(w: Int) extends WithPayloadBits(w, Nil)
