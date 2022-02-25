package constellation.channel

import freechips.rocketchip.config.{Field, Parameters, Config}
import constellation.{NoCKey}

class WithUniformChannels(f: UserChannelParams => UserChannelParams) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(channelParamGen = (src: Int, dst: Int) => {
    f(up(NoCKey, site).channelParamGen(src, dst))
  })
})

class WithUniformVirtualChannels(f: Seq[UserVirtualChannelParams] => Seq[UserVirtualChannelParams]) extends WithUniformChannels(p => {
  p.copy(virtualChannelParams = f(p.virtualChannelParams))
})

class WithUniformUniformVirtualChannels(f: UserVirtualChannelParams => UserVirtualChannelParams)
  extends WithUniformVirtualChannels(v => v.map(vc => f(vc)))

class WithUniformNVirtualChannels(n: Int, p: UserVirtualChannelParams) extends WithUniformVirtualChannels(_ => {
  Seq.fill(n) { p }
})

class WithUniformChannelDepth(depth: Int) extends WithUniformChannels(p => {
  p.copy(channelGen = (u: Parameters) => {
    implicit val p: Parameters = u
    ChannelBuffer(depth) := _
  })
})

class WithUniformVirtualChannelBufferSize(size: Int) extends WithUniformUniformVirtualChannels(v => {
  v.copy(bufferSize = size)
})

class WithUniformChannelSrcMultiplier(mult: Int) extends WithUniformChannels(p => {
  p.copy(srcMultiplier = mult)
})

class WithUniformChannelDestMultiplier(mult: Int) extends WithUniformChannels(p => {
  p.copy(destMultiplier = mult)
})


class WithIngressVNets(f: Int => Int) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(ingresses = up(NoCKey, site).ingresses.zipWithIndex.map { case (u,i) =>
    u.copy(vNetId = f(i))
  })
})

class WithIngressPayloadBits(width: Int) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(ingresses = up(NoCKey, site).ingresses.map(_.copy(payloadBits = width)))
})

class WithEgressPayloadBits(width: Int) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(egresses = up(NoCKey, site).egresses.map(_.copy(payloadBits = width)))
})

class WithFullyConnectedIngresses extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(ingresses = up(NoCKey, site).ingresses.map(i => i.copy(possibleEgresses =
    i.possibleEgresses ++ (0 until up(NoCKey, site).egresses.size).toSet)))
})

class WithIngresses(ingresses: Seq[Int]) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(ingresses = up(NoCKey, site).ingresses ++
      ingresses.map(i => UserIngressParams(i)))
})

class WithEgresses(egresses: Seq[Int]) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(egresses = up(NoCKey, site).egresses ++ egresses.map(i => UserEgressParams(i)))
})
