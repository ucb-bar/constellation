package constellation.channel

import freechips.rocketchip.config.{Field, Parameters, Config}
import constellation.{NoCKey}

class WithUniformChannelDepth(depth: Int) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(channelParamGen = (src: Int, dst: Int) => {
    up(NoCKey, site).channelParamGen(src, dst).copy(channel = (u: Parameters) => {
      implicit val p: Parameters = u
      ChannelBuffer(depth) := _
    })
  })
})

class WithUniformVirtualChannelBufferSize(size: Int) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(channelParamGen = (src: Int, dst: Int) => {
    val cp = up(NoCKey, site).channelParamGen(src, dst)
    cp.copy(virtualChannelParams = cp.virtualChannelParams.map(_.copy(bufferSize = size)))
  })
})

class WithVirtualChannels(v: Seq[UserVirtualChannelParams]) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(channelParamGen = (src: Int, dst: Int) =>
    up(NoCKey, site).channelParamGen(src, dst).copy(
      virtualChannelParams = v
    )
  )
})

class WithUniformVirtualChannels(n: Int, v: UserVirtualChannelParams) extends WithVirtualChannels(Seq.fill(n)(v))

class WithIngressVNets(f: Int => Int) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(ingresses = up(NoCKey, site).ingresses.zipWithIndex.map { case (u,i) =>
    u.copy(vNetId = f(i))
  })
})
