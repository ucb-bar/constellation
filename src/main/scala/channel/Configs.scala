package constellation.channel

import freechips.rocketchip.config.{Field, Parameters, Config}
import constellation.noc.{NoCKey}
import constellation.topology.BidirectionalTree

import scala.math.{floor, log10, pow, max}

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


/* Sets the multiplier of branches to leaf nodes to MULT and doubles the multiplier at each increasing level. */
class WithFatTreeChannels(mult: Int) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(channelParamGen = (src: Int, dst: Int) => {
    val p = up(NoCKey, site).channelParamGen(src, dst)
      val height = up(NoCKey, site).topology.asInstanceOf[BidirectionalTree].height
      val dAry = up(NoCKey, site).topology.asInstanceOf[BidirectionalTree].dAry
      def level(id: Int) = floor(log10(id + 1) / log10(dAry))
      val multiplier = pow(2, height - max(level(src), level(dst))).toInt
      p.copy(srcMultiplier = multiplier,
             destMultiplier = multiplier)
  })
})

class WithIngressVNets(f: Int => Int) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(ingresses = up(NoCKey, site).ingresses.zipWithIndex.map { case (u,i) =>
    u.copy(vNetId = f(i))
  })
})

class WithEgressVNets(f: Int => Int) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(egresses = up(NoCKey, site).egresses.zipWithIndex.map { case (u,i) =>
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
  case NoCKey => up(NoCKey, site).copy(
    flows = up(NoCKey, site).ingresses.zipWithIndex.map { case (ingress, i) => {
      up(NoCKey, site).egresses
        .zipWithIndex
        .filter { case (egress,e) => egress.vNetId == ingress.vNetId }
        .map(_._2)
        .map(e => FlowParams(i, e, ingress.vNetId))
    }}.flatten
  )
})

class WithIngresses(ingresses: Seq[Int]) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(ingresses = up(NoCKey, site).ingresses ++
      ingresses.map(i => UserIngressParams(i)))
})

class WithEgresses(egresses: Seq[Int]) extends Config((site, here, up) => {
  case NoCKey => up(NoCKey, site).copy(egresses = up(NoCKey, site).egresses ++ egresses.map(i => UserEgressParams(i)))
})

