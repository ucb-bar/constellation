package constellation

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.diplomacy._

case class ChannelEdgeParams(cp: ChannelParams, p: Parameters)

object ChannelImp extends SimpleNodeImp[ChannelParams, ChannelParams, ChannelEdgeParams, Channel] {
  def edge(pd: ChannelParams, pu: ChannelParams, p: Parameters, sourceInfo: SourceInfo) = {
    require (pd == pu)
    ChannelEdgeParams(pd, p)
  }
  def bundle(e: ChannelEdgeParams) = new Channel(e.cp)(e.p)
  def render(e: ChannelEdgeParams) = RenderedEdge(colour = "#fb2c00", label = e.cp.payloadBits.toString)

  override def monitor(bundle: Channel, edge: ChannelEdgeParams): Unit = {
    val monitor = Module(new NoCMonitor(edge.cp)(edge.p))
    monitor.io.in := bundle
  }
  // TODO: Add nodepath stuff? override def mixO, override def mixI
}

case class ChannelSourceNode(val sourceParams: ChannelParams)(implicit valName: ValName) extends SourceNode(ChannelImp)(Seq(sourceParams))
case class ChannelDestNode(val destParams: ChannelParams)(implicit valName: ValName) extends SinkNode(ChannelImp)(Seq(destParams))
case class ChannelAdapterNode(
  masterFn: ChannelParams => ChannelParams = { s => s },
  slaveFn:  ChannelParams => ChannelParams = { d => d })(
  implicit valName: ValName) extends AdapterNode(ChannelImp)(masterFn, slaveFn)
case class ChannelIdentityNode()(implicit valName: ValName) extends IdentityNode(ChannelImp)()
