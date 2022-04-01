package constellation.channel

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
  def render(e: ChannelEdgeParams) = if (e.cp.possiblePackets(e.p).size == 0) {
    RenderedEdge(colour = "ffffff", label = "X")
  } else {
    RenderedEdge(colour = "#0000ff", label = e.cp.payloadBits.toString)
  }

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

case class TerminalChannelEdgeParams(cp: TerminalChannelParams, p: Parameters)

object TerminalChannelImp extends SimpleNodeImp[TerminalChannelParams, TerminalChannelParams, TerminalChannelEdgeParams, TerminalChannel] {
  def edge(pd: TerminalChannelParams, pu: TerminalChannelParams, p: Parameters, sourceInfo: SourceInfo) = {
    require (pd == pu)
    TerminalChannelEdgeParams(pd, p)
  }
  def bundle(e: TerminalChannelEdgeParams) = new TerminalChannel(e.cp)(e.p)
  def render(e: TerminalChannelEdgeParams) = if (e.cp.possiblePackets(e.p).size == 0) {
    RenderedEdge(colour = "ffffff", label = "X")
  } else {
    RenderedEdge(colour = e.cp match {
      case e: IngressChannelParams => "#00ff00"
      case e: EgressChannelParams => "#ff0000"
    },
      label = e.cp.payloadBits.toString
    )
  }
}

case class TerminalChannelSourceNode(val sourceParams: TerminalChannelParams)(implicit valName: ValName) extends SourceNode(TerminalChannelImp)(Seq(sourceParams))
case class TerminalChannelDestNode(val destParams: TerminalChannelParams)(implicit valName: ValName) extends SinkNode(TerminalChannelImp)(Seq(destParams))
case class IngressChannelAdapterNode(
  masterFn: IngressChannelParams => IngressChannelParams = { s => s },
  slaveFn:  IngressChannelParams => IngressChannelParams = { d => d })(
  implicit valName: ValName) extends AdapterNode(TerminalChannelImp)(
  (m: TerminalChannelParams) => masterFn(m.asInstanceOf[IngressChannelParams]),
  (s: TerminalChannelParams) => slaveFn (s.asInstanceOf[IngressChannelParams]))
case class EgressChannelAdapterNode(
  masterFn: EgressChannelParams => EgressChannelParams = { s => s },
  slaveFn:  EgressChannelParams => EgressChannelParams = { d => d })(
  implicit valName: ValName) extends AdapterNode(TerminalChannelImp)(
  (m: TerminalChannelParams) => masterFn(m.asInstanceOf[EgressChannelParams]),
  (s: TerminalChannelParams) => slaveFn (s.asInstanceOf[EgressChannelParams]))
case class TerminalChannelIdentityNode()(implicit valName: ValName) extends IdentityNode(TerminalChannelImp)(
)
