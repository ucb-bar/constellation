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
  def render(e: ChannelEdgeParams) = if (e.cp.possibleFlows.size == 0) {
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
case class ChannelEphemeralNode()(implicit valName: ValName) extends EphemeralNode(ChannelImp)()

case class IngressChannelEdgeParams(cp: IngressChannelParams, p: Parameters)
case class EgressChannelEdgeParams(cp: EgressChannelParams, p: Parameters)

object IngressChannelImp extends SimpleNodeImp[IngressChannelParams, IngressChannelParams, IngressChannelEdgeParams, IngressChannel] {
  def edge(pd: IngressChannelParams, pu: IngressChannelParams, p: Parameters, sourceInfo: SourceInfo) = {
    require (pd == pu)
    IngressChannelEdgeParams(pd, p)
  }
  def bundle(e: IngressChannelEdgeParams) = new IngressChannel(e.cp)(e.p)
  def render(e: IngressChannelEdgeParams) = if (e.cp.possibleFlows.size == 0) {
    RenderedEdge(colour = "ffffff", label = "X")
  } else {
    RenderedEdge(colour = "#00ff00", label = e.cp.payloadBits.toString)
  }
}

object EgressChannelImp extends SimpleNodeImp[EgressChannelParams, EgressChannelParams, EgressChannelEdgeParams, EgressChannel] {
  def edge(pd: EgressChannelParams, pu: EgressChannelParams, p: Parameters, sourceInfo: SourceInfo) = {
    require (pd == pu)
    EgressChannelEdgeParams(pd, p)
  }
  def bundle(e: EgressChannelEdgeParams) = new EgressChannel(e.cp)(e.p)
  def render(e: EgressChannelEdgeParams) = if (e.cp.possibleFlows.size == 0) {
    RenderedEdge(colour = "ffffff", label = "X")
  } else {
    RenderedEdge(colour = "#ff0000", label = e.cp.payloadBits.toString)
  }
}

case class IngressChannelSourceNode(val sourceParams: IngressChannelParams)(implicit valName: ValName) extends SourceNode(IngressChannelImp)(Seq(sourceParams))
case class IngressChannelDestNode(val destParams: IngressChannelParams)(implicit valName: ValName) extends SinkNode(IngressChannelImp)(Seq(destParams))
case class EgressChannelSourceNode(val sourceParams: EgressChannelParams)(implicit valName: ValName) extends SourceNode(EgressChannelImp)(Seq(sourceParams))
case class EgressChannelDestNode(val destParams: EgressChannelParams)(implicit valName: ValName) extends SinkNode(EgressChannelImp)(Seq(destParams))


case class IngressChannelAdapterNode(
  masterFn: IngressChannelParams => IngressChannelParams = { s => s },
  slaveFn:  IngressChannelParams => IngressChannelParams = { d => d })(
  implicit valName: ValName) extends AdapterNode(IngressChannelImp)(
  (m: IngressChannelParams) => masterFn(m),
  (s: IngressChannelParams) => slaveFn (s))

case class EgressChannelAdapterNode(
  masterFn: EgressChannelParams => EgressChannelParams = { s => s },
  slaveFn:  EgressChannelParams => EgressChannelParams = { d => d })(
  implicit valName: ValName) extends AdapterNode(EgressChannelImp)(
  (m: EgressChannelParams) => masterFn(m),
  (s: EgressChannelParams) => slaveFn (s))

case class IngressChannelIdentityNode()(implicit valName: ValName) extends IdentityNode(IngressChannelImp)()
case class EgressChannelIdentityNode()(implicit valName: ValName) extends IdentityNode(EgressChannelImp)()

case class IngressChannelEphemeralNode()(implicit valName: ValName) extends EphemeralNode(IngressChannelImp)()
case class EgressChannelEphemeralNode()(implicit valName: ValName) extends EphemeralNode(EgressChannelImp)()
