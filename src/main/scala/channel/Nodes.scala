package constellation.channel

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.diplomacy._

case class EmptyParams()

case class ChannelEdgeParams(cp: ChannelParams, p: Parameters)

object ChannelImp extends SimpleNodeImp[EmptyParams, ChannelParams, ChannelEdgeParams, Channel] {
  def edge(pd: EmptyParams, pu: ChannelParams, p: Parameters, sourceInfo: SourceInfo) = {
    ChannelEdgeParams(pu, p)
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

case class ChannelSourceNode(val destId: Int)(implicit valName: ValName) extends SourceNode(ChannelImp)(Seq(EmptyParams()))
case class ChannelDestNode(val destParams: ChannelParams)(implicit valName: ValName) extends SinkNode(ChannelImp)(Seq(destParams))
case class ChannelAdapterNode(
  slaveFn:  ChannelParams => ChannelParams = { d => d })(
  implicit valName: ValName) extends AdapterNode(ChannelImp)((e: EmptyParams) => e, slaveFn)
case class ChannelIdentityNode()(implicit valName: ValName) extends IdentityNode(ChannelImp)()
case class ChannelEphemeralNode()(implicit valName: ValName) extends EphemeralNode(ChannelImp)()

case class IngressChannelEdgeParams(cp: IngressChannelParams, p: Parameters)
case class EgressChannelEdgeParams(cp: EgressChannelParams, p: Parameters)

object IngressChannelImp extends SimpleNodeImp[EmptyParams, IngressChannelParams, IngressChannelEdgeParams, IngressChannel] {
  def edge(pd: EmptyParams, pu: IngressChannelParams, p: Parameters, sourceInfo: SourceInfo) = {
    IngressChannelEdgeParams(pu, p)
  }
  def bundle(e: IngressChannelEdgeParams) = new IngressChannel(e.cp)(e.p)
  def render(e: IngressChannelEdgeParams) = if (e.cp.possibleFlows.size == 0) {
    RenderedEdge(colour = "ffffff", label = "X")
  } else {
    RenderedEdge(colour = "#00ff00", label = e.cp.payloadBits.toString)
  }
}

object EgressChannelImp extends SimpleNodeImp[EmptyParams, EgressChannelParams, EgressChannelEdgeParams, EgressChannel] {
  def edge(pd: EmptyParams, pu: EgressChannelParams, p: Parameters, sourceInfo: SourceInfo) = {
    EgressChannelEdgeParams(pu, p)
  }
  def bundle(e: EgressChannelEdgeParams) = new EgressChannel(e.cp)(e.p)
  def render(e: EgressChannelEdgeParams) = if (e.cp.possibleFlows.size == 0) {
    RenderedEdge(colour = "ffffff", label = "X")
  } else {
    RenderedEdge(colour = "#ff0000", label = e.cp.payloadBits.toString)
  }
}

case class IngressChannelSourceNode(val destId: Int)(implicit valName: ValName) extends SourceNode(IngressChannelImp)(Seq(EmptyParams()))
case class IngressChannelDestNode(val destParams: IngressChannelParams)(implicit valName: ValName) extends SinkNode(IngressChannelImp)(Seq(destParams))
case class EgressChannelSourceNode(val egressId: Int)(implicit valName: ValName) extends SourceNode(EgressChannelImp)(Seq(EmptyParams()))
case class EgressChannelDestNode(val destParams: EgressChannelParams)(implicit valName: ValName) extends SinkNode(EgressChannelImp)(Seq(destParams))


case class IngressChannelAdapterNode(
  slaveFn:  IngressChannelParams => IngressChannelParams = { d => d })(
  implicit valName: ValName) extends AdapterNode(IngressChannelImp)(m => m, slaveFn)

case class EgressChannelAdapterNode(
  slaveFn:  EgressChannelParams => EgressChannelParams = { d => d })(
  implicit valName: ValName) extends AdapterNode(EgressChannelImp)(m => m, slaveFn)

case class IngressChannelIdentityNode()(implicit valName: ValName) extends IdentityNode(IngressChannelImp)()
case class EgressChannelIdentityNode()(implicit valName: ValName) extends IdentityNode(EgressChannelImp)()

case class IngressChannelEphemeralNode()(implicit valName: ValName) extends EphemeralNode(IngressChannelImp)()
case class EgressChannelEphemeralNode()(implicit valName: ValName) extends EphemeralNode(EgressChannelImp)()
