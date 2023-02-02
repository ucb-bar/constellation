package constellation.noc

import chisel3._
import chisel3.util._


import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, BundleBridgeSink, InModuleBody}
import freechips.rocketchip.util.ElaborationArtefacts
import freechips.rocketchip.prci._
import constellation.router._
import constellation.channel._
import constellation.routing.{RoutingRelation, ChannelRoutingInfo}
import constellation.topology.{PhysicalTopology, UnidirectionalLine}


class NoCTerminalIO(
  val ingressParams: Seq[IngressChannelParams],
  val egressParams: Seq[EgressChannelParams])(implicit val p: Parameters) extends Bundle {
  val ingress = MixedVec(ingressParams.map { u => Flipped(new IngressChannel(u)) })
  val egress = MixedVec(egressParams.map { u => new EgressChannel(u) })
}

class NoC(nocParams: NoCParams)(implicit p: Parameters) extends LazyModule {
  val internalParams = InternalNoCParams(nocParams)
  val allChannelParams = internalParams.channelParams
  val allIngressParams = internalParams.ingressParams
  val allEgressParams = internalParams.egressParams
  val allRouterParams = internalParams.routerParams

  val iP = p.alterPartial({ case InternalNoCKey => internalParams })
  val nNodes = nocParams.topology.nNodes
  val nocName = nocParams.nocName
  val skipValidationChecks = nocParams.skipValidationChecks

  val clockSourceNodes = Seq.tabulate(nNodes) { i => ClockSourceNode(Seq(ClockSourceParameters())) }
  val router_sink_domains = Seq.tabulate(nNodes) { i =>
    val router_sink_domain = LazyModule(new ClockSinkDomain(ClockSinkParameters(
      name = Some(s"${nocName}_router_$i")
    )))
    router_sink_domain.clockNode := clockSourceNodes(i)
    router_sink_domain
  }

  val routers = Seq.tabulate(nNodes) { i => router_sink_domains(i) {
    val inParams = allChannelParams.filter(_.destId == i).map(
      _.copy(payloadBits=allRouterParams(i).user.payloadBits)
    )
    val outParams = allChannelParams.filter(_.srcId == i).map(
      _.copy(payloadBits=allRouterParams(i).user.payloadBits)
    )
    val ingressParams = allIngressParams.filter(_.destId == i).map(
      _.copy(payloadBits=allRouterParams(i).user.payloadBits)
    )
    val egressParams = allEgressParams.filter(_.srcId == i).map(
      _.copy(payloadBits=allRouterParams(i).user.payloadBits)
    )
    val noIn = inParams.size + ingressParams.size == 0
    val noOut = outParams.size + egressParams.size == 0
    if (noIn || noOut) {
      println(s"Constellation WARNING: $nocName router $i seems to be unused, it will not be generated")
      None
    } else {
      Some(LazyModule(new Router(
        routerParams = allRouterParams(i),
        preDiplomaticInParams = inParams,
        preDiplomaticIngressParams = ingressParams,
        outDests = outParams.map(_.destId),
        egressIds = egressParams.map(_.egressId)
      )(iP)))
    }
  }}.flatten

  val ingressNodes = allIngressParams.map { u => IngressChannelSourceNode(u.destId) }
  val egressNodes = allEgressParams.map { u => EgressChannelDestNode(u) }

  // Generate channels between routers diplomatically
  Seq.tabulate(nNodes, nNodes) { case (i, j) => if (i != j) {
    val routerI = routers.find(_.nodeId == i)
    val routerJ = routers.find(_.nodeId == j)
    if (routerI.isDefined && routerJ.isDefined) {
      val sourceNodes: Seq[ChannelSourceNode] = routerI.get.sourceNodes.filter(_.destId == j)
      val destNodes: Seq[ChannelDestNode] = routerJ.get.destNodes.filter(_.destParams.srcId == i)
      require (sourceNodes.size == destNodes.size)
      (sourceNodes zip destNodes).foreach { case (src, dst) =>
        val channelParam = allChannelParams.find(c => c.srcId == i && c.destId == j).get
        router_sink_domains(j) {
          implicit val p: Parameters = iP
          (dst
            := ChannelWidthWidget(routerJ.get.payloadBits, routerI.get.payloadBits)
            := channelParam.channelGen(p)(src)
          )
        }
      }
    }
  }}

  // Generate terminal channels diplomatically
  routers.foreach { dst => router_sink_domains(dst.nodeId) {
    implicit val p: Parameters = iP
    dst.ingressNodes.foreach(n => {
      val ingressId = n.destParams.ingressId
      require(dst.payloadBits <= allIngressParams(ingressId).payloadBits)
      (n
        := IngressWidthWidget(dst.payloadBits, allIngressParams(ingressId).payloadBits)
        := ingressNodes(ingressId)
      )
    })
    dst.egressNodes.foreach(n => {
      val egressId = n.egressId
      require(dst.payloadBits <= allEgressParams(egressId).payloadBits)
      (egressNodes(egressId)
        := EgressWidthWidget(allEgressParams(egressId).payloadBits, dst.payloadBits)
        := n
      )
    })
  }}

  val debugNodes = routers.map { r =>
    val sink = BundleBridgeSink[DebugBundle]()
    sink := r.debugNode
    sink
  }
  val ctrlNodes = if (nocParams.hasCtrl) {
    (0 until nNodes).map { i =>
      routers.find(_.nodeId == i).map { r =>
        val sink = BundleBridgeSink[RouterCtrlBundle]()
        sink := r.ctrlNode.get
        sink
      }
    }
  } else {
    Nil
  }

  println(s"Constellation: $nocName Finished parameter validation")
  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    println(s"Constellation: $nocName Starting NoC RTL generation")
    val io = IO(new NoCTerminalIO(allIngressParams, allEgressParams)(iP) {
      val router_clocks = Vec(nNodes, Input(new ClockBundle(ClockBundleParameters())))
      val router_ctrl = if (nocParams.hasCtrl) Vec(nNodes, new RouterCtrlBundle) else Nil
    })

    (io.ingress zip ingressNodes.map(_.out(0)._1)).foreach { case (l,r) => r <> l }
    (io.egress  zip egressNodes .map(_.in (0)._1)).foreach { case (l,r) => l <> r }
    (io.router_clocks zip clockSourceNodes.map(_.out(0)._1)).foreach { case (l,r) => l <> r }

    if (nocParams.hasCtrl) {
      ctrlNodes.zipWithIndex.map { case (c,i) =>
        if (c.isDefined) {
          io.router_ctrl(i) <> c.get.in(0)._1
        } else {
          io.router_ctrl(i) <> DontCare
        }
      }
    }

    // TODO: These assume a single clock-domain across the entire noc
    val debug_va_stall_ctr = RegInit(0.U(64.W))
    val debug_sa_stall_ctr = RegInit(0.U(64.W))
    val debug_any_stall_ctr = debug_va_stall_ctr + debug_sa_stall_ctr
    debug_va_stall_ctr := debug_va_stall_ctr + debugNodes.map(_.in(0)._1.va_stall.reduce(_+_)).reduce(_+_)
    debug_sa_stall_ctr := debug_sa_stall_ctr + debugNodes.map(_.in(0)._1.sa_stall.reduce(_+_)).reduce(_+_)

    dontTouch(debug_va_stall_ctr)
    dontTouch(debug_sa_stall_ctr)
    dontTouch(debug_any_stall_ctr)

    def prepend(s: String) = Seq(nocName, s).mkString(".")
    ElaborationArtefacts.add(prepend("noc.graphml"), graphML)

    val adjList = routers.map { r =>
      val outs = r.outParams.map(o => s"${o.destId}").mkString(" ")
      val egresses = r.egressParams.map(e => s"e${e.egressId}").mkString(" ")
      val ingresses = r.ingressParams.map(i => s"i${i.ingressId} ${r.nodeId}")
      (Seq(s"${r.nodeId} $outs $egresses") ++ ingresses).mkString("\n")
    }.mkString("\n")
    ElaborationArtefacts.add(prepend("noc.adjlist"), adjList)

    val xys = routers.map(r => {
      val n = r.nodeId
      val ids = (Seq(r.nodeId.toString)
        ++ r.egressParams.map(e => s"e${e.egressId}")
        ++ r.ingressParams.map(i => s"i${i.ingressId}")
      )
      val plotter = nocParams.topology.plotter
      val coords = (Seq(plotter.node(r.nodeId))
        ++ Seq.tabulate(r.egressParams.size ) { i => plotter. egress(i, r. egressParams.size, r.nodeId) }
        ++ Seq.tabulate(r.ingressParams.size) { i => plotter.ingress(i, r.ingressParams.size, r.nodeId) }
      )

      (ids zip coords).map { case (i, (x, y)) => s"$i $x $y" }.mkString("\n")
    }).mkString("\n")
    ElaborationArtefacts.add(prepend("noc.xy"), xys)

    val edgeProps = routers.map { r =>
      val outs = r.outParams.map { o =>
        (Seq(s"${r.nodeId} ${o.destId}") ++ (if (o.possibleFlows.size == 0) Some("unused") else None))
          .mkString(" ")
      }
      val egresses = r.egressParams.map { e =>
        (Seq(s"${r.nodeId} e${e.egressId}") ++ (if (e.possibleFlows.size == 0) Some("unused") else None))
          .mkString(" ")
      }
      val ingresses = r.ingressParams.map { i =>
        (Seq(s"i${i.ingressId} ${r.nodeId}") ++ (if (i.possibleFlows.size == 0) Some("unused") else None))
          .mkString(" ")
      }
      (outs ++ egresses ++ ingresses).mkString("\n")
    }.mkString("\n")
    ElaborationArtefacts.add(prepend("noc.edgeprops"), edgeProps)

    println(s"Constellation: $nocName Finished NoC RTL generation")
  }
}
