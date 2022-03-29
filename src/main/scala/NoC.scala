package constellation

import chisel3._
import chisel3.util._


import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, BundleBridgeSink, InModuleBody}
import freechips.rocketchip.util.ElaborationArtefacts
import freechips.rocketchip.prci._
import constellation.router._
import constellation.channel._
import constellation.routing.{RoutingRelation, PacketRoutingInfo, ChannelRoutingInfo}
import constellation.topology.{PhysicalTopology, UnidirectionalLine}


case class NoCConfig(
  nVirtualNetworks: Int = 1,

  topology: PhysicalTopology = new UnidirectionalLine(1),
  channelParamGen: (Int, Int) => UserChannelParams = (a: Int, b: Int) => UserChannelParams(),
  ingresses: Seq[UserIngressParams] = Nil,
  egresses: Seq[UserEgressParams] = Nil,
  routingRelation: RoutingRelation = RoutingRelation.allLegal,
  routerParams: Int => UserRouterParams = (i: Int) => UserRouterParams(),
  // (blocker, blockee) => bool
  // If true, then blocker must be able to proceed when blockee is blocked
  vNetBlocking: (Int, Int) => Boolean = (_: Int, _: Int) => true,
  nocName: String = "test",
  skipValidationChecks: Boolean = false
)
case object NoCKey extends Field[NoCConfig](NoCConfig())

trait HasNoCParams {
  implicit val p: Parameters
  private val params = p(NoCKey)

  val nNodes = params.topology.nNodes
  val nVirtualNetworks = params.nVirtualNetworks
  val nocName = params.nocName
  val skipValidationChecks = params.skipValidationChecks

  val nodeIdBits = log2Ceil(nNodes)
  val vNetBits = log2Up(params.nVirtualNetworks)
  val nEgresses = params.egresses.size
  val egressIdBits = log2Up(params.egresses.size)
  val egressSrcIds = params.egresses.map(_.srcId)
}

class NoCTerminalIO(
  val ingressParams: Seq[IngressChannelParams],
  val egressParams: Seq[EgressChannelParams])(implicit val p: Parameters) extends Bundle {
  val ingress = MixedVec(ingressParams.map { u => Flipped(new TerminalChannel(u)) })
  val egress = MixedVec(egressParams.map { u => new TerminalChannel(u) })
}

class NoC(implicit p: Parameters) extends LazyModule with HasNoCParams{
  var uniqueChannelId = 0
  def getUniqueChannelId(): Int = {
    val r = uniqueChannelId
    uniqueChannelId = uniqueChannelId + 1
    r
  }

  val fullChannelParams: Seq[ChannelParams] = Seq.tabulate(nNodes, nNodes) { case (i,j) =>
    if (p(NoCKey).topology.topo(i, j)) {
      val cP = p(NoCKey).channelParamGen(i, j)
      val payloadBits = p(NoCKey).routerParams(i).payloadBits
      require(p(NoCKey).routerParams(i).payloadBits == p(NoCKey).routerParams(j).payloadBits)
      Some(ChannelParams(
        srcId = i,
        destId = j,
        payloadBits = payloadBits,
        user = cP,
        uniqueId = getUniqueChannelId()
      ))
    } else {
      None
    }
  }.flatten.flatten

  val globalIngressParams = p(NoCKey).ingresses.zipWithIndex.map { case (u,i) =>
    IngressChannelParams(
      user = u,
      ingressId = i,
      uniqueId = getUniqueChannelId())
  }
  val globalEgressParams = p(NoCKey).egresses.zipWithIndex.map { case (u,e) =>
    EgressChannelParams(
      user = u,
      egressId = e,
      uniqueId = getUniqueChannelId(),
      possiblePackets = globalIngressParams.filter(_.possibleEgresses.contains(e)).map { i =>
        PacketRoutingInfo(e, i.vNetId)
      }.toSet
    )
  }

  globalIngressParams.foreach(_.possibleEgresses.foreach(e => require(e < globalEgressParams.size)))


  // Check sanity of routingRelation, all inputs can route to all outputs

  // Tracks the set of every possible packet that might occupy each virtual channel
  val possiblePacketMap = scala.collection.mutable.Map[ChannelRoutingInfo, Set[PacketRoutingInfo]]().withDefaultValue(Set())

  /** Uses ROUTINGREL to check that, for any ingress, all possible packets from said ingress are
   *  able to reach all intended egresses.
   *
   * @param vNetId virtual network id
   * @param routingRel the routing relation for the network
   */
  def checkConnectivity(vNetId: Int, routingRel: RoutingRelation) = {
    val nextChannelParamMap = (0 until nNodes).map { i => i -> fullChannelParams.filter(_.srcId == i) }.toMap
    val tempPossiblePacketMap = scala.collection.mutable.Map[ChannelRoutingInfo, Set[PacketRoutingInfo]]()
      .withDefaultValue(Set())

    // Loop through accessible ingress/egress pairs
    globalIngressParams.zipWithIndex.filter(_._1.vNetId == vNetId).map { case (iP,iIdx) =>
      val iId = iP.destId
      println(s"Constellation: $nocName Checking connectivity from ingress $iIdx")
      iP.possibleEgresses.toSeq.sorted.map { oIdx =>
        val oP = globalEgressParams(oIdx)
        val oId = oP.srcId
        val pInfo = PacketRoutingInfo(oIdx, vNetId)
        val flowPossiblePackets = scala.collection.mutable.Set[ChannelRoutingInfo]()

        var stack: Seq[ChannelRoutingInfo] = iP.channelRoutingInfos
        while (stack.size != 0) {
          val head = stack.head
          stack = stack.tail

          // This channel is the destination
          val atDest = head.dst == oId
          // In this call to checkConnectivity, a previously searched ingress point
          // generated a packet that could route to its destination through head
          // We don't have to search further along this path.
          val prevFoundRoutable = tempPossiblePacketMap(head).contains(pInfo)

          if (!atDest && !prevFoundRoutable) {
            // Find all possible VCs a packet in head can route to
            val nexts = nextChannelParamMap(head.dst).map { nxtC =>
              nxtC.channelRoutingInfos.map { cI =>
                val can_transition = routingRel(
                  head.dst,
                  head,
                  cI,
                  pInfo
                )
                // if (head.src == 25 && src.dst == 0)
                //   println((can_transition, head, cI, pInfo))
                if (can_transition) Some(cI) else None
              }.flatten
            }.flatten
            // If the packet can't route to any VCs, this is an error
            require(nexts.size > 0,
              s"Failed to route from $iId to $oId at $head for vnet $vNetId")
            val toAdd = nexts.filter(n => !flowPossiblePackets.contains(n))
            nexts.foreach(n => flowPossiblePackets += n)
            stack = toAdd ++ stack
          }
        }
        flowPossiblePackets.foreach { k => tempPossiblePacketMap(k) += pInfo }
      }
    }
    tempPossiblePacketMap.foreach { case (k,v) => possiblePacketMap(k) ++= v }
  }
  def checkAcyclic(vNetId: Int, routingRel: RoutingRelation): Option[Seq[ChannelRoutingInfo]] = {
    var visited = Set[ChannelRoutingInfo]()
    var stack = Seq[ChannelRoutingInfo]()

    def checkAcyclicUtil(cI: ChannelRoutingInfo): Boolean = {
      visited = visited + cI
      stack = stack ++ Seq(cI)

      val neighbors = fullChannelParams.filter(_.srcId == cI.dst).map(_.channelRoutingInfos).flatten.filter { nI =>
        possiblePacketMap(cI).filter(_.vNet == vNetId).map { pI =>
          routingRel(cI.dst, cI, nI, pI)
        }.fold(false)(_||_)
      }

      neighbors.foreach { nI =>
        if (!visited.contains(nI)) {
          if (!checkAcyclicUtil(nI)) return false
        } else if (stack.contains(nI)) {
          return false
        }
      }
      stack = stack.dropRight(1)
      return true
    }

    globalIngressParams.filter(_.vNetId == vNetId).zipWithIndex.map { case (iP,iIdx) =>
      if (!checkAcyclicUtil(iP.channelRoutingInfos(0))) {
        return Some(stack)
      }
    }
    return None
  }

  // Check connectivity, ignoring blocking properties of virtual subnets
  println(s"Constellation: $nocName Checking full connectivity")
  for (vNetId <- 0 until nVirtualNetworks) {
    checkConnectivity(vNetId, p(NoCKey).routingRelation)
  }

  // Connectivity for each virtual subnet
  if (skipValidationChecks) {
    println(s"Constellation WARNING: $nocName skip checking virtual subnet connectivity")
  } else {
    println(s"Constellation: $nocName Checking virtual subnet connectivity")
    for (vNetId <- 0 until nVirtualNetworks) {
      // blockees are vNets which the current vNet can block without affecting its own forwards progress
      val blockees = (0 until nVirtualNetworks).filter(v => v != vNetId && p(NoCKey).vNetBlocking(vNetId, v))
      val blockeeSets = blockees.toSet.subsets.filter(_.size > 0)
      // For each subset of blockers for this virtual network, recheck connectivity assuming
      // every virtual channel accessible to each blocker is locked
      for (b <- blockeeSets) {
        val routingRel = p(NoCKey).routingRelation
        checkConnectivity(vNetId, routingRel && !(new RoutingRelation((nodeId, srcC, nxtC, pInfo) => {
          b.map { v => possiblePacketMap(nxtC).map(_.vNet == v) }.flatten.fold(false)(_||_)
        })))
      }
    }
  }

  // Check for deadlock in escape channels
  println(s"Constellation: $nocName Checking for possibility of deadlock")
  for (vNetId <- 0 until nVirtualNetworks) {
    // blockees are vNets which the current vNet can block without affecting its own forwards progress
    val blockees = (0 until nVirtualNetworks).filter(v => v != vNetId && p(NoCKey).vNetBlocking(vNetId, v))
    val blockeeSets = blockees.toSet.subsets
    // For each subset of blockers for this virtual network, recheck connectivity assuming
    // every virtual channel accessible to each blocker is locked
    for (b <- blockeeSets) {
      val routingRel = p(NoCKey).routingRelation
      val acyclicPath = checkAcyclic(vNetId, routingRel
        && !(new RoutingRelation((nodeId, srcC, nxtC, pInfo) => {b.map { v => possiblePacketMap(nxtC).map(_.vNet == v) }.flatten.fold(false)(_||_)}))
        && new RoutingRelation((nodeId, srcC, nxtC, pInfo) => routingRel.isEscape(nxtC, vNetId))
      )
      acyclicPath.foreach { path =>
        println(s"Constellation WARNING: $nocName cyclic path on virtual network $vNetId may cause deadlock: ${acyclicPath.get}")
      }
    }
  }

  // Tie off inpossible virtual channels
  // Also set possible nodes for each channel
  val channelParams = fullChannelParams.map { cP => cP.copy(
    virtualChannelParams=cP.virtualChannelParams.zipWithIndex.map { case (vP,vId) =>
      val traversable = possiblePacketMap(cP.channelRoutingInfos(vId)).size != 0
      if (!traversable) {
        println(s"Constellation WARNING: $nocName virtual channel $vId from ${cP.srcId} to ${cP.destId} appears to be untraversable")
      }
      vP.copy(possiblePackets=possiblePacketMap(cP.channelRoutingInfos(vId)))
    }
  )}.map(cP => {
    if (!cP.traversable)
      println(s"Constellation WARNING: $nocName physical channel from ${cP.srcId} to ${cP.destId} appears to be untraversable.")
    cP
  }).filter(_.traversable)

  val clockSourceNodes = Seq.tabulate(nNodes) { i => ClockSourceNode(Seq(ClockSourceParameters())) }
  val router_sink_domains = Seq.tabulate(nNodes) { i =>
    val router_sink_domain = LazyModule(new ClockSinkDomain(ClockSinkParameters(
      name = Some(s"${nocName}_router_$i")
    )))
    router_sink_domain.clockNode := clockSourceNodes(i)
    router_sink_domain
  }

  val routers = Seq.tabulate(nNodes) { i => router_sink_domains(i) {
    val inParams = channelParams.filter(_.destId == i)
    val outParams = channelParams.filter(_.srcId == i)
    val ingressParams = globalIngressParams.filter(_.destId == i)
    val egressParams = globalEgressParams.filter(_.srcId == i)
    val noIn = inParams.size + ingressParams.size == 0
    val noOut = outParams.size + egressParams.size == 0
    if (noIn || noOut) {
      println(s"Constellation WARNING: $nocName router $i seems to be unused, it will not be generated")
      None
    } else {
      Some(LazyModule(new Router(
        routerParams = RouterParams(
          nodeId = i,
          user = p(NoCKey).routerParams(i)
        ),
        inParams = inParams,
        outParams = outParams,
        ingressParams = ingressParams,
        egressParams = egressParams
      )))
    }
  }}.flatten

  val ingressNodes = globalIngressParams.map { u => TerminalChannelSourceNode(u) }
  val egressNodes = globalEgressParams.map { u => TerminalChannelDestNode(u) }

  Seq.tabulate(nNodes, nNodes) { case (i, j) => if (i != j) {
    val routerI = routers.find(_.nodeId == i)
    val routerJ = routers.find(_.nodeId == j)
    if (routerI.isDefined && routerJ.isDefined) {
      val sourceNodes = routerI.get.sourceNodes.filter(_.sourceParams.destId == j)
      val destNodes = routerJ.get.destNodes.filter(_.destParams.srcId == i)
      require (sourceNodes.size == destNodes.size)
      val channelParam = p(NoCKey).channelParamGen(i, j)
      (sourceNodes zip destNodes).foreach { case (src, dst) =>
        router_sink_domains(j) { dst := channelParam.channelGen(p)(src) }
      }
    }
  }}

  routers.foreach { dst =>
    dst.ingressNodes.foreach(n =>
      n := ingressNodes(n.destParams.asInstanceOf[IngressChannelParams].ingressId)
    )
    dst.egressNodes.foreach(n =>
      egressNodes(n.sourceParams.asInstanceOf[EgressChannelParams].egressId) := n
    )
  }

  val debugNodes = routers.map { r =>
    val sink = BundleBridgeSink[DebugBundle]()
    sink := r.debugNode
    sink
  }

  println(s"Constellation: $nocName Finished parameter validation")
  lazy val module = new LazyModuleImp(this) {
    println(s"Constellation: $nocName Starting NoC RTL generation")
    val io = IO(new NoCTerminalIO(globalIngressParams, globalEgressParams)(p) {
      val router_clocks = Vec(nNodes, Input(new ClockBundle(ClockBundleParameters())))
    })

    (io.ingress zip ingressNodes.map(_.out(0)._1)).foreach { case (l,r) => r <> l }
    (io.egress  zip egressNodes .map(_.in (0)._1)).foreach { case (l,r) => l <> r }
    (io.router_clocks zip clockSourceNodes.map(_.out(0)._1)).foreach { case (l,r) => l <> r }

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
      val plotter = p(NoCKey).topology.plotter
      val coords = (Seq(plotter.node(r.nodeId))
        ++ Seq.tabulate(r.egressParams.size ) { i => plotter. egress(i, r. egressParams.size, r.nodeId) }
        ++ Seq.tabulate(r.ingressParams.size) { i => plotter.ingress(i, r.ingressParams.size, r.nodeId) }
      )

      (ids zip coords).map { case (i, (x, y)) => s"$i $x $y" }.mkString("\n")
    }).mkString("\n")
    ElaborationArtefacts.add(prepend("noc.xy"), xys)

    val edgeProps = routers.map { r =>
      val outs = r.outParams.map { o =>
        (Seq(s"${r.nodeId} ${o.destId}") ++ (if (o.possiblePackets.size == 0) Some("unused") else None))
          .mkString(" ")
      }
      val egresses = r.egressParams.map { e =>
        (Seq(s"${r.nodeId} e${e.egressId}") ++ (if (e.possiblePackets.size == 0) Some("unused") else None))
          .mkString(" ")
      }
      val ingresses = r.ingressParams.map { i =>
        (Seq(s"i${i.ingressId} ${r.nodeId}") ++ (if (i.possiblePackets.size == 0) Some("unused") else None))
          .mkString(" ")
      }
      (outs ++ egresses ++ ingresses).mkString("\n")
    }.mkString("\n")
    ElaborationArtefacts.add(prepend("noc.edgeprops"), edgeProps)
  }
}
