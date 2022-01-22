package constellation

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.util.ElaborationArtefacts
import constellation.router._
import constellation.routing._
import constellation.topology._

case class NoCConfig(
  nNodes: Int = 3,
  nVirtualNetworks: Int = 1,

  topology: PhysicalTopology = new UnidirectionalLine,
  channelParamGen: (Int, Int) => UserChannelParams = (a: Int, b: Int) => UserChannelParams(),
  ingresses: Seq[UserIngressParams] = Nil,
  egresses: Seq[UserEgressParams] = Nil,
  routingRelation: RoutingRelation = RoutingRelations.allLegal,
  routerParams: Int => UserRouterParams = (i: Int) => UserRouterParams(),
  // (blocker, blockee) => bool
  // If true, then blocker must be able to proceed when blockee is blocked
  vNetBlocking: (Int, Int) => Boolean = (_: Int, _: Int) => true,
  prefix: Option[String] = None
)
case object NoCKey extends Field[NoCConfig](NoCConfig())

trait HasNoCParams {
  implicit val p: Parameters
  private val params = p(NoCKey)

  val nNodes = params.nNodes
  val nVirtualNetworks = params.nVirtualNetworks

  val nodeIdBits = log2Ceil(params.nNodes)
  val vNetBits = log2Up(params.nVirtualNetworks)
  val nEgresses = params.egresses.size
  val egressIdBits = log2Up(params.egresses.size)
  val egressSrcIds = params.egresses.map(_.srcId)
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
        virtualChannelParams = cP.virtualChannelParams.zipWithIndex.map { case (vP, vc) =>
          VirtualChannelParams(i, j, vc, vP.bufferSize, Set[PacketInfo](), getUniqueChannelId())
        }
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
        PacketInfo(e, i.vNetId)
      }.toSet
    )
  }

  globalIngressParams.foreach(_.possibleEgresses.foreach(e => require(e < globalEgressParams.size)))


  // Check sanity of routingRelation, all inputs can route to all outputs

  // Tracks the set of every possible packet that might occupy each virtual channel
  val possiblePacketMap = scala.collection.mutable.Map[ChannelInfoForRouting, Set[PacketInfo]]().withDefaultValue(Set())

  def checkConnectivity(vNetId: Int, routingRel: RoutingRelation) = {
    // Loop through accessible ingress/egress pairs
    globalIngressParams.filter(_.vNetId == vNetId).zipWithIndex.map { case (iP,iIdx) =>
      val iId = iP.destId
      iP.possibleEgresses.map { oIdx =>
        val oP = globalEgressParams(oIdx)
        val oId = oP.srcId

        // Track the positions a packet performing ingress->egress might occupy
        var positions: Set[ChannelInfoForRouting] = iP.channelInfosForRouting.toSet
        while (positions.size != 0) {
          positions.foreach { pos => possiblePacketMap(pos) += (PacketInfo(oIdx, vNetId)) }
          // Determine next possible positions based on current possible positions
          // and connectivity function
          positions = positions.filter(_.dst != oId).map { cI =>
            val nexts = fullChannelParams.filter(_.srcId == cI.dst).map { nxtC =>
              (0 until nxtC.nVirtualChannels).map { nxtV =>
                val can_transition = routingRel(cI.dst)(
                  cI,
                  nxtC.virtualChannelParams(nxtV).asChannelInfoForRouting,
                  PacketInfoForRouting(oId, vNetId)
                )
                if (can_transition) Some(nxtC.virtualChannelParams(nxtV).asChannelInfoForRouting) else None
              }.flatten
            }.flatten
            require(nexts.size > 0,
              s"Failed to route from $iId to $oId at $cI")
            nexts
          }.flatten.toSet
        }
      }
    }
  }
  def checkAcyclic(vNetId: Int, routingRel: RoutingRelation): Option[Seq[ChannelInfoForRouting]] = {
    var visited = Set[ChannelInfoForRouting]()
    var stack = Seq[ChannelInfoForRouting]()

    def checkAcyclicUtil(cI: ChannelInfoForRouting): Boolean = {
      visited = visited + cI
      stack = stack ++ Seq(cI)

      val neighbors = fullChannelParams.filter(_.srcId == cI.dst).map(_.channelInfosForRouting).flatten.filter { nI =>
        possiblePacketMap(cI).filter(_.vNetId == vNetId).map { pI =>
          routingRel(cI.dst)(cI, nI, pI.asPacketInfoForRouting)
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
      if (!checkAcyclicUtil(iP.channelInfosForRouting(0))) {
        return Some(stack)
      }
    }
    return None
  }

  // Check connectivity, ignoring blocking properties of virtual subnets
  println("Constellation: Checking full connectivity")
  for (vNetId <- 0 until nVirtualNetworks) {
    checkConnectivity(vNetId, p(NoCKey).routingRelation)
  }

  // Connectivity for each virtual subnet
  println("Constellation: Checking virtual subnet connectivity")
  for (vNetId <- 0 until nVirtualNetworks) {
    // blockees are vNets which the current vNet can block without affecting its own forwards progress
    val blockees = (0 until nVirtualNetworks).filter(v => v != vNetId && p(NoCKey).vNetBlocking(vNetId, v))
    val blockeeSets = blockees.toSet.subsets.filter(_.size > 0)
    // For each subset of blockers for this virtual network, recheck connectivity assuming
    // every virtual channel accessible to each blocker is locked
    for (b <- blockeeSets) {
      val routingRel = p(NoCKey).routingRelation
      checkConnectivity(vNetId, routingRel && !(new RoutingRelation((nodeId, srcC, nxtC, pInfo) => {
        b.map { v => possiblePacketMap(nxtC).map(_.vNetId == v) }.flatten.fold(false)(_||_)
      })))
    }
  }

  // Check for deadlock in escape channels
  println("Constellation: Checking for possibility of deadlock")
  for (vNetId <- 0 until nVirtualNetworks) {
    // blockees are vNets which the current vNet can block without affecting its own forwards progress
    val blockees = (0 until nVirtualNetworks).filter(v => v != vNetId && p(NoCKey).vNetBlocking(vNetId, v))
    val blockeeSets = blockees.toSet.subsets
    // For each subset of blockers for this virtual network, recheck connectivity assuming
    // every virtual channel accessible to each blocker is locked
    for (b <- blockeeSets) {
      val routingRel = p(NoCKey).routingRelation
      val acyclicPath = checkAcyclic(vNetId, routingRel
        && !(new RoutingRelation((nodeId, srcC, nxtC, pInfo) => {b.map { v => possiblePacketMap(nxtC).map(_.vNetId == v) }.flatten.fold(false)(_||_)}))
        && new RoutingRelation((nodeId, srcC, nxtC, pInfo) => routingRel.isEscape(nxtC, vNetId))
      )
      acyclicPath.foreach { path =>
        println(s"Constellation WARNING: cyclic path on virtual network $vNetId may cause deadlock: ${acyclicPath.get}")
      }
    }
  }


  // Tie off inpossible virtual channels
  // Also set possible nodes for each channel
  val channelParams = fullChannelParams.map { cP => cP.copy(
    virtualChannelParams=cP.virtualChannelParams.zipWithIndex.map { case (vP,vId) =>
      val traversable = possiblePacketMap(vP.asChannelInfoForRouting).size != 0
      if (!traversable) {
        println(s"Constellation WARNING: virtual channel $vId from ${cP.srcId} to ${cP.destId} appears to be untraversable")
      }
      vP.copy(possiblePackets=possiblePacketMap(vP.asChannelInfoForRouting))
    }
  )}
  channelParams.map(cP => if (!cP.traversable)
    println(s"Constellation WARNING: physical channel from ${cP.srcId} to ${cP.destId} appears to be untraversable"))

  val routers = Seq.tabulate(nNodes) { i => LazyModule(new Router(
    routerParams = RouterParams(
      nodeId = i,
      nodeRoutingRelation = p(NoCKey).routingRelation(i),
      user = p(NoCKey).routerParams(i)
    ),
    inParams = channelParams.filter(_.destId == i),
    outParams = channelParams.filter(_.srcId == i),
    ingressParams = globalIngressParams.filter(_.destId == i),
    egressParams = globalEgressParams.filter(_.srcId == i)
  )) }

  val ingressNodes = globalIngressParams.map { u => TerminalChannelSourceNode(u) }
  val egressNodes = globalEgressParams.map { u => TerminalChannelDestNode(u) }

  Seq.tabulate(nNodes, nNodes) { case (i, j) => if (i != j) {
    val sourceNodes = routers(i).sourceNodes.filter(_.sourceParams.destId == j)
    val destNodes = routers(j).destNodes.filter(_.destParams.srcId == i)
    require (sourceNodes.size == destNodes.size)
      (sourceNodes zip destNodes).foreach { t => t._2 := p(NoCKey).channelParamGen(i, j).channel(p)(t._1) }
  }}

  routers.zipWithIndex.map { case (dst,dstId) =>
    dst.ingressNodes.foreach(n =>
      n := ingressNodes(n.destParams.asInstanceOf[IngressChannelParams].ingressId)
    )
    dst.egressNodes.foreach(n =>
      egressNodes(n.sourceParams.asInstanceOf[EgressChannelParams].egressId) := n
    )


  }

  println("Constellation: Starting NoC RTL generation")
  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val ingress = MixedVec(globalIngressParams.map { u => Flipped(new TerminalChannel(u)) })
      val egress = MixedVec(globalEgressParams.map { u => new TerminalChannel(u) })
    })

    (io.ingress zip ingressNodes.map(_.out(0)._1)).foreach { case (l,r) => r <> l }
    (io.egress  zip egressNodes .map(_.in (0)._1)).foreach { case (l,r) => l <> r }

    val routerModules = routers.map(r => r.module)

    val debug_va_stall_ctr = RegInit(0.U(64.W))
    val debug_sa_stall_ctr = RegInit(0.U(64.W))
    val debug_any_stall_ctr = debug_va_stall_ctr + debug_sa_stall_ctr
    debug_va_stall_ctr := debug_va_stall_ctr + routerModules.map(_.io.debug.va_stall.reduce(_+_)).reduce(_+_)
    debug_sa_stall_ctr := debug_sa_stall_ctr + routerModules.map(_.io.debug.sa_stall.reduce(_+_)).reduce(_+_)

    dontTouch(debug_va_stall_ctr)
    dontTouch(debug_sa_stall_ctr)
    dontTouch(debug_any_stall_ctr)

    def prepend(s: String) = (p(NoCKey).prefix ++ Seq(s)).mkString(".")
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
    println(xys)
    ElaborationArtefacts.add(prepend("noc.xy"), xys)
  }
}
