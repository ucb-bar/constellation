package constellation

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import constellation.router.{Router, RouterParams}
import constellation.routing._

class NoC(implicit val p: Parameters) extends Module with HasNoCParams{
  var uniqueChannelId = 0
  def getUniqueChannelId(): Int = {
    val r = uniqueChannelId
    uniqueChannelId = uniqueChannelId + 1
    r
  }

  val fullChannelParams: Seq[ChannelParams] = Seq.tabulate(nNodes, nNodes) { case (i,j) =>
    topologyFunction(i, j).map { cP =>
      ChannelParams(i, j, cP.depth, cP.virtualChannelParams.zipWithIndex.map { case (vP, vc) =>
        VirtualChannelParams(i, j, vc, vP.bufferSize, Set[PacketInfo](), getUniqueChannelId())
      })
    }
  }.flatten.flatten

  val globalIngressParams = p(NoCKey).ingresses.zipWithIndex.map { case (u,i) =>
    IngressChannelParams(u.destId, u.possibleEgresses, u.vNetId, i, getUniqueChannelId())
  }
  val globalEgressParams = p(NoCKey).egresses.zipWithIndex.map { case (u,e) =>
    EgressChannelParams(u.srcId, e, getUniqueChannelId(),
      globalIngressParams.filter(_.possibleEgresses.contains(e)).map { i =>
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
    val blockeeSets = blockees.toSet.subsets
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

  println("Constellation: Starting NoC RTL generation")

  val io = IO(new Bundle {
    val ingress = MixedVec(globalIngressParams.map { u => Flipped(new TerminalChannel(u)) })
    val egress = MixedVec(globalEgressParams.map { u => new TerminalChannel(u) })
  })

  val router_nodes = Seq.tabulate(nNodes) { i => Module(new Router(RouterParams(
    nodeId = i,
    inParams = channelParams.filter(_.destId == i),
    outParams = channelParams.filter(_.srcId == i),
    ingressParams = globalIngressParams.filter(_.destId == i),
    egressParams = globalEgressParams.filter(_.srcId == i),
    nodeRoutingRelation = p(NoCKey).routingRelation(i),
    combineSAST = routerParams(i).combineSAST,
    combineRCVA = routerParams(i).combineRCVA
  ))) }

  router_nodes.zipWithIndex.map { case (dst,dstId) =>
    dst.io.in.map { in =>
      in <> ChannelBuffer(
        router_nodes(in.cParam.srcId).io.out.filter(_.cParam.destId == dstId)(0),
        in.cParam
      )
    }
    (dst.ingressParams zip dst.io.ingress) map { case (u,i) =>
      i <> io.ingress(u.ingressId)
    }
    (dst.egressParams zip dst.io.egress) map { case (u,i) =>
      io.egress(u.egressId) <> i
    }
  }

  val debug_va_stall_ctr = RegInit(0.U(64.W))
  val debug_sa_stall_ctr = RegInit(0.U(64.W))
  val debug_any_stall_ctr = debug_va_stall_ctr + debug_sa_stall_ctr
  debug_va_stall_ctr := debug_va_stall_ctr + router_nodes.map(_.io.debug.va_stall.reduce(_+_)).reduce(_+_)
  debug_sa_stall_ctr := debug_sa_stall_ctr + router_nodes.map(_.io.debug.sa_stall.reduce(_+_)).reduce(_+_)

  dontTouch(debug_va_stall_ctr)
  dontTouch(debug_sa_stall_ctr)
  dontTouch(debug_any_stall_ctr)
}
