package constellation.noc

import chisel3._
import chisel3.util._


import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, BundleBridgeSink, InModuleBody}
import constellation.router._
import constellation.channel._
import constellation.routing._
import constellation.topology.{PhysicalTopology, UnidirectionalLine}

// BEGIN: NoC Parameters
case class NoCParams(
  // Physical specifications
  topology: PhysicalTopology = UnidirectionalLine(1),
  channelParamGen: (Int, Int) => UserChannelParams = (_, _) => UserChannelParams(),
  ingresses: Seq[UserIngressParams] = Nil,
  egresses: Seq[UserEgressParams] = Nil,
  routerParams: Int => UserRouterParams = (i: Int) => UserRouterParams(),

  // Flow specification
  // (blocker, blockee) => bool
  // If true, then blocker must be able to proceed when blockee is blocked
  vNetBlocking: (Int, Int) => Boolean = (_, _) => true,
  flows: Seq[FlowParams] = Nil,

  // Routing specification
  routingRelation: PhysicalTopology => RoutingRelation = AllLegalRouting(),

  // other
  nocName: String = "test",
  skipValidationChecks: Boolean = false,
  hasCtrl: Boolean = false,
)
// END: NoC Parameters


case object InternalNoCKey extends Field[InternalNoCParams]

case class InternalNoCParams(
  userParams: NoCParams,
  nVirtualNetworks: Int,
  routingRelation: RoutingRelation,
  channelParams: Seq[ChannelParams],
  ingressParams: Seq[IngressChannelParams],
  egressParams: Seq[EgressChannelParams],
  routerParams: Seq[RouterParams]
)

trait HasNoCParams {
  implicit val p: Parameters
  val nocParams = p(InternalNoCKey)

  def nNodes = nocParams.userParams.topology.nNodes
  def nVirtualNetworks = nocParams.nVirtualNetworks
  def nocName = nocParams.userParams.nocName
  def hasCtrl = nocParams.userParams.hasCtrl

  def nodeIdBits = log2Ceil(nNodes)
  def vNetBits = log2Up(nVirtualNetworks)
  def nEgresses = nocParams.egressParams.size
  def nIngresses = nocParams.ingressParams.size
  def egressIdBits = log2Up(nEgresses)
  def ingressIdBits = log2Up(nIngresses)
  def egressSrcIds = nocParams.egressParams.map(_.srcId)
  def maxIngressesAtNode = nocParams.routerParams.map(_.nIngress).max
  def maxEgressesAtNode = nocParams.routerParams.map(_.nEgress).max
  def routingRelation = nocParams.routingRelation
  def virtualChannelBits = log2Up((nocParams.channelParams.map(_.nVirtualChannels) :+ 0).max)
}

object InternalNoCParams {
  def apply(nocParams: NoCParams): InternalNoCParams = {
    val nNodes = nocParams.topology.nNodes
    val nVirtualNetworks = nocParams.flows.map(_.vNetId).max + 1
    val nocName = nocParams.nocName
    val skipValidationChecks = nocParams.skipValidationChecks

    val channelParams = Seq.tabulate(nNodes, nNodes) { case (i,j) =>
      if (nocParams.topology.topo(i, j)) {
        val cP = nocParams.channelParamGen(i, j)
        val payloadBits = nocParams.routerParams(j).payloadBits
        Some(ChannelParams(
          srcId = i,
          destId = j,
          payloadBits = payloadBits,
          user = cP,
        ))
      } else {
        None
      }
    }.flatten.flatten

    nocParams.flows.foreach(f => {
      require(f.ingressId < nocParams.ingresses.size)
      require(f.egressId < nocParams.egresses.size)
    })

    for (i <- 0 until nocParams.ingresses.size) {
      for (e <- 0 until nocParams.egresses.size) {
        require(nocParams.flows.filter(f => f.ingressId == i && f.egressId == e).size <= 1)
      }
    }

    val flows = nocParams.flows.map { f =>
      val ingressNode = nocParams.ingresses(f.ingressId).destId
      val egressNode  = nocParams.egresses (f.egressId ).srcId
      FlowRoutingInfo(
        ingressId=f.ingressId, egressId=f.egressId, vNetId=f.vNetId,
        ingressNode=ingressNode,
        egressNode=egressNode,
        ingressNodeId=nocParams.ingresses.take(f.ingressId).count(_.destId == ingressNode),
        egressNodeId=nocParams.egresses.take(f.egressId).count(_.srcId == egressNode)
      )
    }
    val ingressParams = nocParams.ingresses.zipWithIndex.map { case (u,i) => {
      require(u.destId < nNodes, s"Ingress $i: ${u.destId} >= $nNodes")
      IngressChannelParams(
        user = u,
        ingressId = i,
        flows = flows
      )
    }}
    val egressParams = nocParams.egresses.zipWithIndex.map { case (u,e) => {
      require(u.srcId < nNodes, s"Egress $e: ${u.srcId} >= $nNodes")
      EgressChannelParams(
        user = u,
        egressId = e,
        flows = flows
      )
    }}


    // Check sanity of routingRelation, all inputs can route to all outputs

    // Tracks the set of every possible flow that might occupy each virtual channel
    val possibleFlowMap = scala.collection.mutable.Map[ChannelRoutingInfo, Set[FlowRoutingInfo]]()
      .withDefaultValue(Set())

    /** Uses ROUTINGREL to check that, for any ingress, all possible flows from said ingress are
      *  able to reach all intended egresses.
      *
      * @param vNetId virtual network id
      * @param routingRel the routing relation for the network
      */
    def checkConnectivity(vNetId: Int, routingRel: RoutingRelation) = {
      val nextChannelParamMap = (0 until nNodes).map { i => i -> channelParams.filter(_.srcId == i) }.toMap
      val tempPossibleFlowMap = scala.collection.mutable.Map[ChannelRoutingInfo, Set[FlowRoutingInfo]]()
        .withDefaultValue(Set())

      // Loop through accessible ingress/egress pairs
      ingressParams.filter(_.vNetId == vNetId).map { iP =>
        val iId = iP.destId
        //println(s"Constellation: $nocName Checking connectivity from ingress $iIdx")
        iP.possibleFlows.map { flow =>

          val flowPossibleFlows = scala.collection.mutable.Set[ChannelRoutingInfo]()
          var unexplored: Seq[ChannelRoutingInfo] = iP.channelRoutingInfos
          while (unexplored.size != 0) {
            var stack: Seq[ChannelRoutingInfo] = Seq(unexplored.head)
            unexplored = unexplored.tail
            while (stack.size != 0) {
              val head = stack.head
              val atDest = head.dst == flow.egressNode || flowPossibleFlows.contains(head)
              if (!atDest) {
                val nexts = nextChannelParamMap(head.dst).map { nxtC =>
                  nxtC.channelRoutingInfos.map { cI =>
                    if (routingRel(head, cI, flow)) Some(cI) else None
                  }.flatten
                }.flatten
                require(nexts.size > 0,
                  s"Failed to route $flow at $head \n  $stack \n  ${nextChannelParamMap(head.dst)}")
                require((nexts.toSet & stack.toSet).size == 0,
                  s"$flow, $nexts, $stack")
                stack = Seq(nexts.head) ++ stack
                unexplored = nexts.tail.filter(n => !unexplored.contains(n)) ++ unexplored
              } else {
                flowPossibleFlows += head
                flowPossibleFlows ++= stack
                stack = Nil
              }
            }
          }
          flowPossibleFlows.foreach { k => tempPossibleFlowMap(k) += flow }
        }
      }
      tempPossibleFlowMap.foreach { case (k,v) => possibleFlowMap(k) ++= v }
    }
    def checkAcyclic(routingRel: RoutingRelation): Option[Seq[ChannelRoutingInfo]] = {
      var visited = Set[ChannelRoutingInfo]()
      var stack = Seq[ChannelRoutingInfo]()
      def checkAcyclicUtil(cI: ChannelRoutingInfo): Boolean = {
        visited = visited + cI
        stack = stack ++ Seq(cI)
        val neighbors = channelParams
          .filter(_.srcId == cI.dst)
          .map(_.channelRoutingInfos)
          .flatten
          .filter(nI => possibleFlowMap(cI)
            .filter(_.egressNode != cI.dst)
            .foldLeft(false) { (b, pI) => b || routingRel(cI, nI, pI) }
          )
        neighbors.foreach { nI =>
          if (!visited.contains(nI)) {
            if (!checkAcyclicUtil(nI)) return false
          } else if (stack.contains(nI)) {
            stack = stack ++ Seq(nI)
            return false
          }
        }
        stack = stack.dropRight(1)
        return true
      }

      ingressParams.zipWithIndex.map { case (iP,iIdx) =>
        if (!checkAcyclicUtil(iP.channelRoutingInfos(0))) {
          return Some(stack)
        }
      }
      return None
    }

    val routingRelation = nocParams.routingRelation(nocParams.topology)

    // Check connectivity, ignoring blocking properties of virtual subnets
    println(s"Constellation: $nocName Checking full connectivity")
    for (vNetId <- 0 until nVirtualNetworks) {
      checkConnectivity(vNetId, routingRelation)
    }

    // Connectivity for each virtual subnet
    if (skipValidationChecks) {
      println(s"Constellation WARNING: $nocName skip checking virtual subnet connectivity")
    } else {
      println(s"Constellation: $nocName Checking virtual subnet connectivity")
      for (vNetId <- 0 until nVirtualNetworks) {
        // blockees are vNets which the current vNet can block without affecting its own forwards progress
        val blockees = (0 until nVirtualNetworks).filter(v =>
          v != vNetId && nocParams.vNetBlocking(vNetId, v) && flows.exists(_.vNetId == v)
        )
        if (blockees.size > 0)
          println(s"Constellation: $nocName Checking if $vNetId can proceed when blocked by ${blockees}")
        val blockeeSets = blockees.toSet.subsets.filter(_.size > 0)
        // For each subset of blockers for this virtual network, recheck connectivity assuming
        // every virtual channel accessible to each blocker is locked
        for (b <- blockeeSets) {
          checkConnectivity(vNetId, new RoutingRelation(nocParams.topology) {
            def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {
              val base = routingRelation(srcC, nxtC, flow)
              val blocked = b.map { v => possibleFlowMap(nxtC).map(_.vNetId == v) }.flatten.fold(false)(_||_)
              base && !blocked
            }
          })
        }
      }
    }

    // Check for deadlock in escape channels
    println(s"Constellation: $nocName Checking for possibility of deadlock")
    val acyclicPath = checkAcyclic(new RoutingRelation(nocParams.topology) {
      def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {
        val escape = routingRelation.isEscape(nxtC, flow.vNetId)
        routingRelation(srcC, nxtC, flow) && escape
      }
    })
    require(acyclicPath.isEmpty, s"Cyclic path may cause deadlock: ${acyclicPath.get}")

    // Tie off inpossible virtual channels
    // Also set possible nodes for each channel
    val finalChannelParams = channelParams.map { cP => cP.copy(
      virtualChannelParams=cP.virtualChannelParams.zipWithIndex.map { case (vP,vId) =>
        val traversable = possibleFlowMap(cP.channelRoutingInfos(vId)).size != 0
        if (!traversable) {
          println(s"Constellation WARNING: $nocName virtual channel $vId from ${cP.srcId} to ${cP.destId} appears to be untraversable")
        }
        vP.copy(
          possibleFlows=possibleFlowMap(cP.channelRoutingInfos(vId))
        )
      }
    )}.map(cP => {
      if (!cP.traversable)
        println(s"Constellation WARNING: $nocName physical channel from ${cP.srcId} to ${cP.destId} appears to be untraversable.")
      cP
    }).filter(_.traversable)

    val routerParams = (0 until nNodes).map { i =>
      RouterParams(
        nodeId = i,
        nIngress = ingressParams.count(_.destId == i),
        nEgress = egressParams.count(_.srcId == i),
        user = nocParams.routerParams(i)
      )
    }

    InternalNoCParams(
      userParams = nocParams,
      nVirtualNetworks = nVirtualNetworks,
      routingRelation = routingRelation,
      channelParams = finalChannelParams,
      ingressParams = ingressParams,
      egressParams = egressParams,
      routerParams = routerParams
    )
  }
}
