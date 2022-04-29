package constellation.noc

import chisel3._
import chisel3.util._


import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, BundleBridgeSink, InModuleBody}
import constellation.router._
import constellation.channel._
import constellation.routing._
import constellation.topology.{PhysicalTopology, UnidirectionalLine}


case class NoCParams(
  nVirtualNetworks: Int = 1,

  topology: PhysicalTopology = new UnidirectionalLine(1),
  channelParamGen: (Int, Int) => UserChannelParams = (a: Int, b: Int) => UserChannelParams(),
  ingresses: Seq[UserIngressParams] = Nil,
  egresses: Seq[UserEgressParams] = Nil,
  flows: Seq[FlowParams] = Nil,
  routingRelation: RoutingRelation = new AllLegalRouting,
  routerParams: Int => UserRouterParams = (i: Int) => UserRouterParams(),
  // (blocker, blockee) => bool
  // If true, then blocker must be able to proceed when blockee is blocked
  vNetBlocking: (Int, Int) => Boolean = (_: Int, _: Int) => true,
  nocName: String = "test",
  skipValidationChecks: Boolean = false,
  hasCtrl: Boolean = false,
)
case object NoCKey extends Field[NoCParams](NoCParams())


case object InternalNoCKey extends Field[InternalNoCParams]

case class InternalNoCParams(
  userParams: NoCParams,
  channelParams: Seq[ChannelParams],
  ingressParams: Seq[IngressChannelParams],
  egressParams: Seq[EgressChannelParams],
  routerParams: Seq[RouterParams]
)

trait HasNoCParams {
  implicit val p: Parameters
  val nocParams = p(InternalNoCKey)

  val nNodes = nocParams.userParams.topology.nNodes
  val nVirtualNetworks = nocParams.userParams.nVirtualNetworks
  val nocName = nocParams.userParams.nocName
  val hasCtrl = nocParams.userParams.hasCtrl

  val nodeIdBits = log2Ceil(nNodes)
  val vNetBits = log2Up(nocParams.userParams.nVirtualNetworks)
  val nEgresses = nocParams.egressParams.size
  val nIngresses = nocParams.ingressParams.size
  val egressIdBits = log2Up(nEgresses)
  val ingressIdBits = log2Up(nIngresses)
  val egressSrcIds = nocParams.egressParams.map(_.srcId)
  val routingRelation = nocParams.userParams.routingRelation
}

object InternalNoCParams {
  def apply(nocParams: NoCParams): InternalNoCParams = {
    val nNodes = nocParams.topology.nNodes
    val nVirtualNetworks = nocParams.nVirtualNetworks
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
      require(f.vNetId < nocParams.nVirtualNetworks)
    })

    for (i <- 0 until nocParams.ingresses.size) {
      for (e <- 0 until nocParams.egresses.size) {
        require(nocParams.flows.filter(f => f.ingressId == i && f.egressId == e).size <= 1)
      }
    }

    val ingressParams = nocParams.ingresses.zipWithIndex.map { case (u,i) => {
      require(u.destId < nNodes)
      IngressChannelParams(
        user = u,
        ingressId = i,
        flows = nocParams.flows,
        egresses = nocParams.egresses
      )
    }}
    val egressParams = nocParams.egresses.zipWithIndex.map { case (u,e) => {
      require(u.srcId < nNodes)
      EgressChannelParams(
        user = u,
        egressId = e,
        flows = nocParams.flows
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

          var stack: Seq[ChannelRoutingInfo] = iP.channelRoutingInfos
          while (stack.size != 0) {
            val head = stack.head
            flowPossibleFlows += head
            stack = stack.tail

            // This channel is the destination
            val atDest = head.dst == flow.dst

            if (!atDest) {
              // Find all possible VCs a flow in head can route to
              val nexts = nextChannelParamMap(head.dst).map { nxtC =>
                nxtC.channelRoutingInfos.map { cI =>
                  val can_transition = routingRel(
                    head,
                    cI,
                    flow
                  )
                  if (can_transition) Some(cI) else None
                }.flatten
              }.flatten
              // If the flow can't route to any VCs, this is an error
              require(nexts.size > 0,
                s"Failed to route from $iId to ${flow.dst} at $head for vnet $vNetId")
              val toAdd = nexts.filter(n => !flowPossibleFlows.contains(n))
              //require((nexts.toSet | stack.toSet).size == 0)
              stack = toAdd ++ stack
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
            .filter(_.dst != cI.dst)
            .map(pI => routingRel(cI, nI, pI))
            .fold(false)(_||_)
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

    // Check connectivity, ignoring blocking properties of virtual subnets
    println(s"Constellation: $nocName Checking full connectivity")
    for (vNetId <- 0 until nVirtualNetworks) {
      checkConnectivity(vNetId, nocParams.routingRelation)
    }

    // Connectivity for each virtual subnet
    if (skipValidationChecks) {
      println(s"Constellation WARNING: $nocName skip checking virtual subnet connectivity")
    } else {
      println(s"Constellation: $nocName Checking virtual subnet connectivity")
      for (vNetId <- 0 until nVirtualNetworks) {
        // blockees are vNets which the current vNet can block without affecting its own forwards progress
        val blockees = (0 until nVirtualNetworks).filter(v => v != vNetId && nocParams.vNetBlocking(vNetId, v))
        val blockeeSets = blockees.toSet.subsets.filter(_.size > 0)
        // For each subset of blockers for this virtual network, recheck connectivity assuming
        // every virtual channel accessible to each blocker is locked
        for (b <- blockeeSets) {
          checkConnectivity(vNetId, new RoutingRelation {
            def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {
              val base = nocParams.routingRelation(srcC, nxtC, flow)
              val blocked = b.map { v => possibleFlowMap(nxtC).map(_.vNet == v) }.flatten.fold(false)(_||_)
              base && !blocked
            }
          })
        }
      }
    }

    // Check for deadlock in escape channels
    println(s"Constellation: $nocName Checking for possibility of deadlock")
    val acyclicPath = checkAcyclic(new RoutingRelation {
      def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo) = {
        val escape = nocParams.routingRelation.isEscape(nxtC, flow.vNet)
        nocParams.routingRelation(srcC, nxtC, flow) && escape
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
        user = nocParams.routerParams(i)
      )
    }

    InternalNoCParams(
      userParams = nocParams,
      channelParams = finalChannelParams,
      ingressParams = ingressParams,
      egressParams = egressParams,
      routerParams = routerParams
    )
  }
}
