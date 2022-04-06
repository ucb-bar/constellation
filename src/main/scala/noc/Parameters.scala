package constellation.noc

import chisel3._
import chisel3.util._


import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, BundleBridgeSink, InModuleBody}
import constellation.router._
import constellation.channel._
import constellation.routing.{RoutingRelation, PacketRoutingInfo, ChannelRoutingInfo}
import constellation.topology.{PhysicalTopology, UnidirectionalLine}


case class NoCParams(
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
  skipValidationChecks: Boolean = false,
  hasCtrl: Boolean = false
)
case object NoCKey extends Field[NoCParams](NoCParams())

trait HasNoCParams {
  implicit val p: Parameters
  val nocParams = p(NoCKey)

  val nNodes = nocParams.topology.nNodes
  val nVirtualNetworks = nocParams.nVirtualNetworks
  val nocName = nocParams.nocName
  val skipValidationChecks = nocParams.skipValidationChecks
  val hasCtrl = nocParams.hasCtrl

  val nodeIdBits = log2Ceil(nNodes)
  val vNetBits = log2Up(nocParams.nVirtualNetworks)
  val nEgresses = nocParams.egresses.size
  val egressIdBits = log2Up(nocParams.egresses.size)
  val egressSrcIds = nocParams.egresses.map(_.srcId)
}

case class InternalNoCParams(
  userParams: NoCParams,
  channelParams: Seq[ChannelParams],
  ingressParams: Seq[IngressChannelParams],
  egressParams: Seq[EgressChannelParams],
  routerParams: Seq[RouterParams]
)

object InternalNoCParams {
  def apply(nocParams: NoCParams): InternalNoCParams = {
    val nNodes = nocParams.topology.nNodes
    val nVirtualNetworks = nocParams.nVirtualNetworks
    val nocName = nocParams.nocName
    val skipValidationChecks = nocParams.skipValidationChecks

    var uniqueChannelId = 0
    def getUniqueChannelId(): Int = {
      val r = uniqueChannelId
      uniqueChannelId = uniqueChannelId + 1
      r
    }

    val channelParams = Seq.tabulate(nNodes, nNodes) { case (i,j) =>
      if (nocParams.topology.topo(i, j)) {
        val cP = nocParams.channelParamGen(i, j)
        val payloadBits = nocParams.routerParams(i).payloadBits
        require(nocParams.routerParams(i).payloadBits == nocParams.routerParams(j).payloadBits)
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

    val ingressParams = nocParams.ingresses.zipWithIndex.map { case (u,i) => {
      require(u.destId < nNodes)
      IngressChannelParams(
        user = u,
        ingressId = i,
        uniqueId = getUniqueChannelId())
    }}
    val egressParams = nocParams.egresses.zipWithIndex.map { case (u,e) => {
      require(u.srcId < nNodes)
      EgressChannelParams(
        user = u,
        egressId = e,
        uniqueId = getUniqueChannelId(),
        possiblePackets = ingressParams.filter(_.possibleEgresses.contains(e)).map { i =>
          PacketRoutingInfo(e, i.vNetId, u.srcId)
        }.toSet
      )
    }}

    ingressParams.foreach(_.possibleEgresses.foreach(e => require(e < egressParams.size)))


    // Check sanity of routingRelation, all inputs can route to all outputs

    // Tracks the set of every possible packet that might occupy each virtual channel
    val possiblePacketMap = scala.collection.mutable.Map[ChannelRoutingInfo, Set[PacketRoutingInfo]]()
      .withDefaultValue(Set())

    /** Uses ROUTINGREL to check that, for any ingress, all possible packets from said ingress are
      *  able to reach all intended egresses.
      *
      * @param vNetId virtual network id
      * @param routingRel the routing relation for the network
      */
    def checkConnectivity(vNetId: Int, routingRel: RoutingRelation) = {
      val nextChannelParamMap = (0 until nNodes).map { i => i -> channelParams.filter(_.srcId == i) }.toMap
      val tempPossiblePacketMap = scala.collection.mutable.Map[ChannelRoutingInfo, Set[PacketRoutingInfo]]()
        .withDefaultValue(Set())

      // Loop through accessible ingress/egress pairs
      ingressParams.zipWithIndex.filter(_._1.vNetId == vNetId).map { case (iP,iIdx) =>
        val iId = iP.destId
        //println(s"Constellation: $nocName Checking connectivity from ingress $iIdx")
        iP.possibleEgresses.toSeq.sorted.map { oIdx =>
          val oP = egressParams(oIdx)
          val oId = oP.srcId
          val pInfo = PacketRoutingInfo(oIdx, vNetId, oId)
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

        val neighbors = channelParams.filter(_.srcId == cI.dst).map(_.channelRoutingInfos).flatten.filter { nI =>
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

      ingressParams.filter(_.vNetId == vNetId).zipWithIndex.map { case (iP,iIdx) =>
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
          val routingRel = nocParams.routingRelation
          checkConnectivity(vNetId, new RoutingRelation((nodeId, srcC, nxtC, pInfo) => {
            val base = routingRel(nodeId, srcC, nxtC, pInfo)
            val blocked = b.map { v => possiblePacketMap(nxtC).map(_.vNet == v) }.flatten.fold(false)(_||_)
            base && !blocked
          }))
        }
      }
    }

    // Check for deadlock in escape channels
    println(s"Constellation: $nocName Checking for possibility of deadlock")
    for (vNetId <- 0 until nVirtualNetworks) {
      // blockees are vNets which the current vNet can block without affecting its own forwards progress
      val blockees = (0 until nVirtualNetworks).filter(v => v != vNetId && nocParams.vNetBlocking(vNetId, v))
      val blockeeSets = blockees.toSet.subsets
      // For each subset of blockers for this virtual network, recheck connectivity assuming
      // every virtual channel accessible to each blocker is locked
      for (b <- blockeeSets) {
        val routingRel = nocParams.routingRelation
        val acyclicPath = checkAcyclic(vNetId, new RoutingRelation((nodeId, srcC, nxtC, pInfo) => {
          val base = routingRel(nodeId, srcC, nxtC, pInfo)
          val blocked = b.map { v => possiblePacketMap(nxtC).map(_.vNet == v) }.flatten.fold(false)(_||_)
          val escape = routingRel.isEscape(nxtC, vNetId)
          base && !blocked && escape
        }))
        acyclicPath.foreach { path =>
          println(s"Constellation WARNING: $nocName cyclic path on virtual network $vNetId may cause deadlock: ${acyclicPath.get}")
        }
      }
    }

    // Tie off inpossible virtual channels
    // Also set possible nodes for each channel
    val finalChannelParams = channelParams.map { cP => cP.copy(
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
