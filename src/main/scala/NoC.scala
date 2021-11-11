package constellation

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import constellation.router.{Router, RouterParams}

class NoC(implicit val p: Parameters) extends Module with HasNoCParams{
  val fullChannelParams: Seq[ChannelParams] = Seq.tabulate(nNodes, nNodes) { case (i,j) =>
    topologyFunction(i, j)
  }.flatten.flatten

  // Check sanity of masterAllocTable, all inputs can route to all outputs

  // srcId, vId, dstId
  type Pos = (Int, Int, Int)

  // Tracks the set of every possible packet that might occupy each virtual channel
  val possiblePacketMap = scala.collection.mutable.Map[Pos, Set[PacketRoutingInfo]]().withDefaultValue(Set())

  def checkConnectivity(vNetId: Int, f: (Int, Int, Int, Int, Int, Int, Int) => Boolean) = {
    // Loop through accessible ingress/egress pairs
    globalIngressParams.filter(_.vNetId == vNetId).zipWithIndex.map { case (iP,iIdx) =>
      val iId = iP.destId
      iP.possibleEgresses.map { oIdx =>
        val oP = globalEgressParams(oIdx)
        val oId = oP.srcId

        // Track the positions a packet performing ingress->egress might occupy
        var positions: Set[Pos] = Set((-1, 0, iId))
        while (positions.size != 0) {
          positions.foreach { pos => possiblePacketMap(pos) += (PacketRoutingInfo(oIdx, vNetId)) }
          // Determine next possible positions based on current possible positions
          // and connectivity function
          positions = positions.filter(_._3 != oId).map { case (srcId, srcV, nodeId) =>
            val nexts = fullChannelParams.filter(_.srcId == nodeId).map { nxtC =>
              (0 until nxtC.nVirtualChannels).map { nxtV =>
                val can_transition = f(nodeId, srcId, srcV, nxtC.destId, nxtV, oId, vNetId)
                if (can_transition) Some((nodeId, nxtV, nxtC.destId)) else None
              }.flatten
            }.flatten
            require(nexts.size > 0,
              s"Failed to route from $iId to $oId at $srcId, $srcV, $nodeId")
            nexts
          }.flatten.toSet
        }
      }
    }
  }

  // Check connectivity, ignoring blocking properties of virtual subnets
  println("Constellation: Checking full connectivity")
  for (vNetId <- 0 until nVirtualNetworks) {
    checkConnectivity(vNetId,
      (nodeId: Int, srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, oId: Int, vNetId: Int) => {
        masterAllocTable(nodeId)(srcId, srcV, nxtId, nxtV, oId, vNetId)
      }
    )
  }

  // Connectivity for each virtual subnet
  println("Constellation: Checking virtual subnet connectivity")
  for (vNetId <- 0 until nVirtualNetworks) {
    // blockees are vNets which the current vNet can block without affecting its own forwards progress
    val blockees = (0 until nVirtualNetworks).filter(v => v != vNetId && params.vNetBlocking(vNetId, v))
    val blockeeSets = blockees.toSet.subsets
    // For each subset of blockers for this virtual network, recheck connectivity assuming
    // every virtual channel accessible to each blocker is locked
    for (b <- blockeeSets) {
      checkConnectivity(vNetId,
        (nodeId: Int, srcId: Int, srcV: Int, nxtId: Int, nxtV: Int, oId: Int, vNetId: Int) => {
          (masterAllocTable(nodeId)(srcId, srcV, nxtId, nxtV, oId, vNetId) &&
            !(b.map { v => possiblePacketMap((nodeId, nxtV, nxtId)).map(_.vNetId == v) }.flatten.fold(false)(_||_))
          )
        }
      )
    }
  }

  // Tie off inpossible virtual channels
  // Also set possible nodes for each channel
  val channelParams = fullChannelParams.map { cP => cP.copy(
    virtualChannelParams=cP.virtualChannelParams.zipWithIndex.map { case (vP,vId) =>
      val traversable = possiblePacketMap((cP.srcId, vId, cP.destId)).size != 0
      if (!traversable) {
        println(s"Constellation WARNING: virtual channel $vId from ${cP.srcId} to ${cP.destId} appears to be untraversable")
      }
      vP.copy(possiblePackets=possiblePacketMap((cP.srcId, vId, cP.destId)))
    }
  )}
  channelParams.map(cP => if (!cP.traversable)
    println(s"Constellation WARNING: physical channel from ${cP.srcId} to ${cP.destId} appears to be untraversable"))

  println("Constellation: Starting NoC RTL generation")

  val io = IO(new Bundle {
    val ingress = MixedVec(globalIngressParams.map { u => Flipped(new TerminalChannel(u)) })
    val egress = MixedVec(globalEgressParams.map { u => new TerminalChannel(u) })
  })

  val router_nodes = Seq.tabulate(nNodes) { i => Module(new Router(routerParams(i).copy(
    nodeId = i,
    inParams = channelParams.filter(_.destId == i),
    outParams = channelParams.filter(_.srcId == i),
    ingressParams = globalIngressParams.filter(_.destId == i),
    egressParams = globalEgressParams.filter(_.srcId == i),
    masterAllocTable = masterAllocTable(i),
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
}
