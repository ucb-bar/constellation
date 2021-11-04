package constellation

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import constellation.router.{Router, RouterParams}

class NoC(implicit val p: Parameters) extends Module with HasNoCParams{
  val fullChannelParams: Seq[ChannelParams] = Seq.tabulate(nNodes, nNodes) { case (i,j) =>
    topologyFunction(i, j)
  }.flatten.flatten
  val ingressParams = ingressNodes.zipWithIndex.map { case (nId,i) =>
    ChannelParams(-1, nId, Seq(VirtualChannelParams(-1,
      possiblePackets=Seq.tabulate(egressNodes.size, nVirtualNetworks) { case (e, v) => (terminalConnectivity(i,e,v), (e, v)) }
        .flatten.filter(_._1).map(_._2).toSet
    )), ingressId=Some(i), vNetId=Some(ingressVNets(i)))
  }
  val egressParams = egressNodes.zipWithIndex.map { case (nId,e) =>
    ChannelParams(nId, -1, Seq(VirtualChannelParams(-1,
      possiblePackets=Seq.tabulate(nVirtualNetworks) { v =>
        ((0 until ingressNodes.size).map { i => terminalConnectivity(i,e,v) }.reduce(_||_), (e, v))
      }.filter(_._1).map(_._2).toSet
    )), egressId=Some(e))
  }

  // Check sanity of masterAllocTable, all inputs can route to all outputs

  // srcId, vId, dstId
  type Pos = (Int, Int, Int)
  val possiblePacketMap = scala.collection.mutable.Map[Pos, Set[(Int, Int)]]().withDefaultValue(Set[(Int, Int)]())

  ingressNodes.zipWithIndex.map { case (iId,iIdx) =>
    val vNetId = ingressVNets(iIdx)
    egressNodes.zipWithIndex.map { case (oId,oIdx) =>
      if (terminalConnectivity(iIdx, oIdx, vNetId)) {
        var positions: Set[Pos] = Set((-1, 0, iId))
        while (positions.size != 0) {
          positions.foreach { pos => possiblePacketMap(pos) += ((oIdx, vNetId)) }
          positions = positions.filter(_._3 != oId).map { case (srcId, srcV, nodeId) =>
            val nexts = fullChannelParams.filter(_.srcId == nodeId).map { nxtC =>
              (0 until nxtC.nVirtualChannels).map { nxtV =>
                val can_transition = masterAllocTable(
                  nodeId)(srcId, srcV, nxtC.destId, nxtV, oId, vNetId)
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

  // Tie off inpossible virtual channels
  // Also set possible nodes for each channel
  val channelParams = fullChannelParams.map { cP => cP.copy(
    virtualChannelParams=cP.virtualChannelParams.zipWithIndex.map { case (vP,vId) =>
      val traversable = possiblePacketMap((cP.srcId, vId, cP.destId)).size != 0
      if (!traversable) {
        println(s"WARNING, virtual channel $vId from ${cP.srcId} to ${cP.destId} appears to be untraversable")
      }
      vP.copy(possiblePackets=possiblePacketMap((cP.srcId, vId, cP.destId)))
    }
  )}
  channelParams.map(cP => if (!cP.traversable)
    println(s"WARNING, physical channel from ${cP.srcId} to ${cP.destId} appears to be untraversable"))

  val io = IO(new Bundle {
    val ingress = MixedVec(ingressParams.map { u => Flipped(new TerminalChannel(u)) })
    val egress = MixedVec(egressParams.map { u => new TerminalChannel(u) })
  })

  val router_nodes = Seq.tabulate(nNodes) { i => Module(new Router(routerParams(i).copy(
    nodeId = i,
    inParams = channelParams.filter(_.destId == i),
    outParams = channelParams.filter(_.srcId == i),
    ingressParams = ingressParams.filter(_.destId == i),
    egressParams = egressParams.filter(_.srcId == i),
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
      i <> io.ingress(u.ingressId.get)
    }
    (dst.egressParams zip dst.io.egress) map { case (u,i) =>
      io.egress(u.egressId.get) <> i
    }
  }
}
