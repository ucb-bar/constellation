package constellation

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import constellation.router.{Router, RouterParams}

class NoC(implicit val p: Parameters) extends Module with HasNoCParams{
  val fullChannelParams: Seq[ChannelParams] = Seq.tabulate(nNodes, nNodes) { case (i,j) =>
    topologyFunction(i, j)
  }.flatten.flatten
  def fullInParams = (0 until nNodes).map { i => fullChannelParams.filter(_.destId == i) }
  def fullOutParams = (0 until nNodes).map { i => fullChannelParams.filter(_.srcId == i) }
  val inputParams = inputNodes.zipWithIndex.map { case (nId,i) =>
    ChannelParams(-1, nId, Seq(VirtualChannelParams(-1)), terminalInputId=i)
  }
  val outputParams = outputNodes.zipWithIndex.map { case (nId,i) =>
    ChannelParams(nId, -1, Seq(VirtualChannelParams(-1)), terminalOutputId=i)
  }

  // Check sanity of masterAllocTable, all inputs can route to all outputs

  // srcId, vId, dstId
  type Pos = (Int, Int, Int)
  var traversableVirtualChannels: Set[Pos] = Set[Pos]()

  for (vNetId <- 0 until nVirtualNetworks) {
    inputNodes.distinct.map { iId =>
      outputNodes.distinct.map { oId =>
        var positions: Set[Pos] = Set((-1, 0, iId))
        while (positions.size != 0) {
          positions = positions.filter(_._3 != oId).map { case (srcId, srcV, nodeId) =>
            val nexts = fullOutParams(nodeId).map { nxtC =>
              (0 until nxtC.nVirtualChannels).map { nxtV =>
                val can_transition = masterAllocTable(
                  nodeId)(srcId, srcV, nxtC.destId, nxtV, oId, vNetId)
                if (can_transition) Some((nodeId, nxtV, nxtC.destId)) else None
              }.flatten
            }.flatten
            require(nexts.size > 0,
              s"Failed to route from $iId to $oId at $srcId, $srcV, $nodeId")
            traversableVirtualChannels = traversableVirtualChannels ++ nexts.toSet
            nexts
          }.flatten.toSet
        }
      }
    }
  }
  
  // Tie off inaccessible virtual channels
  val channelParams = fullChannelParams.map { cP => cP.copy(
    virtualChannelParams=cP.virtualChannelParams.zipWithIndex.map { case (vP,vId) =>
      vP.copy(traversable=traversableVirtualChannels.contains((cP.srcId, vId, cP.destId)))
    }
  )}
  channelParams.map(cP => if (!cP.traverable)
    println(s"WARNING, channel from $cP.srcId to $cp.destId appears to be untraversable"))
  val inParams = (0 until nNodes).map { i => channelParams.filter(_.destId == i) }
  val outParams = (0 until nNodes).map { i => channelParams.filter(_.srcId == i) }

  val io = IO(new Bundle {
    val in = MixedVec(inputParams.map { u => Flipped(new IOChannel(u)) })
    val out = MixedVec(outputParams.map { u => new IOChannel(u) })
  })

  val router_nodes = Seq.tabulate(nNodes) { i => Module(new Router(RouterParams(
    i,
    inParams(i),
    outParams(i),
    inputParams.filter(_.destId == i),
    outputParams.filter(_.srcId == i),
    masterAllocTable(i)
  ))) }

  router_nodes.zipWithIndex.map { case (dst,dstId) =>
    dst.io.in.map { in =>
      in <> ChannelBuffer(
        router_nodes(in.cParam.srcId).io.out.filter(_.cParam.destId == dstId)(0),
        in.cParam
      )
    }
    (dst.terminalInParams zip dst.io.terminal_in) map { case (u,i) =>
      i <> io.in(u.terminalInputId)
    }
    (dst.terminalOutParams zip dst.io.terminal_out) map { case (u,i) =>
      io.out(u.terminalOutputId) <> i
    }
  }
}
