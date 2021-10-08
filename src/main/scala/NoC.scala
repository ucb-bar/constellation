package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}


class NoC(implicit val p: Parameters) extends Module with HasAstroNoCParams{
  val io = IO(new Bundle {

  })

  val channelParams = (0 until nNodes).map { i => (0 until nNodes).map { j =>
    val vChannels = topologyFunction(i, j)
    if (vChannels.isEmpty) None else Some(ChannelParams(i, j, vChannels))
  }.flatten }.flatten

  val inParams = (0 until nNodes).map { i => channelParams.filter(_.destId == i) }
  val outParams = (0 until nNodes).map { i => channelParams.filter(_.srcId == i) }


  // val inParams = (0 until nNodes).map { i => (0 until nNodes).map { j =>
  //   val vChannels = topologyFunction(j, i)
  //   if (vChannels.isEmpty) None else Some(ChannelParams(j, i, vChannels))
  // }.flatten }
  // val outParams = (0 until nNodes).map { i => (0 until nNodes).map { j =>
  //   val vChannels = topologyFunction(i, j)
  //   if (vChannels.isEmpty) None else Some(ChannelParams(i, j, vChannels))
  // }.flatten }

  val router_nodes = Seq.tabulate(nNodes) { i => Module(new Router(RouterParams(
    i, inParams(i), outParams(i), virtualLegalPathsFunction(i), routingFunctions(i)
  ))) }

  val in_idxs = Array.fill(nNodes) { 0 }
  val out_idxs = Array.fill(nNodes) { 0 }

  router_nodes.zipWithIndex.map { case (dst,dstId) =>
    dst.io.in.map { in =>
      in <> router_nodes(in.cParams.srcId).io.out.filter(_.cParams.destId == dstId)(0)
    }
  }
}
