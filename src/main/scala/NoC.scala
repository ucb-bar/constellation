package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}


class NoC(implicit val p: Parameters) extends Module with HasAstroNoCParams{
  val io = IO(new Bundle {
    val in = Vec(inputNodes.size, Vec(nPrios, Flipped(Decoupled(new Flit))))
  })

  val channelParams: Seq[ChannelParams] = Seq.tabulate(nNodes, nNodes) { case (i,j) =>
    val vChannels = topologyFunction(i, j)
    if (vChannels.isEmpty) None else Some(ChannelParams(i, j, vChannels))
  }.flatten.flatten
  val inParams = (0 until nNodes).map { i => channelParams.filter(_.destId == i) }
  val outParams = (0 until nNodes).map { i => channelParams.filter(_.srcId == i) }


  // srcId, destId, virtChannelId, prio
  type ResourceTuple = (Int, Int, Int, Int)
  val resourceTuples: Seq[ResourceTuple] = channelParams.map { c =>
    Seq.tabulate(c.virtualChannelParams.size) { v =>
      Seq.tabulate(nPrios) { p => (c.srcId, c.destId, v, p) }
    }.flatten
  }.flatten
  val edgesList: Seq[(ResourceTuple, ResourceTuple)] =
    (0 until nNodes).map { n =>
      inParams(n).map { iP =>
        (0 until iP.virtualChannelParams.size).map { iv =>
          outParams(n).map { oP =>
            (0 until oP.virtualChannelParams.size).map { ov =>
              (0 until nPrios).map { inPrio =>
                if (virtualLegalPathsFunction(n)(iP.srcId, iv, oP.destId, ov)(inPrio)) {
                  (0 until nPrios).map { outPrio =>
                    ((iP.srcId, n, iv, inPrio), (n, oP.destId, ov, outPrio))
                  }
                } else {
                  Nil
                }
              }.flatten
            }.flatten
          }.flatten
        }.flatten
      }.flatten
    }.flatten
  // resourceTuples.foreach(t => println(t))
  // edgesList.foreach(t => println(t))

  // def checkCyclic: Boolean = {
  //   val visited = scala.collection.mutable.Map[ResourceTuple, Boolean]().withDefaultValue(false)
  //   val recStack = scala.collection.mutable.Map[ResourceTuple, Boolean]().withDefaultValue(false)

  //   def isCyclicUtil(node: ResourceTuple): Boolean = {
  //     visited(node) = true
  //     recStack(node) = true

  //     for (next <- edges(node))
  //       if (!visited(next)) {
  //         if (isCyclicUtil(next)) return true
  //       } else if (recStack(next)) {
  //         return true
  //       }

  //     recStack(node) = false
  //     false
  //   }

  //   for (node <- nodes)
  //     if (!visited(node))
  //       if (isCyclicUtil(node)) return true
  //   false
  // }




  val router_nodes = Seq.tabulate(nNodes) { i => Module(new Router(RouterParams(
    i,
    inParams(i),
    outParams(i),
    if (inputNodes.contains(i)) Seq(ChannelParams(-1, i, Seq.fill(nPrios) { VirtualChannelParams(-1) }, isInput=true)) else Nil,
    virtualLegalPathsFunction(i),
    virtualLegalInitsFunction(i),
    routingFunctions(i)
  ))) }

  val in_idxs = Array.fill(nNodes) { 0 }
  val out_idxs = Array.fill(nNodes) { 0 }

  router_nodes.zipWithIndex.map { case (dst,dstId) =>
    dst.io.in.map { in =>
      in <> router_nodes(in.cParams.srcId).io.out.filter(_.cParams.destId == dstId)(0)
    }
    val inputs = (io.in zip inputNodes).filter { case (_,i) => i == dstId }.map(_._1)
    (dst.io.terminal_in zip inputs).map { case (l,r) => l <> r }
  }




  router_nodes.foreach(i => dontTouch(i.io))
}
