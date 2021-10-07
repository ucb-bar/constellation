package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}


class NoC(implicit val p: Parameters) extends Module with HasAstroNoCParams{
  val io = IO(new Bundle {

  })

  val inParams = (0 until nNodes).map { i => (0 until nNodes).map { j =>
    topologyFunction(j, i)
  }.flatten }
  val outParams = (0 until nNodes).map { i => (0 until nNodes).map { j =>
    topologyFunction(i, j)
  }.flatten }
  val vcAllocLegalPaths = (0 until nNodes).map { i => virtualLegalPathsFunction(i) }

  val router_nodes = Seq.tabulate(nNodes) { i => Module(new Router(RouterParams(i, inParams(i), outParams(i), vcAllocLegalPaths(i)))) }

  val in_idxs = Array.fill(nNodes) { 0 }
  val out_idxs = Array.fill(nNodes) { 0 }

  router_nodes.zipWithIndex.map { case (dst,j) =>
    router_nodes.zipWithIndex.map { case (src,i) =>
      val connected = topologyFunction(i, j).isDefined
      require(!(connected && i == j))
      if (connected) {
        dst.io.in(in_idxs(j)) <> src.io.out(out_idxs(i))
        in_idxs(j) = in_idxs(j) + 1
        out_idxs(i) = out_idxs(i) + 1
      }
    }
  }
}
