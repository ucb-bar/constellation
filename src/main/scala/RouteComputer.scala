package constellation

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._


class RouteComputerReq(val cParam: ChannelParams)(implicit val p: Parameters) extends Bundle with HasChannelParams {
  val src_virt_id = UInt(virtualChannelBits.W)
  val src_user = UInt(userBits.W)
  val dest_id = UInt(nodeIdBits.W)
}

class RouteComputerResp(val cParam: ChannelParams, val nOutputs: Int)(implicit val p: Parameters) extends Bundle with HasChannelParams {
  val src_virt_id = UInt(virtualChannelBits.W)
  val out_channels = UInt(nOutputs.W)
}



class RouteComputer(val rParams: RouterParams)(implicit val p: Parameters) extends Module with HasRouterParams {
  val io = IO(new Bundle {
    val req = MixedVec(allInParams.map { u => Flipped(Decoupled(new RouteComputerReq(u))) })
    val resp = MixedVec(allInParams.map { u => Valid(new RouteComputerResp(u, nOutputs)) })
  })

  (io.req zip io.resp).zipWithIndex.map { case ((req, resp), i) =>
    req.ready := true.B
    resp.valid := req.valid
    resp.bits.src_virt_id := req.bits.src_virt_id
    if (outParams.size == 0) {
      assert(!req.valid)
      resp.bits.out_channels := 0.U
    } else {
      val out = Wire(Vec(nOutputs, Bool()))
      (0 until nOutputs).map { o =>
        val table = Seq.tabulate(1 << userBits, nNodes) { case (user, dest) =>
          ((user << nodeIdBits) + dest,
            routingFunction(allInParams(i).srcId, dest, allOutParams(o).destId, user))
        }.flatten

        val addr = Cat(req.bits.src_user, req.bits.dest_id)
        out(o) := table.filter(_._2).map(_._1.U === addr).orR
      }
      resp.bits.out_channels := out.asUInt
    }
  }
}
