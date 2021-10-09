package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}


class RouteComputerReq(val param: ChannelParams)(implicit val p: Parameters) extends Bundle with HasAstroNoCParams {
  val src_virt_id = UInt(log2Ceil(param.virtualChannelParams.size).W)
  val prio = UInt(prioBits.W)
  val dest_id = UInt(idBits.W)
}

class RouteComputerResp(val param: ChannelParams, val nOutputs: Int)(implicit val p: Parameters) extends Bundle with HasAstroNoCParams {
  val src_virt_id = UInt(log2Ceil(param.virtualChannelParams.size).W)
  val out_channels = UInt(nOutputs.W)
}



class RouteComputer(val rParams: RouterParams)(implicit val p: Parameters) extends Module with HasRouterParams {
  val io = IO(new Bundle {
    val req = MixedVec(inParams.map { u => Flipped(Decoupled(new RouteComputerReq(u))) })
    val resp = MixedVec(inParams.map { u => Valid(new RouteComputerResp(u, nOutputs)) })
  })
  val routingMatrix = outParams.map { u =>
    VecInit((0 until nNodes).map { d => rParams.routingFunction(d, u.destId).B })
  }

  (io.req zip io.resp) map { case (req, resp) =>
    req.ready := true.B
    resp.valid := req.valid
    resp.bits.src_virt_id := req.bits.src_virt_id
    resp.bits.out_channels := VecInit(routingMatrix.map(_(req.bits.dest_id))).asUInt
  }
}
