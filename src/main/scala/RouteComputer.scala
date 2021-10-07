package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}


class RouteComputerReq(val param: ChannelParams)(implicit val p: Parameters) extends Bundle with HasAstroNoCParams {
  val src_virt_id = UInt(log2Ceil(param.virtualChannelParams.size).W)
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
  io := DontCare

  // if (params.shareRouteComputer) {
  //   val route_arbiter = Module(new RRArbiter(new RouteComputerReq, nInputs))
  //   (route_arbiter.io.in zip io.req).map { case (l,r) => l <> r }
  //   route_arbiter.io.out
  //   route_arbiter.io.chosen
  // } else {
  //   (io.req zip io.resp) map { case (req, resp) =>
      
  //   }
  // }

}
