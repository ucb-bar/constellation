package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}


class RouteComputerReq(implicit val p: Parameters) extends Bundle with HasAstroNoCParams {
  val src_virt_id = UInt(virtChannelBits.W)
  val dest_id = UInt(idBits.W)
}

class RouteComputerResp(nOutputs: Int)(implicit val p: Parameters) extends Bundle with HasAstroNoCParams {
  val src_virt_id = UInt(virtChannelBits.W)
  val out_channels = UInt(nOutputs.W)
}



class RouteComputer(inParams: Seq[ChannelParams], outParams: Seq[ChannelParams], id: Int)(implicit val p: Parameters) extends Module with HasAstroNoCParams {
  val nInputs = inParams.size
  val nOutputs = outParams.size
  val io = IO(new Bundle {
    val req = Vec(nInputs, Flipped(Decoupled(new RouteComputerReq)))
    val resp = Vec(nInputs, Valid(new RouteComputerResp(nOutputs)))
  })
  if (params.shareRouteComputer) {
    val route_arbiter = Module(new RRArbiter(new RouteComputerReq, nInputs))
    (route_arbiter.io.in zip io.req).map { case (l,r) => l <> r }
    route_arbiter.io.out
    route_arbiter.io.chosen
  } else {
    (io.req zip io.resp) map { case (req, resp) =>
      
    }
  }

}
