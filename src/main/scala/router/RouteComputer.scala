package constellation.router

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

import constellation._

class RouteComputerReq(val cParam: ChannelParams)(implicit val p: Parameters) extends Bundle with HasChannelParams {
  val src_virt_id = UInt(virtualChannelBits.W)
  val src_user = UInt(userBits.W)
  val dest_id = UInt(nodeIdBits.W)
}

class RouteComputerResp(val cParam: ChannelParams,
  val outParams: Seq[ChannelParams],
  val terminalOutParams: Seq[ChannelParams])(implicit val p: Parameters) extends Bundle
    with HasChannelParams with HasRouterOutputParams {

  val src_virt_id = UInt(virtualChannelBits.W)
  val vc_sel = MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool()) })
}



class RouteComputer(val rParams: RouterParams)(implicit val p: Parameters) extends Module with HasRouterParams {
  val io = IO(new Bundle {
    val req = MixedVec(allInParams.map { u => Flipped(Decoupled(new RouteComputerReq(u))) })
    val resp = MixedVec(allInParams.map { u => Valid(new RouteComputerResp(u, outParams, terminalOutParams)) })
  })

  (io.req zip io.resp).zipWithIndex.map { case ((req, resp), i) =>
    req.ready := true.B
    resp.valid := req.valid
    resp.bits.src_virt_id := req.bits.src_virt_id
    if (outParams.size == 0) {
      assert(!req.valid)
      resp.bits.vc_sel := DontCare
    } else {
      (0 until nAllOutputs).map { o =>
        if (o < nOutputs) {
          (0 until outParams(o).nVirtualChannels).map { outVId =>
            val table = Seq.tabulate(allInParams(i).nVirtualChannels, 1 << userBits, nNodes) {
              case (inVId, user, dest) => {
                val v = rParams.masterAllocTable(
                  allInParams(i).srcId, inVId,
                  outParams(o).destId, outVId,
                  dest, user)
                ((((inVId << userBits) + user) << nodeIdBits) + dest, v)
              }
            }.flatten.flatten

            val addr = Cat(req.bits.src_virt_id, req.bits.src_user, req.bits.dest_id)
            resp.bits.vc_sel(o)(outVId) := table.filter(_._2).map(_._1.U === addr).orR
          }
        } else {
          resp.bits.vc_sel(o)(0) := false.B
        }
      }
    }
  }
}
