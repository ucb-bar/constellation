package constellation.router

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.{TruthTable, decoder}

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._
import freechips.rocketchip.rocket.DecodeLogic

import constellation.channel._
import constellation.routing.{FlowRoutingBundle}
import constellation.noc.{HasNoCParams}

class RouteComputerReq(val cParam: BaseChannelParams)(implicit val p: Parameters) extends Bundle
    with HasChannelParams {
  val src_virt_id = UInt(virtualChannelBits.W)
  val flow = new FlowRoutingBundle
}

class RouteComputerResp(val cParam: BaseChannelParams,
  val outParams: Seq[ChannelParams],
  val egressParams: Seq[EgressChannelParams])(implicit val p: Parameters) extends Bundle
    with HasChannelParams with HasRouterOutputParams {

  val src_virt_id = UInt(virtualChannelBits.W)
  val vc_sel = MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool()) })
}



class RouteComputer(
  val routerParams: RouterParams,
  val inParams: Seq[ChannelParams],
  val outParams: Seq[ChannelParams],
  val ingressParams: Seq[IngressChannelParams],
  val egressParams: Seq[EgressChannelParams]
)(implicit val p: Parameters) extends Module
    with HasRouterParams
    with HasRouterInputParams
    with HasRouterOutputParams
    with HasNoCParams {
  val io = IO(new Bundle {
    val req = MixedVec(allInParams.map { u => Flipped(Decoupled(new RouteComputerReq(u))) })
    val resp = MixedVec(allInParams.map { u => Valid(new RouteComputerResp(u, outParams, egressParams)) })
  })

  (io.req zip io.resp).zipWithIndex.map { case ((req, resp), i) =>
    req.ready := true.B
    resp.valid := req.valid
    resp.bits.src_virt_id := req.bits.src_virt_id
    if (outParams.size == 0) {
      assert(!req.valid)
      resp.bits.vc_sel := DontCare
    } else {

      def tupleFlatten(t: (Int, Int, Int, Int)): UInt = {
        val l2 = (BigInt(t._1) << ingressIdBits) | t._2
        val l3 = (l2 << egressIdBits) | t._3
        val width = req.bits.src_virt_id.getWidth + ingressIdBits + egressIdBits + log2Ceil(nNodes)
        ((l3 << log2Ceil(nNodes)) | t._4).U(width.W)
      }

      val flow = req.bits.flow

      val table = allInParams(i).possibleFlows.toSeq.distinct.map { pI =>
        allInParams(i).channelRoutingInfos.map { cI =>
          var row: String = "b"
          (0 until nOutputs).foreach { o =>
            (0 until outParams(o).nVirtualChannels).foreach { outVId =>
              row = row + (if (routingRelation(cI, outParams(o).channelRoutingInfos(outVId), pI)) "1" else "0")
            }
          }
          ((cI.vc, pI.ingressId, pI.egressId, pI.dst), row)
        }
      }.flatten
      val addr = Cat(
        req.bits.src_virt_id,
        flow.ingress_id,
        flow.egress_id,
        flow.egress_dst_id)
      val width = outParams.map(_.nVirtualChannels).reduce(_+_)
      val decoded = if (table.size > 0) {
        val truthTable = TruthTable(
          table.map { e => (BitPat(tupleFlatten(e._1)), BitPat(e._2)) },
          BitPat("b" + "?" * width)
        )
        Reverse(decoder(addr, truthTable))
      } else {
        0.U(width.W)
      }
      var idx = 0

      (0 until nAllOutputs).foreach { o =>
        if (o < nOutputs) {
          (0 until outParams(o).nVirtualChannels).foreach { outVId =>
            resp.bits.vc_sel(o)(outVId) := decoded(idx)
            idx += 1
          }
        } else {
          resp.bits.vc_sel(o)(0) := false.B
        }
      }
    }
  }
}
