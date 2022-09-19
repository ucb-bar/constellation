package constellation.router

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.{TruthTable, decoder}

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._
import freechips.rocketchip.rocket.DecodeLogic

import constellation.channel._
import constellation.routing.{FlowRoutingBundle, FlowRoutingInfo}
import constellation.noc.{HasNoCParams}

class RouteComputerReq(implicit val p: Parameters) extends Bundle with HasNoCParams {
  val src_virt_id = UInt(virtualChannelBits.W)
  val flow = new FlowRoutingBundle
}

class RouteComputerResp(
  val outParams: Seq[ChannelParams],
  val egressParams: Seq[EgressChannelParams])(implicit val p: Parameters) extends Bundle
    with HasRouterOutputParams {
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
    val req = MixedVec(allInParams.map { u => Flipped(Decoupled(new RouteComputerReq)) })
    val resp = MixedVec(allInParams.map { u => Output(new RouteComputerResp(outParams, egressParams)) })
  })

  (io.req zip io.resp).zipWithIndex.map { case ((req, resp), i) =>
    req.ready := true.B
    if (outParams.size == 0) {
      assert(!req.valid)
      resp.vc_sel := DontCare
    } else {

      def toUInt(t: (Int, FlowRoutingInfo)): UInt = {
        val l2 = (BigInt(t._1) << req.bits.flow.vnet_id        .getWidth) | t._2.vNetId
        val l3 = (          l2 << req.bits.flow.ingress_node   .getWidth) | t._2.ingressNode
        val l4 = (          l3 << req.bits.flow.ingress_node_id.getWidth) | t._2.ingressNodeId
        val l5 = (          l4 << req.bits.flow.egress_node    .getWidth) | t._2.egressNode
        val l6 = (          l5 << req.bits.flow.egress_node_id .getWidth) | t._2.egressNodeId
        l6.U(req.bits.getWidth.W)
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
          ((cI.vc, pI), row)
        }
      }.flatten
      val addr = req.bits.asUInt
      val width = outParams.map(_.nVirtualChannels).reduce(_+_)
      val decoded = if (table.size > 0) {
        val truthTable = TruthTable(
          table.map { e => (BitPat(toUInt(e._1)), BitPat(e._2)) },
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
            resp.vc_sel(o)(outVId) := decoded(idx)
            idx += 1
          }
        } else {
          resp.vc_sel(o)(0) := false.B
        }
      }
    }
  }
}
