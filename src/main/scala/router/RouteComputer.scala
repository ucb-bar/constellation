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
      val flow = req.bits.flow
      (0 until nAllOutputs).map { o =>
        if (o < nOutputs) {
          (0 until outParams(o).nVirtualChannels).map { outVId =>
            val table = allInParams(i).possibleFlows.toSeq.distinct.map { pI =>
              allInParams(i).channelRoutingInfos.map { cI =>
                val v = routingRelation(
                  cI,
                  outParams(o).channelRoutingInfos(outVId),
                  pI
                )
                ((cI.vc, pI.ingressId, pI.egressId, pI.dst), v)
              }
            }.flatten
            val trues = table.filter(_._2).map(_._1)
            val falses = table.filter(!_._2).map(_._1)

            val vcWidth = req.bits.src_virt_id.getWidth
            def tupleFlatten(t: (Int, Int, Int, Int)): UInt = {
              val l2 = (BigInt(t._1) << ingressIdBits) | t._2
              val l3 = (l2 << egressIdBits) | t._3
              ((l3 << log2Ceil(nNodes)) | t._4).U
            }

            resp.bits.vc_sel(o)(outVId) := (if (falses.size == 0) {
              true.B
            } else if (trues.size == 0) {
              false.B
            } else {
              val truthTable = TruthTable(
                (trues.map  { t => (BitPat(tupleFlatten(t)), BitPat(true.B)) } ++
                 falses.map { t => (BitPat(tupleFlatten(t)), BitPat(false.B)) }),
                BitPat("b?")
              )
              val addr = Cat(
                req.bits.src_virt_id,
                flow.ingress_id,
                flow.egress_id,
                flow.egress_dst_id)
              decoder(addr, truthTable)
            })
          }
        } else {
          resp.bits.vc_sel(o)(0) := false.B
        }
      }
    }
  }
}
