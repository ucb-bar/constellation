package constellation.router

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._
import freechips.rocketchip.rocket.DecodeLogic

import constellation.channel._
import constellation.routing.{FlowIdentifierBundle}
import constellation.noc.{HasNoCParams}

class RouteComputerReq(val cParam: BaseChannelParams)(implicit val p: Parameters) extends Bundle with HasChannelParams {
  val src_virt_id = UInt(virtualChannelBits.W)
  val flow = new FlowIdentifierBundle
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
)(implicit val p: Parameters) extends Module with HasRouterParams with HasNoCParams {
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
            val table = allInParams(i).possiblePackets.toSeq.distinct.map { pI =>
              allInParams(i).channelRoutingInfos.map { cI =>
                val v = routingRelation(
                  nodeId,
                  cI,
                  outParams(o).channelRoutingInfos(outVId),
                  pI
                )
                ((cI.vc, pI.ingressId, pI.egressId, pI.vNet), v)
              }
            }.flatten
            val trues = table.filter(_._2).map(_._1)
            val falses = table.filter(!_._2).map(_._1)
            val addr = (
              req.bits.src_virt_id,
              flow.ingress,
              flow.egress,
              flow.vnet
            )

            def eq(a: (Int, Int, Int, Int), b: (UInt, UInt, UInt, UInt)): Bool = {
              a._1.U === b._1 && a._2.U === b._2 && a._3.U === b._3 && a._4.U === b._4
            }

            resp.bits.vc_sel(o)(outVId) := (if (falses.size == 0) {
              true.B
            } else if (trues.size == 0) {
              false.B
            } else {
              // The Quine-McCluskey impl in rocketchip memory leaks sometimes here...
              //if (trues.size + falses.size >= 100) {
              if (trues.size > falses.size) {
                falses.map(t => !eq(t, addr)).andR
              } else {
                trues.map(t => eq(t, addr)).orR
              }
              // } else {
              //   println(s"DecodeLogic ${trues.size} ${falses.size}")
              //   val r = DecodeLogic(addr, trues, falses)
              //   println("Done DecodeLogic")
              //   r
              // }
            })
          }
        } else {
          resp.bits.vc_sel(o)(0) := false.B
        }
      }
    }
  }
}
