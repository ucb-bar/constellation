package constellation.router

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

import constellation.channel.{ChannelParams, IngressChannelParams, EgressChannelParams, Flit}

class SwitchBundle(val outParams: Seq[ChannelParams], val egressParams: Seq[EgressChannelParams])(implicit val p: Parameters) extends Bundle with HasRouterOutputParams{
  val flit = new Flit(allOutParams(0))
  val out_channel_oh = Vec(nAllOutputs, Bool())
  val out_virt_channel = UInt(log2Up(allOutParams.map(_.nVirtualChannels).max).W)
}

class Switch(
  val routerParams: RouterParams,
  val inParams: Seq[ChannelParams],
  val outParams: Seq[ChannelParams],
  val ingressParams: Seq[IngressChannelParams],
  val egressParams: Seq[EgressChannelParams]
)(implicit val p: Parameters) extends Module with HasRouterParams {

  val io = IO(new Bundle {
    val in = MixedVec(allInParams.map { u => Vec(u.destMultiplier,
      Input(Valid(new SwitchBundle(outParams, egressParams)))) })
    val out = MixedVec(allOutParams.map { u => Output(Valid(new Flit(u))) })
  })

  val in_flat = Wire(Vec(allInParams.map(_.destMultiplier).reduce(_+_),
    Valid(new SwitchBundle(outParams, egressParams))))
  var idx = 0
  io.in.foreach(_.foreach { i =>
    in_flat(idx) := i
    idx += 1
  })

  io.out.zipWithIndex.map { case (o,x) =>
    val oh = in_flat.map(i => i.valid && i.bits.out_channel_oh(x))
    assert(PopCount(oh) <= 1.U)
    o.valid := oh.reduce(_||_)
    o.bits := Mux1H(oh, in_flat.map(_.bits.flit))
    o.bits.virt_channel_id := Mux1H(oh, in_flat.map(_.bits.out_virt_channel))
  }
}
