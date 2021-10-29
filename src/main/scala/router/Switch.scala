package constellation.router

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

import constellation._

class SwitchBundle(val outParams: Seq[ChannelParams], val egressParams: Seq[ChannelParams])(implicit val p: Parameters) extends Bundle with HasRouterOutputParams{
  val flit = new Flit(allOutParams(0))
  val out_channel_oh = Vec(nAllOutputs, Bool())
  val out_virt_channel = UInt(log2Up(allOutParams.map(_.nVirtualChannels).max).W)
}

class Switch(val rP: RouterParams)(implicit val p: Parameters) extends Module with HasRouterParams {

  val io = IO(new Bundle {
    val in = Vec(nAllInputs, Input(Valid(new SwitchBundle(outParams, egressParams))))
    val out = MixedVec(allOutParams.map { u => Output(Valid(new Flit(u))) })
  })

  io.out.zipWithIndex.map { case (o,x) =>
    val oh = io.in.map(i => i.valid && i.bits.out_channel_oh(x))
    assert(PopCount(oh) <= 1.U)
    o.valid := oh.reduce(_||_)
    o.bits := Mux1H(oh, io.in.map(_.bits.flit))
    o.bits.virt_channel_id := Mux1H(oh, io.in.map(_.bits.out_virt_channel))
  }
}
