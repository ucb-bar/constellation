package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

class SwitchBundle(val nOutputs: Int)(implicit val p: Parameters) extends Bundle {
  val flit = new Flit
  val out_channel = UInt(log2Up(nOutputs).W)
}

class Switch(nInputs: Int, nOutputs: Int)(implicit val p: Parameters) extends Module with HasAstroNoCParams {

  val io = IO(new Bundle {
    val in = Vec(nInputs, Input(Valid(new SwitchBundle(nOutputs))))
    val out = Vec(nOutputs, Output(Valid(new Flit)))
  })

  io.out.zipWithIndex.map { case (o,x) =>
    val oh = io.in.map(i => i.valid && i.bits.out_channel === x.U)
    assert(PopCount(oh) <= 1.U)
    o.valid := oh.reduce(_||_)
    o.bits := Mux1H(oh, io.in.map(_.bits.flit))
  }
}
