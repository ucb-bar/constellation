package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

class SwitchBundle(nOutputs: Int)(implicit val p: Parameters) extends Bundle {
  val flit = new Flit
  val out_channel = UInt(log2Ceil(nOutputs).W)
}

class Switch(nInputs: Int, nOutputs: Int)(implicit val p: Parameters) extends Module with HasAstroNoCParams {

  val io = IO(new Bundle {
    val in = Vec(nInputs, Input(Valid(new SwitchBundle(nOutputs))))
    val out = Vec(nOutputs, Output(Valid(new Flit)))
  })
}
