package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}


class InputBuffer(size: Int)(implicit val p: Parameters) extends Module with HasAstroNoCParams {
  val io = IO(new Bundle {
    val in = Flipped(Valid(new Flit))
    val head = Output(UInt(log2Ceil(size).W))
    val full = Output(Bool())

    val read_req = Input(Valid(UInt(log2Ceil(size).W)))
    val read_resp = Output(new Flit)
  })
}
