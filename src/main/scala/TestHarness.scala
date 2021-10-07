package astronoc

import chisel3._
import chisel3.util._

import scala.collection.mutable.{ArrayBuffer, LinkedHashMap}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImpLike, LazyModuleImp}
import freechips.rocketchip.config.{Field, Parameters}

import astronoc._

class TestHarness(implicit val p: Parameters) extends Module {
  val io = IO(new Bundle {
    val success = Output(Bool())
  })

  val noc = Module(new NoC)

  io.success := true.B
}
