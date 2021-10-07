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
  val cParams = Seq.fill(2) { ChannelParams(virtualChannelParams=Seq.fill(2) { VirtualChannelParams(bufferSize=3) })}
  val rParams = RouterParams(0, cParams, cParams, (inChannel: Int, inVirtChannel: Int, outChannel: Int, outVirtChannel: Int) => (prio: UInt) => false.B)
  val nocTest = Module(new Router(rParams))

  nocTest.io := DontCare

  io.success := true.B
}
