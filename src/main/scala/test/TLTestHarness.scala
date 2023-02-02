package constellation.test

import chisel3._
import chisel3.util._
import chisel3.util.random.LFSR

import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config.{Field, Parameters, Config}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

import constellation.noc.{NoCParams, HasNoCParams, NoC}
import constellation.channel._
import constellation.protocol.{TLNoC, TLNoCParams, DiplomaticNetworkNodeMapping}
import constellation.router.{HasRouterCtrlConsts}

import scala.collection.immutable.ListMap

case class TLNoCTesterParams(
  nocParams: NoCParams = NoCParams(),
  inNodeMapping: Seq[Int] = Nil,
  outNodeMapping: Seq[Int] = Nil,
  delay: Double = 0.0,
  txns: Int = 1000
)

case object TLNoCTesterKey extends Field[TLNoCTesterParams](TLNoCTesterParams())

class TLNoCTester(implicit p: Parameters) extends LazyModule {
  val tParams = p(TLNoCTesterKey)
  val txns = tParams.txns
  val inNodeMapping = ListMap(tParams.inNodeMapping.zipWithIndex.map { case (i,j) => s"[$j]" -> i }:_*)
  val outNodeMapping = ListMap(tParams.outNodeMapping.zipWithIndex.map { case (i,j) => s"[$j]" -> i }:_*)
  val nodeMapping = DiplomaticNetworkNodeMapping(
    inNodeMapping,
    outNodeMapping)
  val nManagers = outNodeMapping.size
  val nClients = inNodeMapping.size
  val noc = LazyModule(new TLNoC(TLNoCParams(nodeMapping, p(TLNoCTesterKey).nocParams.copy(nocName="test"))))

  val fuzzers = (0 until nClients) map { n =>
    val fuzz = LazyModule(new TLFuzzer(txns))
    noc.node := TLDelayer(tParams.delay) := fuzz.node
    fuzz
  }

  (0 until nManagers) foreach { n =>
    val ram = LazyModule(new TLRAM(AddressSet(0x0+0x400*n, 0x3ff)))
    DisableMonitors { implicit p => ram.node := TLFragmenter(4, 256) } := TLDelayer(tParams.delay) := noc.node
  }

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val finished = Output(Bool())
    })
    io.finished := fuzzers.map(_.module.io.finished).reduce(_&&_)
  }
}

class TLTestHarness(implicit val p: Parameters) extends Module with HasSuccessIO {
  val tester = Module(LazyModule(new TLNoCTester).module)
  io.success := tester.io.finished

  // Dummy plusarg to avoid breaking verilator builds with emulator.cc
  val useless_plusarg = PlusArg("useless_plusarg", width=1)
  dontTouch(useless_plusarg)
  ElaborationArtefacts.add("plusArgs", PlusArgArtefacts.serialize_cHeader)
}
