package constellation.test

import chisel3._
import chisel3.util._
import chisel3.util.random.LFSR

import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config.{Field, Parameters, Config}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

import constellation.noc.{NoCKey, HasNoCParams, NoC}
import constellation.channel._
import constellation.rc.{TLNoC, TLNoCParams}
import constellation.router.{HasRouterCtrlConsts}


case class TLNoCTesterParams(
  inNodeMapping: Seq[Int],
  outNodeMapping: Seq[Int],
  ctrlSourceNode: Int = 0,
  txns: Int = 1000
)

case object TLNoCTesterKey extends Field[TLNoCTesterParams](TLNoCTesterParams(Nil, Nil))

class WithTLNoCTesterParams(p: TLNoCTesterParams) extends Config((site, here, up) => {
  case TLNoCTesterKey => p
})

class TLNoCTester(implicit p: Parameters) extends LazyModule {
  val tParams = p(TLNoCTesterKey)
  val txns = tParams.txns
  val inNodeMapping = tParams.inNodeMapping
  val outNodeMapping = tParams.outNodeMapping
  val nManagers = outNodeMapping.size
  val nClients = inNodeMapping.size
  val noc = LazyModule(new TLNoC(TLNoCParams("test", inNodeMapping, outNodeMapping, Some(p(NoCKey)))))

  val fuzzers = (0 until nClients) map { n =>
    val fuzz = LazyModule(new TLFuzzer(txns))
    noc.node := TLDelayer(0.1) := fuzz.node
    fuzz
  }

  (0 until nManagers) foreach { n =>
    val ram = LazyModule(new TLRAM(AddressSet(0x0+0x400*n, 0x3ff)))
    DisableMonitors { implicit p => ram.node := TLFragmenter(4, 256) } := TLDelayer(0.1) := noc.node
  }

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val finished = Output(Bool())
    })
    io.finished := fuzzers.map(_.module.io.finished).reduce(_&&_)
  }
}

class TLTestHarness(implicit val p: Parameters) extends Module {
  val io = IO(new Bundle {
    val success = Output(Bool())
  })
  val tester = Module(LazyModule(new TLNoCTester).module)
  io.success := tester.io.finished

  // Dummy plusarg to avoid breaking verilator builds with emulator.cc
  val useless_plusarg = PlusArg("useless_plusarg", width=1)
  dontTouch(useless_plusarg)
  ElaborationArtefacts.add("plusArgs", PlusArgArtefacts.serialize_cHeader)
}
