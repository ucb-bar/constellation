package constellation.test

import chisel3._
import chisel3.util._

import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config.{Field, Parameters, Config}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import freechips.rocketchip.amba.axi4._

import scala.collection.immutable.ListMap
import constellation.noc.{NoCParams}
import constellation.protocol._

case class AXI4NoCTesterParams(
  nocParams: NoCParams = NoCParams(),
  inNodeMapping: Seq[Int] = Nil,
  outNodeMapping: Seq[Int] = Nil,
  txns: Int = 1000
)

case object AXI4NoCTesterKey extends Field[AXI4NoCTesterParams](AXI4NoCTesterParams())


class WithAXI4NoCTesterParams(p: AXI4NoCTesterParams) extends Config((site, here, up) => {
  case AXI4NoCTesterKey => p
})


class AXI4NoCTester(implicit p: Parameters) extends LazyModule {
  val tParams = p(AXI4NoCTesterKey)
  val txns = tParams.txns
  val inNodeMapping = ListMap(tParams.inNodeMapping.zipWithIndex.map { case (i,j) => s"[$j]" -> i }:_*)
  val outNodeMapping = ListMap(tParams.outNodeMapping.zipWithIndex.map { case (i,j) => s"[$j]" -> i }:_*)
  val nodeMapping = DiplomaticNetworkNodeMapping(
    inNodeMapping, outNodeMapping)
  val nSlaves = outNodeMapping.size
  val nMasters = inNodeMapping.size

  val noc = LazyModule(new AXI4NoC(AXI4NoCParams(nodeMapping, p(AXI4NoCTesterKey).nocParams.copy(nocName="test"))))
  val slaveSize = 0x1000
  val masterBandSize = slaveSize >> log2Ceil(nMasters)
  def filter(i: Int) = TLFilter.mSelectIntersect(AddressSet(i * masterBandSize, ~BigInt(slaveSize - masterBandSize)))

  val slaves = Seq.tabulate(nSlaves) { i => LazyModule(new AXI4RAM(AddressSet(slaveSize * i, slaveSize-1))) }
  slaves.foreach { s => (s.node
    := AXI4Fragmenter()
    := AXI4Buffer(BufferParams.flow)
    := AXI4Deinterleaver(4096)
    := noc.node) }

  val masters = Seq.fill(nMasters) { LazyModule(new TLFuzzer(txns, 32, nOrdered = Some(1))) }
  masters.zipWithIndex.foreach { case (m, i) => (noc.node
    := TLToAXI4()
    := TLFilter(filter(i))
    := TLRAMModel(s"${name} Master $i")
    := m.node) }

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val finished = Output(Bool())
    })
    io.finished := masters.map(_.module.io.finished).reduce(_||_)
  }

}

class AXI4TestHarness(implicit val p: Parameters) extends Module with HasSuccessIO {
  val tester = Module(LazyModule(new AXI4NoCTester).module)
  io.success := tester.io.finished

  // Dummy plusarg to avoid breaking verilator builds with emulator.cc
  val useless_plusarg = PlusArg("useless_plusarg", width=1)
  dontTouch(useless_plusarg)
  ElaborationArtefacts.add("plusArgs", PlusArgArtefacts.serialize_cHeader)
}

