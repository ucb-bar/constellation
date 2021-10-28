package constellation

import chipsalliance.rocketchip.config.{Config, Parameters}
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._

class ChiselTester(implicit val p: Parameters) extends Module {
  val noc = Module(new NoC)
  val noc_tester = Module(new NoCTester(noc.inputParams, noc.outputParams))
  noc.io.in <> noc_tester.io.to_noc
  noc_tester.io.from_noc <> noc.io.out
  when(noc_tester.io.success) { stop() }
}

abstract class NocTest(configs: Seq[Config]) extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "NoC"

  configs.foreach { config =>
    it should s"pass test with config ${config.getClass.getName}" in {
      implicit val p: Parameters = config
      test(new ChiselTester).withAnnotations(Seq(VerilatorBackendAnnotation))
        .runUntilStop(timeout = 1000 * 1000)
    }
  }
}



// these tests allow you to run an infividual config
class NocTest00 extends NocTest(Seq(new TestConfig00))
class NocTest01 extends NocTest(Seq(new TestConfig01))
class NocTest02 extends NocTest(Seq(new TestConfig02))
class NocTest03 extends NocTest(Seq(new TestConfig03))
class NocTest04 extends NocTest(Seq(new TestConfig04))
class NocTest05 extends NocTest(Seq(new TestConfig05))
class NocTest06 extends NocTest(Seq(new TestConfig06))
class NocTest07 extends NocTest(Seq(new TestConfig07))
class NocTest08 extends NocTest(Seq(new TestConfig08))
class NocTest09 extends NocTest(Seq(new TestConfig09))
class NocTest10 extends NocTest(Seq(new TestConfig10))
class NocTest11 extends NocTest(Seq(new TestConfig11))
class NocTest12 extends NocTest(Seq(new TestConfig12))
class NocTest13 extends NocTest(Seq(new TestConfig13))
class NocTest14 extends NocTest(Seq(new TestConfig14))
class NocTest15 extends NocTest(Seq(new TestConfig15))
class NocTest16 extends NocTest(Seq(new TestConfig16))
class NocTest17 extends NocTest(Seq(new TestConfig17))
class NocTest18 extends NocTest(Seq(new TestConfig18))
class NocTest19 extends NocTest(Seq(new TestConfig19))
class NocTest20 extends NocTest(Seq(new TestConfig20))
class NocTest21 extends NocTest(Seq(new TestConfig21))
class NocTest22 extends NocTest(Seq(new TestConfig22))
class NocTest23 extends NocTest(Seq(new TestConfig23))
class NocTest24 extends NocTest(Seq(new TestConfig24))
class NocTest25 extends NocTest(Seq(new TestConfig25))
class NocTest26 extends NocTest(Seq(new TestConfig26))
class NocTest27 extends NocTest(Seq(new TestConfig27))
class NocTest28 extends NocTest(Seq(new TestConfig28))
class NocTest29 extends NocTest(Seq(new TestConfig29))
class NocTest30 extends NocTest(Seq(new TestConfig30))
class NocTest31 extends NocTest(Seq(new TestConfig31))
class NocTest32 extends NocTest(Seq(new TestConfig32))
class NocTest33 extends NocTest(Seq(new TestConfig33))
class NocTest34 extends NocTest(Seq(new TestConfig34))
class NocTest35 extends NocTest(Seq(new TestConfig35))
class NocTest36 extends NocTest(Seq(new TestConfig36))
class NocTest37 extends NocTest(Seq(new TestConfig37))
class NocTest38 extends NocTest(Seq(new TestConfig38))

