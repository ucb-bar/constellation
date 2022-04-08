package constellation

import chipsalliance.rocketchip.config.{Config, Parameters}
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import constellation.test._

class NoCChiselTester(implicit val p: Parameters) extends Module {
  val th = Module(new TestHarness)
  when (th.io.success) { stop() }
}

class TLNoCChiselTester(implicit val p: Parameters) extends Module {
  val th = Module(new TLTestHarness)
  when (th.io.success) { stop() }
}

abstract class BaseNoCTest(gen: Parameters => Module, configs: Seq[Config]) extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "NoC"

  configs.foreach { config =>
    it should s"pass test with config ${config.getClass.getName}" in {
      implicit val p: Parameters = config
      test(gen(p)).withAnnotations(Seq(VerilatorBackendAnnotation))
        .runUntilStop(timeout = 1000 * 1000)
    }
  }
}

abstract class NoCTest(configs: Seq[Config]) extends BaseNoCTest(p => new NoCChiselTester()(p), configs)
abstract class TLNoCTest(configs: Seq[Config]) extends BaseNoCTest(p => new TLNoCChiselTester()(p), configs)



// these tests allow you to run an infividual config
class NoCTest00 extends NoCTest(Seq(new TestConfig00))
class NoCTest01 extends NoCTest(Seq(new TestConfig01))
class NoCTest02 extends NoCTest(Seq(new TestConfig02))
class NoCTest03 extends NoCTest(Seq(new TestConfig03))
class NoCTest04 extends NoCTest(Seq(new TestConfig04))
class NoCTest05 extends NoCTest(Seq(new TestConfig05))
class NoCTest06 extends NoCTest(Seq(new TestConfig06))
class NoCTest07 extends NoCTest(Seq(new TestConfig07))
class NoCTest08 extends NoCTest(Seq(new TestConfig08))
class NoCTest09 extends NoCTest(Seq(new TestConfig09))
class NoCTest10 extends NoCTest(Seq(new TestConfig10))
class NoCTest11 extends NoCTest(Seq(new TestConfig11))
class NoCTest12 extends NoCTest(Seq(new TestConfig12))
class NoCTest13 extends NoCTest(Seq(new TestConfig13))
class NoCTest14 extends NoCTest(Seq(new TestConfig14))
class NoCTest15 extends NoCTest(Seq(new TestConfig15))
class NoCTest16 extends NoCTest(Seq(new TestConfig16))
class NoCTest17 extends NoCTest(Seq(new TestConfig17))
class NoCTest18 extends NoCTest(Seq(new TestConfig18))
class NoCTest19 extends NoCTest(Seq(new TestConfig19))
class NoCTest20 extends NoCTest(Seq(new TestConfig20))
class NoCTest21 extends NoCTest(Seq(new TestConfig21))
class NoCTest22 extends NoCTest(Seq(new TestConfig22))
class NoCTest23 extends NoCTest(Seq(new TestConfig23))
class NoCTest24 extends NoCTest(Seq(new TestConfig24))
class NoCTest25 extends NoCTest(Seq(new TestConfig25))
class NoCTest26 extends NoCTest(Seq(new TestConfig26))
class NoCTest27 extends NoCTest(Seq(new TestConfig27))
class NoCTest28 extends NoCTest(Seq(new TestConfig28))
class NoCTest29 extends NoCTest(Seq(new TestConfig29))
class NoCTest30 extends NoCTest(Seq(new TestConfig30))
class NoCTest31 extends NoCTest(Seq(new TestConfig31))
class NoCTest32 extends NoCTest(Seq(new TestConfig32))
class NoCTest33 extends NoCTest(Seq(new TestConfig33))
class NoCTest34 extends NoCTest(Seq(new TestConfig34))
class NoCTest35 extends NoCTest(Seq(new TestConfig35))
class NoCTest36 extends NoCTest(Seq(new TestConfig36))
class NoCTest37 extends NoCTest(Seq(new TestConfig37))
class NoCTest38 extends NoCTest(Seq(new TestConfig38))
class NoCTest39 extends NoCTest(Seq(new TestConfig39))
class NoCTest40 extends NoCTest(Seq(new TestConfig40))
class NoCTest41 extends NoCTest(Seq(new TestConfig41))
class NoCTest42 extends NoCTest(Seq(new TestConfig42))
class NoCTest43 extends NoCTest(Seq(new TestConfig43))
class NoCTest44 extends NoCTest(Seq(new TestConfig44))
class NoCTest45 extends NoCTest(Seq(new TestConfig45))
class NoCTest46 extends NoCTest(Seq(new TestConfig46))
class NoCTest47 extends NoCTest(Seq(new TestConfig47))
class NoCTest48 extends NoCTest(Seq(new TestConfig48))
class NoCTest49 extends NoCTest(Seq(new TestConfig49))
class NoCTest50 extends NoCTest(Seq(new TestConfig50))
class NoCTest51 extends NoCTest(Seq(new TestConfig51))
class NoCTest52 extends NoCTest(Seq(new TestConfig52))
class NoCTest53 extends NoCTest(Seq(new TestConfig53))
class NoCTest54 extends NoCTest(Seq(new TestConfig54))
class NoCTest55 extends NoCTest(Seq(new TestConfig55))
class NoCTest56 extends NoCTest(Seq(new TestConfig56))
class NoCTest57 extends NoCTest(Seq(new TestConfig57))
class NoCTest58 extends NoCTest(Seq(new TestConfig58))
class NoCTest59 extends NoCTest(Seq(new TestConfig59))
class NoCTest60 extends NoCTest(Seq(new TestConfig60))

class NoCTestTL00 extends TLNoCTest(Seq(new TLTestConfig00))
class NoCTestTL01 extends TLNoCTest(Seq(new TLTestConfig01))
class NoCTestTL02 extends TLNoCTest(Seq(new TLTestConfig02))
class NoCTestTL03 extends TLNoCTest(Seq(new TLTestConfig03))
class NoCTestTL04 extends TLNoCTest(Seq(new TLTestConfig04))
