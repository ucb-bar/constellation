package constellation

import chipsalliance.rocketchip.config.{Config, Parameters}
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec


abstract class NocTest(config: Config) extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "NoC"
  it should s"pass test with config ${config.getClass.getName}" in {
    implicit val p: Parameters = config
    test(new ChiselTester).withAnnotations(Seq(VerilatorBackendAnnotation))
      .runUntilStop(timeout = 1000 * 1000)
  }
}

class NocTest00 extends NocTest(new TestConfig00)
class NocTest01 extends NocTest(new TestConfig01)
class NocTest02 extends NocTest(new TestConfig02)
class NocTest03 extends NocTest(new TestConfig03)
class NocTest04 extends NocTest(new TestConfig04)
class NocTest05 extends NocTest(new TestConfig05)
class NocTest06 extends NocTest(new TestConfig06)
class NocTest07 extends NocTest(new TestConfig07)
class NocTest08 extends NocTest(new TestConfig08)
class NocTest09 extends NocTest(new TestConfig09)
class NocTest10 extends NocTest(new TestConfig10)
class NocTest11 extends NocTest(new TestConfig11)
class NocTest12 extends NocTest(new TestConfig12)
class NocTest13 extends NocTest(new TestConfig13)
class NocTest14 extends NocTest(new TestConfig14)
class NocTest15 extends NocTest(new TestConfig15)
class NocTest16 extends NocTest(new TestConfig16)
class NocTest17 extends NocTest(new TestConfig17)
class NocTest18 extends NocTest(new TestConfig18)
class NocTest19 extends NocTest(new TestConfig19)
class NocTest20 extends NocTest(new TestConfig20)
class NocTest21 extends NocTest(new TestConfig21)
class NocTest22 extends NocTest(new TestConfig22)
class NocTest23 extends NocTest(new TestConfig23)
class NocTest24 extends NocTest(new TestConfig24)
class NocTest25 extends NocTest(new TestConfig25)
class NocTest26 extends NocTest(new TestConfig26)
class NocTest27 extends NocTest(new TestConfig27)
class NocTest28 extends NocTest(new TestConfig28)
class NocTest29 extends NocTest(new TestConfig29)
class NocTest30 extends NocTest(new TestConfig30)
class NocTest31 extends NocTest(new TestConfig31)
class NocTest32 extends NocTest(new TestConfig32)
class NocTest33 extends NocTest(new TestConfig33)
class NocTest34 extends NocTest(new TestConfig34)
class NocTest35 extends NocTest(new TestConfig35)

