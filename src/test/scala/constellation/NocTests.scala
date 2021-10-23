package constellation

import chipsalliance.rocketchip.config.Parameters
import chiseltest._
import chiseltest.internal.CachingAnnotation
import org.scalatest.ParallelTestExecution
import org.scalatest.flatspec.AnyFlatSpec

class NocTests extends AnyFlatSpec with ChiselScalatestTester with ParallelTestExecution {

  val annos = Seq(VerilatorBackendAnnotation, CachingAnnotation)

  val configs = Seq(
    new TestConfig00,
    new TestConfig01,
    new TestConfig02,
    new TestConfig03,
    new TestConfig04,
    new TestConfig05,
    new TestConfig06,
    new TestConfig07,
    new TestConfig08,
    new TestConfig09,
    new TestConfig10,
    new TestConfig11,
    new TestConfig12,
    new TestConfig13,
    new TestConfig14,
    new TestConfig15,
    new TestConfig16,
    new TestConfig17,
    new TestConfig18,
    new TestConfig19,
    new TestConfig20,
    new TestConfig21,
    new TestConfig22,
    new TestConfig23,
    new TestConfig24,
    new TestConfig25,
    new TestConfig26,
    new TestConfig27,
    new TestConfig28,
    new TestConfig29,
    new TestConfig30,
    new TestConfig31,
    new TestConfig32,
    new TestConfig33,
    new TestConfig34,
    new TestConfig35,
  )

  configs.foreach { conf =>
    it should s"execute the TestHarness with ${conf.getClass.getName}" in {
      implicit val p: Parameters = conf
      test(new ChiselTester).withAnnotations(annos).runUntilStop(timeout = 1000 * 1000)
    }
  }
}
