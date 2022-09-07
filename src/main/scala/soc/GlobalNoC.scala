package constellation.soc

import chisel3._
import chisel3.util._

import constellation.channel._
import constellation.noc._
import constellation.protocol._

import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.prci._

case class GlobalNoCParams(
  nocParams: NoCParams = NoCParams()
)

trait CanAttachToGlobalNoC {
  val protocolParams: ProtocolParams
  val io_global: Data
}

case object GlobalNoCKey extends Field[GlobalNoCParams](GlobalNoCParams())

class GlobalNoCDomain(implicit p: Parameters) extends ClockSinkDomain()(p) {
  InModuleBody {
    val interfaces = getChildren.map(_.module).collect {
      case a: CanAttachToGlobalNoC => a
    }.toSeq
    if (interfaces.size > 0) {
      val noc = Module(new ProtocolNoC(ProtocolNoCParams(
        p(GlobalNoCKey).nocParams,
        interfaces.map(_.protocolParams)
      )))

      (interfaces zip noc.io.protocol).foreach { case (l,r) =>
        l.io_global <> r
      }
    }
  }
}

trait CanHaveGlobalNoC { this: BaseSubsystem =>
  lazy val globalNoCDomain = LazyModule(new GlobalNoCDomain)
  globalNoCDomain.clockNode := locateTLBusWrapper(SBUS).fixedClockNode
}
