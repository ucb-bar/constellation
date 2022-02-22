package constellation.rc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util._

import constellation.{NoC, NoCKey}
import constellation.channel.{TerminalChannel}

import scala.collection.mutable.Set

case object InstantiateGlobalNoCInterconnect extends Field[Boolean](false)
trait CanHaveGlobalNoCInterconnect { this: BaseSubsystem =>
  val noc = p(InstantiateGlobalNoCInterconnect).option(LazyModule(new NoC()(p.alterPartial({
    case NoCKey => p(NoCKey).copy(nocName="global")
  }))))

  var connected_ingresses = Set[Int]()
  def connectNoCIngress(i: Int): ModuleValue[TerminalChannel] = {
    require(!connected_ingresses(i) && i < noc.get.globalIngressParams.size)
    connected_ingresses += i
    val sink = this { BundleBridgeSink[TerminalChannel]() }
    val source = BundleBridgeSource[TerminalChannel](() => new TerminalChannel(noc.get.globalIngressParams(i)))
    sink := source
    this { InModuleBody { noc.get.module.io.ingress(i) <> sink.bundle } }
    InModuleBody { source.bundle }
  }
  var connected_egresses = Set[Int]()
  def connectNoCEgress(i: Int): ModuleValue[TerminalChannel] = {
    require(!connected_egresses(i) && i < noc.get.globalEgressParams.size)
    connected_egresses += i
    val sink = this { BundleBridgeSink[TerminalChannel]() }
    val source = BundleBridgeSource[TerminalChannel](() => Flipped(new TerminalChannel(noc.get.globalEgressParams(i))))
    sink := source
    this { InModuleBody { sink.bundle <> noc.get.module.io.egress(i) } }
    InModuleBody { source.bundle }
  }

  InModuleBody { require(connected_egresses.size == noc.get.globalEgressParams.size && connected_ingresses.size == noc.get.globalIngressParams.size) }
}

