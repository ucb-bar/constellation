package constellation.soc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util._

import constellation.noc.{NoCParams}
import constellation.protocol._

import scala.collection.immutable.{ListMap}

case class ConstellationSystemBusParams(
  sbusParams: SystemBusParams,
  tlNoCParams: TLNoCParams,
  useGlobalNoC: Boolean = false
) extends TLBusWrapperInstantiationLike {
  def instantiate(context: HasTileLinkLocations, loc: Location[TLBusWrapper]
  )(implicit p: Parameters): ConstellationSystemBus = {
    val constellation = LazyModule(new ConstellationSystemBus(
      sbusParams, tlNoCParams, loc.name, useGlobalNoC, context))

    constellation.suggestName(loc.name)
    context.tlBusWrapperLocationMap += (loc -> constellation)
    constellation
  }
}

class ConstellationSystemBus(
  sbus_params: SystemBusParams, noc_params: TLNoCParams, name: String,
  useGlobalNoC: Boolean, context: HasTileLinkLocations
)(implicit p: Parameters) extends TLBusWrapper(sbus_params, name) {
  private val replicator = sbus_params.replication.map(r => LazyModule(new RegionReplicator(r)))
  val prefixNode = replicator.map { r =>
    r.prefix := addressPrefixNexusNode
    addressPrefixNexusNode
  }

  private val system_bus_noc = if (useGlobalNoC) {
    context.asInstanceOf[CanHaveGlobalNoC].globalNoCDomain {
      LazyModule(new TLGlobalNoC(noc_params, name))
    }
  } else {
    LazyModule(new TLNoC(noc_params, name))
  }
  val inwardNode: TLInwardNode = (system_bus_noc.node :=* TLFIFOFixer(TLFIFOFixer.allVolatile)
    :=* replicator.map(_.node).getOrElse(TLTempNode()))
  val outwardNode: TLOutwardNode = system_bus_noc.node
  def busView: TLEdge = system_bus_noc.node.edges.in.head

  val builtInDevices: BuiltInDevices = BuiltInDevices.attach(sbus_params, outwardNode)
}

case class ConstellationMemoryBusParams(
  mbusParams: MemoryBusParams,
  tlNoCParams: TLNoCParams,
  useGlobalNoC: Boolean = false
) extends TLBusWrapperInstantiationLike {
  def instantiate(context: HasTileLinkLocations, loc: Location[TLBusWrapper]
  )(implicit p: Parameters): ConstellationMemoryBus = {
    val constellation = LazyModule(new ConstellationMemoryBus(
      mbusParams, tlNoCParams, loc.name, useGlobalNoC, context))

    constellation.suggestName(loc.name)
    context.tlBusWrapperLocationMap += (loc -> constellation)
    constellation
  }
}

class ConstellationMemoryBus(mbus_params: MemoryBusParams, noc_params: TLNoCParams, name: String, useGlobalNoC: Boolean, context: HasTileLinkLocations)
  (implicit p: Parameters) extends TLBusWrapper(mbus_params, name) {
  private val replicator = mbus_params.replication.map(r => LazyModule(new RegionReplicator(r)))
  val prefixNode = replicator.map { r =>
    r.prefix := addressPrefixNexusNode
    addressPrefixNexusNode
  }

  private val memory_bus_noc = if (useGlobalNoC) {
    context.asInstanceOf[CanHaveGlobalNoC].globalNoCDomain {
      LazyModule(new TLGlobalNoC(noc_params, name))
    }
  } else {
    LazyModule(new TLNoC(noc_params, name))
  }
  val inwardNode: TLInwardNode =
    replicator.map(memory_bus_noc.node :*=* TLFIFOFixer(TLFIFOFixer.all) :*=* _.node)
        .getOrElse(memory_bus_noc.node :*=* TLFIFOFixer(TLFIFOFixer.all))

  val outwardNode: TLOutwardNode = ProbePicker() :*= memory_bus_noc.node
  def busView: TLEdge = memory_bus_noc.node.edges.in.head

  val builtInDevices: BuiltInDevices = BuiltInDevices.attach(mbus_params, outwardNode)
}

case class ConstellationPeripheryBusParams(
  pbusParams: PeripheryBusParams,
  tlNoCParams: TLNoCParams,
  useGlobalNoC: Boolean = false
) extends TLBusWrapperInstantiationLike {
  def instantiate(context: HasTileLinkLocations, loc: Location[TLBusWrapper]
  )(implicit p: Parameters): ConstellationPeripheryBus = {
    val constellation = LazyModule(new ConstellationPeripheryBus(pbusParams, tlNoCParams, loc.name, useGlobalNoC, context))

    constellation.suggestName(loc.name)
    context.tlBusWrapperLocationMap += (loc -> constellation)
    constellation
  }
}

class ConstellationPeripheryBus(pbus_params: PeripheryBusParams, noc_params: TLNoCParams, name: String, useGlobalNoC: Boolean, context: HasTileLinkLocations)
  (implicit p: Parameters) extends TLBusWrapper(pbus_params, name) {

  private val replicator = pbus_params.replication.map(r => LazyModule(new RegionReplicator(r)))
  val prefixNode = replicator.map { r =>
    r.prefix := addressPrefixNexusNode
    addressPrefixNexusNode
  }

  def genNoC(): TLNoCLike = if (useGlobalNoC) {
    context.asInstanceOf[CanHaveGlobalNoC].globalNoCDomain {
      val noc = LazyModule(new TLGlobalNoC(noc_params, name))
      noc
    }
  } else {
    val noc = LazyModule(new TLNoC(noc_params, name))
    noc
  }

  private val fixer = LazyModule(new TLFIFOFixer(TLFIFOFixer.all))
  private val node: TLNode = pbus_params.atomics.map { pa =>
    val in_xbar = LazyModule(new TLXbar)
    val out_noc = genNoC()
    val fixer_node = replicator.map(fixer.node :*= _.node).getOrElse(fixer.node)
    (out_noc.node
      :*= fixer_node
      :*= TLBuffer(pa.buffer)
      :*= (pa.widenBytes.filter(_ > beatBytes).map { w =>
          TLWidthWidget(w) :*= TLAtomicAutomata(arithmetic = pa.arithmetic)
        } .getOrElse { TLAtomicAutomata(arithmetic = pa.arithmetic) })
      :*= in_xbar.node)
  } .getOrElse {
    val noc = genNoC()
    noc.node :*= fixer.node
  }

  def inwardNode: TLInwardNode = node
  def outwardNode: TLOutwardNode = node
  def busView: TLEdge = fixer.node.edges.in.head

  val builtInDevices: BuiltInDevices = BuiltInDevices.attach(pbus_params, outwardNode)

}

