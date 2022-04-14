package constellation.rc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util._

import constellation.noc.{NoCParams}

import scala.collection.immutable.{ListMap}

case class ConstellationTLNetworkNodeMapping(
  inNodeMapping: ListMap[String, Int] = ListMap[String, Int](),
  outNodeMapping: ListMap[String, (Int, Boolean)] = ListMap[String, (Int, Boolean)](),
)

case class ConstellationTLNetworkNodeMappingKey(where: Location[TLBusWrapper]) extends Field[ConstellationTLNetworkNodeMapping](
  ConstellationTLNetworkNodeMapping()
)

case class ConstellationSystemBusParams(
  params: SystemBusParams,
  privateNoC: Option[NoCParams]
) extends TLBusWrapperInstantiationLike {
  def instantiate(context: HasTileLinkLocations, loc: Location[TLBusWrapper])(implicit p: Parameters): ConstellationSystemBus = {
    val base_noc_params = TLNoCParams("sbus", p(ConstellationTLNetworkNodeMappingKey(loc)), privateNoC)
    val noc_params = context match {
      case c: CanHaveGlobalTLInterconnect =>
        base_noc_params.copy(globalTerminalChannels = Some(() => c.getSubnetTerminalChannels(SBUS)))
      case _ => base_noc_params
    }
    val constellation = LazyModule(new ConstellationSystemBus(params, noc_params, loc.name))

    constellation.suggestName(loc.name)
    context.tlBusWrapperLocationMap += (loc -> constellation)
    constellation
  }
}


class ConstellationSystemBus(sbus_params: SystemBusParams, noc_params: TLNoCParams, name: String)
  (implicit p: Parameters) extends TLBusWrapper(sbus_params, name) {
  private val replicator = sbus_params.replication.map(r => LazyModule(new RegionReplicator(r)))
  val prefixNode = replicator.map { r =>
    r.prefix := addressPrefixNexusNode
    addressPrefixNexusNode
  }

  private val system_bus_noc = LazyModule(new TLNoC(noc_params))
  val inwardNode: TLInwardNode = system_bus_noc.node :=* TLFIFOFixer(TLFIFOFixer.allVolatile) :=* replicator.map(_.node).getOrElse(TLTempNode())
  val outwardNode: TLOutwardNode = system_bus_noc.node
  def busView: TLEdge = system_bus_noc.node.edges.in.head

  val builtInDevices: BuiltInDevices = BuiltInDevices.attach(sbus_params, outwardNode)
}

case class ConstellationMemoryBusParams(
  params: MemoryBusParams,
  privateNoC: Option[NoCParams]
) extends TLBusWrapperInstantiationLike {
  def instantiate(context: HasTileLinkLocations, loc: Location[TLBusWrapper])(implicit p: Parameters): ConstellationMemoryBus = {
    val base_noc_params = TLNoCParams("mbus", p(ConstellationTLNetworkNodeMappingKey(loc)), privateNoC)
    val noc_params = context match {
      case c: CanHaveGlobalTLInterconnect =>
        base_noc_params.copy(globalTerminalChannels = Some(() => c.getSubnetTerminalChannels(MBUS)))
      case _ => base_noc_params
    }
    val constellation = LazyModule(new ConstellationMemoryBus(params, noc_params, loc.name))

    constellation.suggestName(loc.name)
    context.tlBusWrapperLocationMap += (loc -> constellation)
    constellation
  }
}

class ConstellationMemoryBus(mbus_params: MemoryBusParams, noc_params: TLNoCParams, name: String)
  (implicit p: Parameters) extends TLBusWrapper(mbus_params, name) {
  private val replicator = mbus_params.replication.map(r => LazyModule(new RegionReplicator(r)))
  val prefixNode = replicator.map { r =>
    r.prefix := addressPrefixNexusNode
    addressPrefixNexusNode
  }

  private val memory_bus_noc = LazyModule(new TLNoC(noc_params))
  val inwardNode: TLInwardNode =
    replicator.map(memory_bus_noc.node :*=* TLFIFOFixer(TLFIFOFixer.all) :*=* _.node)
        .getOrElse(memory_bus_noc.node :*=* TLFIFOFixer(TLFIFOFixer.all))

  val outwardNode: TLOutwardNode = ProbePicker() :*= memory_bus_noc.node
  def busView: TLEdge = memory_bus_noc.node.edges.in.head

  val builtInDevices: BuiltInDevices = BuiltInDevices.attach(mbus_params, outwardNode)
}

case class ConstellationPeripheryBusParams(
  params: PeripheryBusParams,
  privateNoC: Option[NoCParams]
) extends TLBusWrapperInstantiationLike {
  def instantiate(context: HasTileLinkLocations, loc: Location[TLBusWrapper])(implicit p: Parameters): ConstellationPeripheryBus = {
    val base_noc_params = TLNoCParams(loc.name, p(ConstellationTLNetworkNodeMappingKey(loc)), privateNoC)
    val noc_params = context match {
      case c: CanHaveGlobalTLInterconnect =>
        base_noc_params.copy(globalTerminalChannels = Some(() => c.getSubnetTerminalChannels(MBUS)))
      case _ => base_noc_params
    }
    val constellation = LazyModule(new ConstellationPeripheryBus(loc.name, params, noc_params))

    constellation.suggestName(loc.name)
    context.tlBusWrapperLocationMap += (loc -> constellation)
    constellation
  }
}

class ConstellationPeripheryBus(name: String, pbus_params: PeripheryBusParams, noc_params: TLNoCParams)
  (implicit p: Parameters) extends TLBusWrapper(pbus_params, name) {

  private val replicator = pbus_params.replication.map(r => LazyModule(new RegionReplicator(r)))
  val prefixNode = replicator.map { r =>
    r.prefix := addressPrefixNexusNode
    addressPrefixNexusNode
  }

  private val fixer = LazyModule(new TLFIFOFixer(TLFIFOFixer.all))
  private val node: TLNode = pbus_params.atomics.map { pa =>
    val in_xbar = LazyModule(new TLXbar)
    val out_noc = LazyModule(new TLNoC(noc_params))
    val fixer_node = replicator.map(fixer.node :*= _.node).getOrElse(fixer.node)
    (out_noc.node
      :*= fixer_node
      :*= TLBuffer(pa.buffer)
      :*= (pa.widenBytes.filter(_ > beatBytes).map { w =>
          TLWidthWidget(w) :*= TLAtomicAutomata(arithmetic = pa.arithmetic)
        } .getOrElse { TLAtomicAutomata(arithmetic = pa.arithmetic) })
      :*= in_xbar.node)
  } .getOrElse {
    val noc = LazyModule(new TLNoC(noc_params))
    noc.node :*= fixer.node
  }

  def inwardNode: TLInwardNode = node
  def outwardNode: TLOutwardNode = node
  def busView: TLEdge = fixer.node.edges.in.head

  val builtInDevices: BuiltInDevices = BuiltInDevices.attach(pbus_params, outwardNode)

}
