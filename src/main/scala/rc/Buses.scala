package constellation.rc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util._

import constellation.noc.{NoCConfig}

case class ConstellationSystemBusParams(
  params: SystemBusParams,
  inNodeMapping: Seq[Int],
  outNodeMapping: Seq[Int],
  privateNoC: Option[NoCConfig]
) extends TLBusWrapperInstantiationLike {
  def instantiate(context: HasTileLinkLocations, loc: Location[TLBusWrapper])(implicit p: Parameters): ConstellationSystemBus = {
    val base_noc_params = TLNoCParams("sbus", inNodeMapping, outNodeMapping, privateNoC)
    val noc_params = context match {
      case c: CanHaveGlobalTLInterconnect =>
        base_noc_params.copy(globalTerminalChannels = Some(() => c.getSubnetTerminalChannels(SBUS)))
      case _ => base_noc_params
    }
    val constellation = LazyModule(new ConstellationSystemBus(params, noc_params))

    constellation.suggestName(loc.name)
    context.tlBusWrapperLocationMap += (loc -> constellation)
    constellation
  }
}


class ConstellationSystemBus(sbus_params: SystemBusParams, noc_params: TLNoCParams)
  (implicit p: Parameters) extends TLBusWrapper(sbus_params, "system_bus_noc") {
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
  inNodeMapping: Seq[Int],
  outNodeMapping: Seq[Int],
  privateNoC: Option[NoCConfig]
) extends TLBusWrapperInstantiationLike {
  def instantiate(context: HasTileLinkLocations, loc: Location[TLBusWrapper])(implicit p: Parameters): ConstellationMemoryBus = {
    val base_noc_params = TLNoCParams("mbus", inNodeMapping, outNodeMapping, privateNoC)
    val noc_params = context match {
      case c: CanHaveGlobalTLInterconnect =>
        base_noc_params.copy(globalTerminalChannels = Some(() => c.getSubnetTerminalChannels(MBUS)))
      case _ => base_noc_params
    }
    val constellation = LazyModule(new ConstellationMemoryBus(params, noc_params))

    constellation.suggestName(loc.name)
    context.tlBusWrapperLocationMap += (loc -> constellation)
    constellation
  }
}

class ConstellationMemoryBus(mbus_params: MemoryBusParams, noc_params: TLNoCParams)
  (implicit p: Parameters) extends TLBusWrapper(mbus_params, "memory_bus_noc") {
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
