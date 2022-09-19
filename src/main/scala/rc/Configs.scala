package constellation.rc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util._

import constellation.noc.NoCParams
import scala.collection.immutable.{ListMap}

// Use a noc based system bus. By default instantiates a private noc within the bus
class WithSbusNoC(nocParams: Option[NoCParams]) extends Config((site, here, up) => {
  case TLNetworkTopologyLocated(InSubsystem) => {
    up(TLNetworkTopologyLocated(InSubsystem), site).map(topo => {
      topo match {
        case j: TLBusWrapperTopology => {
          new TLBusWrapperTopology(j.instantiations.map(inst => inst match {
            case (SBUS, sbus_params: SystemBusParams) =>
              (SBUS, ConstellationSystemBusParams(sbus_params, nocParams))
            case a => a
          }), j.connections)
        }
        case x => x
      }
    })
  }
  case GlobalTLInterconnectKey => up(GlobalTLInterconnectKey).copy(
    supportedBuses = up(GlobalTLInterconnectKey).supportedBuses ++ (if (nocParams.isDefined) Seq(SBUS) else Nil)
  )
})


// Use a noc based memory bus. By default instantiates a private noc within the bus
class WithMbusNoC(nocParams: Option[NoCParams]) extends Config((site, here, up) => {
  case TLNetworkTopologyLocated(InSubsystem) => {
    up(TLNetworkTopologyLocated(InSubsystem), site).map(topo => {
      topo match {
        case j: TLBusWrapperTopology => {
          new TLBusWrapperTopology(j.instantiations.map(inst => inst match {
            case (MBUS, mbus_params: MemoryBusParams) =>
              (MBUS, ConstellationMemoryBusParams(mbus_params, nocParams))
            case a => a
          }), j.connections)
        }
        case x => x
      }
    })
  }
  case GlobalTLInterconnectKey => up(GlobalTLInterconnectKey).copy(
    supportedBuses = up(GlobalTLInterconnectKey).supportedBuses ++ (if (nocParams.isDefined) Seq(MBUS) else Nil)
  )

})

// Use a noc based control bus. By default instantiates a private noc within the bus
class WithCbusNoC(nocParams: Option[NoCParams], explicitWidth: Option[Int] = None) extends Config((site, here, up) => {
  case TLNetworkTopologyLocated(InSubsystem) => {
    up(TLNetworkTopologyLocated(InSubsystem), site).map(topo => {
      topo match {
        case j: TLBusWrapperTopology => {
          new TLBusWrapperTopology(j.instantiations.map(inst => inst match {
            case (CBUS, cbus_params: PeripheryBusParams) =>
              (CBUS, ConstellationPeripheryBusParams(cbus_params, nocParams, explicitWidth))
            case a => a
          }), j.connections)
        }
        case x => x
      }
    })
  }
  case GlobalTLInterconnectKey => up(GlobalTLInterconnectKey).copy(
    supportedBuses = up(GlobalTLInterconnectKey).supportedBuses ++ (if (nocParams.isDefined) Seq(CBUS) else Nil)
  )
})

// Config options for setting node mappings for each bus
class WithSbusNoCInNodeMapping(matchStr: String, node: Int) extends Config((site, here, up) => {
  case ConstellationTLNetworkNodeMappingKey(SBUS) => up(ConstellationTLNetworkNodeMappingKey(SBUS)).copy(
    inNodeMapping = up(ConstellationTLNetworkNodeMappingKey(SBUS)).inNodeMapping + (matchStr -> node)
  )
})
class WithSbusNoCOutNodeMapping(matchStr: String, node: Int) extends Config((site, here, up) => {
  case ConstellationTLNetworkNodeMappingKey(SBUS) => up(ConstellationTLNetworkNodeMappingKey(SBUS)).copy(
    outNodeMapping = up(ConstellationTLNetworkNodeMappingKey(SBUS)).outNodeMapping + (matchStr -> node)
  )
})

class WithMbusNoCInNodeMapping(matchStr: String, node: Int) extends Config((site, here, up) => {
  case ConstellationTLNetworkNodeMappingKey(MBUS) => up(ConstellationTLNetworkNodeMappingKey(MBUS)).copy(
    inNodeMapping = up(ConstellationTLNetworkNodeMappingKey(MBUS)).inNodeMapping + (matchStr -> node)
  )
})
class WithMbusNoCOutNodeMapping(matchStr: String, node: Int) extends Config((site, here, up) => {
  case ConstellationTLNetworkNodeMappingKey(MBUS) => up(ConstellationTLNetworkNodeMappingKey(MBUS)).copy(
    outNodeMapping = up(ConstellationTLNetworkNodeMappingKey(MBUS)).outNodeMapping + (matchStr -> node)
  )
})

class WithCbusNoCInNodeMapping(matchStr: String, node: Int) extends Config((site, here, up) => {
  case ConstellationTLNetworkNodeMappingKey(CBUS) => up(ConstellationTLNetworkNodeMappingKey(CBUS)).copy(
    inNodeMapping = up(ConstellationTLNetworkNodeMappingKey(CBUS)).inNodeMapping + (matchStr -> node)
  )
})
class WithCbusNoCOutNodeMapping(matchStr: String, node: Int) extends Config((site, here, up) => {
  case ConstellationTLNetworkNodeMappingKey(CBUS) => up(ConstellationTLNetworkNodeMappingKey(CBUS)).copy(
    outNodeMapping = up(ConstellationTLNetworkNodeMappingKey(CBUS)).outNodeMapping + (matchStr -> node)
  )
})

class WithCBusNoCGlobalNoCCtrlMapping(f: Int => Int) extends Config((site, here, up) => {
  case ConstellationTLNetworkNodeMappingKey(CBUS) => up(ConstellationTLNetworkNodeMappingKey(CBUS)).copy(
    outNodeMapping = up(ConstellationTLNetworkNodeMappingKey(CBUS)).outNodeMapping ++
      (0 until site(GlobalTLInterconnectKey).nocParams.topology.nNodes).map { i => s"global_noc_ctrl[$i]" -> f(i) }.toMap
  )
})


// Use a global shared noc
class WithGlobalNoC(nocParams: NoCParams) extends Config((site, here, up) => {
  case GlobalTLInterconnectKey => up(GlobalTLInterconnectKey).copy(
    nocParams=nocParams
  )
})

class WithGlobalNoCWidth(w: Int) extends Config((site, here, up) => {
  case GlobalTLInterconnectKey => up(GlobalTLInterconnectKey).copy(payloadWidth=w)
})
