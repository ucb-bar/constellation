package constellation.rc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util._

import constellation.noc.{NoCKey}
import scala.collection.immutable.{ListMap}

// Use a noc based system bus. By default instantiates a private noc within the bus
class WithSbusNoC extends Config((site, here, up) => {
  case TLNetworkTopologyLocated(InSubsystem) => {
    up(TLNetworkTopologyLocated(InSubsystem), site).map(topo => {
      topo match {
        case j: TLBusWrapperTopology => {
          new TLBusWrapperTopology(j.instantiations.map(inst => inst match {
            case (SBUS, sbus_params: SystemBusParams) =>
              (SBUS, ConstellationSystemBusParams(sbus_params, Some(up(NoCKey))))
            case a => a
          }), j.connections)
        }
        case x => x
      }
    })
  }
})


// Use a noc based memory bus. By default instantiates a private noc within the bus
class WithMbusNoC extends Config((site, here, up) => {
  case TLNetworkTopologyLocated(InSubsystem) => {
    up(TLNetworkTopologyLocated(InSubsystem), site).map(topo => {
      topo match {
        case j: TLBusWrapperTopology => {
          new TLBusWrapperTopology(j.instantiations.map(inst => inst match {
            case (MBUS, mbus_params: MemoryBusParams) =>
              (MBUS, ConstellationMemoryBusParams(mbus_params, Some(up(NoCKey))))
            case a => a
          }), j.connections)
        }
        case x => x
      }
    })
  }
})

// Use a noc based control bus. By default instantiates a private noc within the bus
class WithCbusNoC(explicitWidth: Option[Int] = None) extends Config((site, here, up) => {
  case TLNetworkTopologyLocated(InSubsystem) => {
    up(TLNetworkTopologyLocated(InSubsystem), site).map(topo => {
      topo match {
        case j: TLBusWrapperTopology => {
          new TLBusWrapperTopology(j.instantiations.map(inst => inst match {
            case (CBUS, cbus_params: PeripheryBusParams) =>
              (CBUS, ConstellationPeripheryBusParams(cbus_params, Some(up(NoCKey)), explicitWidth))
            case a => a
          }), j.connections)
        }
        case x => x
      }
    })
  }
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



// connect them to the global noc instead
class WithSbusGlobalNoC extends Config((site, here, up) => {
  case GlobalTLInterconnectKey => up(GlobalTLInterconnectKey).copy(
    supportedBuses = up(GlobalTLInterconnectKey).supportedBuses + SBUS,
    nocParams = up(NoCKey)
  )
  case TLNetworkTopologyLocated(InSubsystem) => {
    up(TLNetworkTopologyLocated(InSubsystem), site).map(topo => {
      topo match {
        case j: TLBusWrapperTopology => {
          new TLBusWrapperTopology(j.instantiations.map(inst => inst match {
            case (SBUS, sbus_params: ConstellationSystemBusParams) => {
              (SBUS, sbus_params.copy(privateNoC=None))
            }
            case a => a
          }), j.connections)
        }
        case x => x
      }
    })
  }
})

class WithMbusGlobalNoC extends Config((site, here, up) => {
  case GlobalTLInterconnectKey => up(GlobalTLInterconnectKey).copy(
    supportedBuses = up(GlobalTLInterconnectKey).supportedBuses + MBUS,
    nocParams = up(NoCKey)
  )
  case TLNetworkTopologyLocated(InSubsystem) => {
    up(TLNetworkTopologyLocated(InSubsystem), site).map(topo => {
      topo match {
        case j: TLBusWrapperTopology => {
          new TLBusWrapperTopology(j.instantiations.map(inst => inst match {
            case (MBUS, mbus_params: ConstellationMemoryBusParams) => {
              (MBUS, mbus_params.copy(privateNoC=None))
            }
            case a => a
          }), j.connections)
        }
        case x => x
      }
    })
  }
})

class WithGlobalNoCWidth(w: Int) extends Config((site, here, up) => {
  case GlobalTLInterconnectKey => up(GlobalTLInterconnectKey).copy(payloadWidth=w)
})
