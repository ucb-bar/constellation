package constellation.soc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util._

import constellation.noc.NoCParams
import constellation.protocol.TLNoCParams
import scala.collection.immutable.{ListMap}

class WithSbusNoC(tlnocParams: TLNoCParams, globalNoC: Boolean = false) extends Config((site, here, up) => {
  case TLNetworkTopologyLocated(InSubsystem) => {
    up(TLNetworkTopologyLocated(InSubsystem), site).map(topo => {
      topo match {
        case j: TLBusWrapperTopology => {
          new TLBusWrapperTopology(j.instantiations.map(inst => inst match {
            case (SBUS, sbus_params: SystemBusParams) =>
              (SBUS, ConstellationSystemBusParams(sbus_params, tlnocParams, globalNoC))
            case a => a
          }), j.connections)
        }
        case x => x
      }
    })
  }
})

class WithMbusNoC(tlnocParams: TLNoCParams, globalNoC: Boolean = false) extends Config((site, here, up) => {
  case TLNetworkTopologyLocated(InSubsystem) => {
    up(TLNetworkTopologyLocated(InSubsystem), site).map(topo => {
      topo match {
        case j: TLBusWrapperTopology => {
          new TLBusWrapperTopology(j.instantiations.map(inst => inst match {
            case (MBUS, mbus_params: MemoryBusParams) =>
              (MBUS, ConstellationMemoryBusParams(mbus_params, tlnocParams, globalNoC))
            case a => a
          }), j.connections)
        }
        case x => x
      }
    })
  }
})

class WithCbusNoC(tlnocParams: TLNoCParams) extends Config((site, here, up) => {
  case TLNetworkTopologyLocated(InSubsystem) => {
    up(TLNetworkTopologyLocated(InSubsystem), site).map(topo => {
      topo match {
        case j: TLBusWrapperTopology => {
          new TLBusWrapperTopology(j.instantiations.map(inst => inst match {
            case (CBUS, cbus_params: PeripheryBusParams) =>
              (CBUS, ConstellationPeripheryBusParams(cbus_params, tlnocParams))
            case a => a
          }), j.connections)
        }
        case x => x
      }
    })
  }
})

class WithPbusNoC(tlnocParams: TLNoCParams, globalNoC: Boolean = false) extends Config((site, here, up) => {
  case TLNetworkTopologyLocated(InSubsystem) => {
    up(TLNetworkTopologyLocated(InSubsystem), site).map(topo => {
      topo match {
        case j: TLBusWrapperTopology => {
          new TLBusWrapperTopology(j.instantiations.map(inst => inst match {
            case (PBUS, pbus_params: PeripheryBusParams) =>
              (PBUS, ConstellationPeripheryBusParams(pbus_params, tlnocParams, globalNoC))
            case a => a
          }), j.connections)
        }
        case x => x
      }
    })
  }
})


class WithGlobalNoC(params: GlobalNoCParams) extends Config((site, here, up) => {
  case GlobalNoCKey => params
})
