package constellation.soc

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util._

import constellation.noc.NoCParams
import constellation.protocol.TLNoCParams
import scala.collection.immutable.{ListMap}

/** System bus */
class WithSbusNoC(tlnocParams: TLNoCParams, inlineNoC: Boolean = false) extends Config((site, here, up) => {
  case TLNetworkTopologyLocated(InSubsystem) => {
    up(TLNetworkTopologyLocated(InSubsystem), site).map(topo => {
      topo match {
        case j: TLBusWrapperTopology => {
          new TLBusWrapperTopology(j.instantiations.map(inst => inst match {
            case (SBUS, sbus_params: SystemBusParams) =>
              (SBUS, ConstellationSystemBusParams(sbus_params, tlnocParams, inlineNoC))
            case a => a
          }), j.connections)
        }
        case x => x
      }
    })
  }
})

/** Memory bus */
class WithMbusNoC(tlnocParams: TLNoCParams, inlineNoC: Boolean = false) extends Config((site, here, up) => {
  case TLNetworkTopologyLocated(InSubsystem) => {
    up(TLNetworkTopologyLocated(InSubsystem), site).map(topo => {
      topo match {
        case j: TLBusWrapperTopology => {
          new TLBusWrapperTopology(j.instantiations.map(inst => inst match {
            case (MBUS, mbus_params: MemoryBusParams) =>
              (MBUS, ConstellationMemoryBusParams(mbus_params, tlnocParams, inlineNoC))
            case a => a
          }), j.connections)
        }
        case x => x
      }
    })
  }
})

/** Control bus */
class WithCbusNoC(tlnocParams: TLNoCParams, inlineNoC: Boolean = false) extends Config((site, here, up) => {
  case TLNetworkTopologyLocated(InSubsystem) => {
    up(TLNetworkTopologyLocated(InSubsystem), site).map(topo => {
      topo match {
        case j: TLBusWrapperTopology => {
          new TLBusWrapperTopology(j.instantiations.map(inst => inst match {
            case (CBUS, cbus_params: PeripheryBusParams) =>
              (CBUS, ConstellationPeripheryBusParams(cbus_params, tlnocParams, inlineNoC))
            case a => a
          }), j.connections)
        }
        case x => x
      }
    })
  }
})

/** Peripheral bus */
class WithPbusNoC(tlnocParams: TLNoCParams, inlineNoC: Boolean = false) extends Config((site, here, up) => {
  case TLNetworkTopologyLocated(InSubsystem) => {
    up(TLNetworkTopologyLocated(InSubsystem), site).map(topo => {
      topo match {
        case j: TLBusWrapperTopology => {
          new TLBusWrapperTopology(j.instantiations.map(inst => inst match {
            case (PBUS, pbus_params: PeripheryBusParams) =>
              (PBUS, ConstellationPeripheryBusParams(pbus_params, tlnocParams, inlineNoC))
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
