package constellation.rc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util._


case object NBUS extends TLBusWrapperLocation("subsystem_nbus")

case class NoCControlBusTopologyParams(
  nbus: ConstellationPeripheryBusParams
) extends TLBusWrapperTopology(
  instantiations = List((NBUS, nbus)),
  connections = List((CBUS, NBUS, TLBusWrapperConnection.crossTo(NoCrossing, None)))
)
