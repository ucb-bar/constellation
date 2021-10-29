package constellation.router

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}

import constellation._

case class RouterParams(
  nodeId: Int,
  inParams: Seq[ChannelParams],
  outParams: Seq[ChannelParams],
  ingressParams: Seq[ChannelParams],
  egressParams: Seq[ChannelParams],
  masterAllocTable: (Int, Int, Int, Int, Int, Int) => Boolean,
  combineSAST: Boolean = false,
  combineRCVA: Boolean = false,
)

trait HasRouterOutputParams extends HasNoCParams {
  val outParams: Seq[ChannelParams]
  val egressParams: Seq[ChannelParams]

  def allOutParams = outParams ++ egressParams

  def nOutputs = outParams.size
  def nEgress = egressParams.size
  def nAllOutputs = allOutParams.size
}

trait HasRouterInputParams extends HasNoCParams {
  val inParams: Seq[ChannelParams]
  val ingressParams: Seq[ChannelParams]

  def allInParams = inParams ++ ingressParams

  def nInputs = inParams.size
  def nIngress = ingressParams.size
  def nAllInputs = allInParams.size
}

trait HasRouterParams extends HasRouterOutputParams with HasRouterInputParams
{
  val rP: RouterParams
  val nodeId = rP.nodeId
  val inParams = rP.inParams
  val outParams = rP.outParams
  val ingressParams = rP.ingressParams
  val egressParams = rP.egressParams

  def possibleTransition(inParam: ChannelParams, outParam: ChannelParams): Boolean = {

    // Always allow transition to output if the packet has reached
    // its destination
    if (outParam.destId == -1)
      return true

    val legalVirtualTransition = Seq.tabulate(
      inParam.nVirtualChannels,
      outParam.nVirtualChannels,
      nNodes,
      nVirtualNetworks) { case (inV, outV, destId, vNetId) =>
        (rP.masterAllocTable(inParam.srcId, inV, outParam.destId, outV, destId, vNetId) ||
          (inParam.isIngress && outParam.isEgress))
    }.flatten.flatten.flatten.reduce(_||_)

    legalVirtualTransition
  }
}

class Router(val rP: RouterParams)(implicit val p: Parameters) extends Module with HasRouterParams {
  allOutParams.foreach(u => require(u.srcId == nodeId))
  allInParams.foreach(u => require(u.destId == nodeId))

  val io = IO(new Bundle {
    val in = MixedVec(inParams.map { u => Flipped(new Channel(u)) })
    val ingress = MixedVec(ingressParams.map { u => Flipped(new TerminalChannel(u)) })
    val out = MixedVec(outParams.map { u => new Channel(u) })
    val egress = MixedVec(egressParams.map { u => new TerminalChannel(u) })
  })

  require(nAllInputs >= 1)
  require(nAllOutputs >= 1)
  require(nodeId < (1 << nodeIdBits))

  val input_units = inParams.zipWithIndex.map { case (u,i) =>
    Module(new InputUnit(u, outParams, egressParams, rP.combineRCVA, rP.combineSAST))
      .suggestName(s"input_unit_${i}_from_${u.srcId}") }
  val ingress_units = ingressParams.zipWithIndex.map { case (u,i) =>
    Module(new IngressUnit(u, outParams, egressParams, rP.combineRCVA, rP.combineSAST))
      .suggestName(s"ingress_unit_${i+nInputs}_from_${u.ingressId}") }
  val all_input_units = input_units ++ ingress_units

  val output_units = outParams.zipWithIndex.map { case (u,i) =>
    Module(new OutputUnit(inParams, ingressParams, u))
      .suggestName(s"output_unit_${i}_to_${u.destId}")}
  val egress_units = egressParams.zipWithIndex.map { case (u,i) =>
    Module(new EgressUnit(inParams, ingressParams, u))
      .suggestName(s"egress_unit_${i+nOutputs}_to_${u.egressId}")}
  val all_output_units = output_units ++ egress_units

  val switch = Module(new Switch(rP))
  val switch_allocator = Module(new SwitchAllocator(rP))
  val vc_allocator = Module(new VCAllocator(rP))
  val route_computer = Module(new RouteComputer(rP))

  (io.in zip input_units).foreach {
    case (i,u) => u.io.in <> i }
  (io.ingress zip ingress_units).foreach {
    case (i,u) => u.io.in <> i.flit }
  (output_units zip io.out).foreach {
    case (u,o) => o <> u.io.out }
  (egress_units zip io.egress).foreach {
    case (u,o) => o.flit <> u.io.out }

  (route_computer.io.req zip all_input_units).foreach {
    case (i,u) => i <> u.io.router_req }
  (all_input_units zip route_computer.io.resp).foreach {
    case (u,o) => u.io.router_resp <> o }

  (vc_allocator.io.req zip all_input_units).foreach {
    case (i,u) => i <> u.io.vcalloc_req }
  (all_input_units zip vc_allocator.io.resp).foreach {
    case (u,o) => u.io.vcalloc_resp <> o }


  (all_output_units zip vc_allocator.io.out_allocs).foreach {
    case (u,a) => u.io.allocs <> a }
  (vc_allocator.io.channel_available zip all_output_units).foreach {
    case (a,u) => a := u.io.channel_available }

  all_input_units.foreach(in => all_output_units.zipWithIndex.foreach { case (out,outIdx) =>
    if (possibleTransition(in.cParam, out.cParam)) {
      in.io.out_credit_available(outIdx) := out.io.credit_available
    } else {
      in.io.out_credit_available(outIdx).foreach(_ := false.B)
    }
    dontTouch(in.io.out_credit_available)
  })
  (all_input_units zip switch_allocator.io.req).foreach {
    case (u,r) => r <> u.io.salloc_req }
  (output_units zip switch_allocator.io.credit_alloc).foreach {
    case (u,a) => u.io.credit_alloc := a }

  (switch.io.in zip all_input_units).foreach {
    case (i,u) => i <> u.io.out }
  (all_output_units zip switch.io.out).foreach {
    case (u,o) => u.io.in <> o }

}
