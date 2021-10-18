package constellation

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}

case class RouterParams(
  nodeId: Int,
  inParams: Seq[ChannelParams],
  outParams: Seq[ChannelParams],
  terminalInParams: Seq[ChannelParams],
  terminalOutParams: Seq[ChannelParams],
  vcAllocLegalPaths: (Int, Int, Int, Int) => Int => Boolean,
  routingFunction: (Int, Int, Int) => Int => Boolean
)

trait HasRouterOutputParams extends HasNoCParams {
  val outParams: Seq[ChannelParams]
  val terminalOutParams: Seq[ChannelParams]

  def allOutParams = outParams ++ terminalOutParams

  def nOutputs = outParams.size
  def nTerminalOutputs = terminalOutParams.size
  def nAllOutputs = allOutParams.size
}

trait HasRouterInputParams extends HasNoCParams {
  val inParams: Seq[ChannelParams]
  val terminalInParams: Seq[ChannelParams]

  def allInParams = inParams ++ terminalInParams

  def nInputs = inParams.size
  def nTerminalInputs = terminalInParams.size
  def nAllInputs = allInParams.size
}

trait HasRouterParams extends HasRouterOutputParams with HasRouterInputParams
{
  val rParams: RouterParams
  val nodeId = rParams.nodeId
  val inParams = rParams.inParams
  val outParams = rParams.outParams
  val terminalInParams = rParams.terminalInParams
  val terminalOutParams = rParams.terminalOutParams

  def possibleTransition(inParam: ChannelParams, outParam: ChannelParams): Boolean = {

    // Always allow transition to output if the packet has reached
    // its destination
    if (outParam.destId == -1)
      return true

    val legalVirtualTransition = Seq.tabulate(
      inParam.nVirtualChannels,
      outParam.nVirtualChannels,
      nPrios) { case (inV, outV, prio) =>
        (rParams.vcAllocLegalPaths(inParam.srcId, inV, outParam.destId, outV)(prio) ||
          (inParam.isTerminalInput && outParam.isTerminalOutput))
    }.flatten.flatten.reduce(_||_)

    val legalPhysicalTransition = outputNodes.map(out => (0 until nPrios).map { prio =>
      rParams.routingFunction(inParam.srcId, out, outParam.destId)(prio)
    }).flatten.reduce(_||_)

    require(!(!legalVirtualTransition && legalPhysicalTransition))
    legalVirtualTransition && legalPhysicalTransition
  }
}

class Router(val rParams: RouterParams)(implicit val p: Parameters) extends Module with HasRouterParams {
  allOutParams.foreach(u => require(u.srcId == nodeId))
  allInParams.foreach(u => require(u.destId == nodeId))

  val io = IO(new Bundle {
    val in = MixedVec(inParams.map { u => Flipped(new Channel(u)) })
    val terminal_in = MixedVec(terminalInParams.map { u => Flipped(new IOChannel(u)) })
    val out = MixedVec(outParams.map { u => new Channel(u) })
    val terminal_out = MixedVec(terminalOutParams.map { u => new IOChannel(u) })
  })

  require(nAllInputs >= 1)
  require(nAllOutputs >= 1)
  require(nodeId < (1 << nodeIdBits))

  val input_units = inParams.map { u =>
    Module(new InputUnit(u, outParams, terminalOutParams)) }
  val terminal_input_units = terminalInParams.map { u =>
    Module(new TerminalInputUnit(u, outParams, terminalOutParams)) }
  val all_input_units = input_units ++ terminal_input_units

  val output_units = outParams.map { u =>
    Module(new OutputUnit(inParams, terminalInParams, u)) }
  val terminal_output_units = terminalOutParams.map { u =>
    Module(new TerminalOutputUnit(inParams, terminalInParams, u)) }
  val all_output_units = output_units ++ terminal_output_units

  val switch = Module(new Switch(rParams))
  val switch_allocator = Module(new SwitchAllocator(rParams))
  val vc_allocator = Module(new VCAllocator(rParams))
  val route_computer = Module(new RouteComputer(rParams))

  (io.in zip input_units).foreach {
    case (i,u) => u.io.in <> i }
  (io.terminal_in zip terminal_input_units).foreach {
    case (i,u) => u.io.in <> i.flit }
  (output_units zip io.out).foreach {
    case (u,o) => o <> u.io.out }
  (terminal_output_units zip io.terminal_out).foreach {
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
