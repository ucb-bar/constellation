package constellation.router

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.util.{PlusArg}

import constellation.channel._
import constellation.routing.{RoutingRelation}
import constellation.{NoCKey, HasNoCParams}

case class UserRouterParams(
  payloadBits: Int = 64,
  combineSAST: Boolean = false,
  combineRCVA: Boolean = false,
  earlyRC: Boolean = false,
  vcAllocator: VCAllocatorParams => Parameters => VCAllocator = (vP) => (p) => new IterativeVCAllocator(vP)(p)
)

case class RouterParams(
  nodeId: Int,
  user: UserRouterParams
)

trait HasRouterOutputParams extends HasNoCParams {
  def outParams: Seq[ChannelParams]
  def egressParams: Seq[EgressChannelParams]

  def allOutParams = outParams ++ egressParams

  def nOutputs = outParams.size
  def nEgress = egressParams.size
  def nAllOutputs = allOutParams.size
}

trait HasRouterInputParams extends HasNoCParams {
  def inParams: Seq[ChannelParams]
  def ingressParams: Seq[IngressChannelParams]

  def allInParams = inParams ++ ingressParams

  def nInputs = inParams.size
  def nIngress = ingressParams.size
  def nAllInputs = allInParams.size
}

trait HasRouterParams extends HasRouterOutputParams with HasRouterInputParams
{
  def routerParams: RouterParams
  def nodeId = routerParams.nodeId
}

class Router(
  val routerParams: RouterParams,
  val inParams: Seq[ChannelParams],
  val outParams: Seq[ChannelParams],
  val ingressParams: Seq[IngressChannelParams],
  val egressParams: Seq[EgressChannelParams]
)(implicit p: Parameters) extends LazyModule with HasRouterParams {
  allOutParams.foreach(u => require(u.srcId == nodeId && u.payloadBits == routerParams.user.payloadBits))
  allInParams.foreach(u => require(u.destId == nodeId && u.payloadBits == routerParams.user.payloadBits))

  val destNodes = inParams.map(u => ChannelDestNode(u))
  val sourceNodes = outParams.map(u => ChannelSourceNode(u))
  val ingressNodes = ingressParams.map(u => TerminalChannelDestNode(u))
  val egressNodes = egressParams.map(u => TerminalChannelSourceNode(u))

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val debug = Output(new Bundle {
        val va_stall = Vec(nAllInputs, UInt())
        val sa_stall = Vec(nAllInputs, UInt())
      })
    })
    dontTouch(io.debug)
    val io_in = destNodes.map(_.in(0)._1)
    val io_out = sourceNodes.map(_.out(0)._1)
    val io_ingress = ingressNodes.map(_.in(0)._1)
    val io_egress = egressNodes.map(_.out(0)._1)

    require(nAllInputs >= 1)
    require(nAllOutputs >= 1)
    require(nodeId < (1 << nodeIdBits))

    val input_units = inParams.zipWithIndex.map { case (u,i) =>
      Module(new InputUnit(u, outParams, egressParams,
        routerParams.user.combineRCVA, routerParams.user.combineSAST, routerParams.user.earlyRC))
        .suggestName(s"input_unit_${i}_from_${u.srcId}") }
    val ingress_units = ingressParams.zipWithIndex.map { case (u,i) =>
      Module(new IngressUnit(u, outParams, egressParams,
        routerParams.user.combineRCVA, routerParams.user.combineSAST))
        .suggestName(s"ingress_unit_${i+nInputs}_from_${u.ingressId}") }
    val all_input_units = input_units ++ ingress_units

    val output_units = outParams.zipWithIndex.map { case (u,i) =>
      Module(new OutputUnit(inParams, ingressParams, u))
        .suggestName(s"output_unit_${i}_to_${u.destId}")}
    val egress_units = egressParams.zipWithIndex.map { case (u,i) =>
      Module(new EgressUnit(inParams, ingressParams, u))
        .suggestName(s"egress_unit_${i+nOutputs}_to_${u.egressId}")}
    val all_output_units = output_units ++ egress_units

    val switch = Module(new Switch(routerParams, inParams, outParams, ingressParams, egressParams))
    val switch_allocator = Module(new SwitchAllocator(routerParams, inParams, outParams, ingressParams, egressParams))
    val vc_allocator = Module(routerParams.user.vcAllocator(
      VCAllocatorParams(routerParams, inParams, outParams, ingressParams, egressParams)
    )(p))
    val route_computer = Module(new RouteComputer(routerParams, inParams, outParams, ingressParams, egressParams))


    val fires_count = WireInit(PopCount(vc_allocator.io.req.flatten.map(_.fire())))
    dontTouch(fires_count)


    (io_in zip input_units).foreach {
    case (i,u) => u.io.in <> i }
    (io_ingress zip ingress_units).foreach {
      case (i,u) => u.io.in <> i.flit }
    (output_units zip io_out).foreach {
      case (u,o) => o <> u.io.out }
    (egress_units zip io_egress).foreach {
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
      in.io.out_credit_available(outIdx) := out.io.credit_available
    })
    (all_input_units zip switch_allocator.io.req).foreach {
      case (u,r) => r <> u.io.salloc_req }
    (output_units zip switch_allocator.io.credit_alloc).foreach {
      case (u,a) => u.io.credit_alloc := a }

    (switch.io.in zip all_input_units).foreach {
      case (i,u) => i <> u.io.out }
    (all_output_units zip switch.io.out).foreach {
      case (u,o) => u.io.in <> o }
    switch.io.sel := (if (routerParams.user.combineSAST) {
      switch_allocator.io.switch_sel
    } else {
      RegNext(switch_allocator.io.switch_sel)
    })

    (io.debug.va_stall zip all_input_units.map(_.io.debug.va_stall)).map { case (l,r) => l := r }
    (io.debug.sa_stall zip all_input_units.map(_.io.debug.sa_stall)).map { case (l,r) => l := r }

    val debug_tsc = RegInit(0.U(64.W))
    debug_tsc := debug_tsc + 1.U
    val debug_sample = RegInit(0.U(64.W))
    debug_sample := debug_sample + 1.U
    val sample_rate = PlusArg("noc_util_sample_rate", width=20)
    when (debug_sample === sample_rate - 1.U) { debug_sample := 0.U }

    def sample(fire: Bool, s: String) = {
      val util_ctr = RegInit(0.U(64.W))
      val fired = RegInit(false.B)
      util_ctr := util_ctr + fire
      fired := fired || fire
      when (sample_rate =/= 0.U && debug_sample === sample_rate - 1.U && fired) {
        printf(s"nocsample %d $s %d\n", debug_tsc, util_ctr);
        fired := fire
      }
    }

    destNodes.map(_.in(0)).foreach { case (in, edge) => in.flit.map { f =>
      sample(f.fire(), s"${edge.cp.srcId} $nodeId")
    } }
    ingressNodes.map(_.in(0)).foreach { case (in, edge) =>
      sample(in.flit.fire(), s"i${edge.cp.asInstanceOf[IngressChannelParams].ingressId} $nodeId")
    }
    egressNodes.map(_.out(0)).foreach { case (out, edge) =>
      sample(out.flit.fire(), s"$nodeId e${edge.cp.asInstanceOf[EgressChannelParams].egressId}")
    }

  }
}
