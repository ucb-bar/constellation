package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}

class Router(id: Int, inParams: Seq[ChannelParams], outParams: Seq[ChannelParams])(implicit val p: Parameters) extends Module with HasAstroNoCParams {
  val nInputs = inParams.size
  val nOutputs = outParams.size
  val io = IO(new Bundle {
    val in = Vec(nInputs, Flipped(Valid(new Flit)))
    val out = Vec(nInputs, Valid(new Flit))
  })

  require(nInputs >= 1 && nOutputs >= 1)
  require(id < (1 << idBits))
  val input_units = inParams.map { u => Module(new InputUnit(u, outParams)) }
  //val switch = Module(new Switch)
  val switch_allocator = Module(new SwitchAllocator(inParams, outParams))
  val vc_allocator = Module(new VCAllocator(inParams, outParams))
  val route_computer = Module(new RouteComputer(inParams, outParams, id))
  val output_units = outParams.map { u => Module(new OutputUnit(inParams, u)) }

  (io.in zip input_units).map { case (i,u) => u.io.in := i }


  (route_computer.io.req zip input_units).map { case (i,u) => i <> u.io.router_req }
  (input_units zip route_computer.io.resp).map { case (u,o) => u.io.router_resp <> o }

  (vc_allocator.io.req zip input_units).map { case (i,u) => i <> u.io.vcalloc_req }
  (input_units zip vc_allocator.io.resp).map { case (u,o) => u.io.vcalloc_resp <> o }
  (output_units zip vc_allocator.io.out_alloc).map { case (u,a) => u.io.alloc <> a }

  input_units.foreach(u => (u.io.out_credits zip output_units).map { case (l,r) = l := r.io.credits })
  (input_units zip switch_allocator.io.req).map { case (u,r) => r <> u.io.swalloc_req }

}
