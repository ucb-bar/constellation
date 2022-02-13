package constellation.router

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

import constellation.channel._
import constellation.util.{GrantHoldArbiter, ArbiterPolicy}

class SwitchAllocReq(val outParams: Seq[ChannelParams], val egressParams: Seq[EgressChannelParams])
  (implicit val p: Parameters) extends Bundle with HasRouterOutputParams {
  val vc_sel = MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool()) })
  val tail = Bool()
}

class SwitchAllocator(
  val routerParams: RouterParams,
  val inParams: Seq[ChannelParams],
  val outParams: Seq[ChannelParams],
  val ingressParams: Seq[IngressChannelParams],
  val egressParams: Seq[EgressChannelParams]
)(implicit val p: Parameters) extends Module with HasRouterParams {
  val io = IO(new Bundle {
    val req = MixedVec(allInParams.map(u =>
      Vec(u.destMultiplier, Flipped(Decoupled(new SwitchAllocReq(outParams, egressParams))))))
    val credit_alloc = MixedVec(outParams.map { u => Valid(UInt(log2Up(u.nVirtualChannels).W)) })
  })
  val nInputChannels = allInParams.map(_.nVirtualChannels).sum

  val arbs = Seq.fill(nAllOutputs) { Module(new GrantHoldArbiter(
    new SwitchAllocReq(outParams, egressParams),
    allInParams.map(_.destMultiplier).reduce(_+_),
    (d: SwitchAllocReq) => d.tail,
    policy = ArbiterPolicy.RoundRobin
  )) }
  arbs.foreach(_.io.out(0).ready := true.B)

  var idx = 0
  io.req.foreach(_.foreach { o =>
    val fires = Wire(Vec(arbs.size, Bool()))
    arbs.zipWithIndex.foreach { case (a,i) =>
      a.io.in(idx).valid := o.valid && o.bits.vc_sel(i).reduce(_||_)
      a.io.in(idx).bits := o.bits
      fires(i) := a.io.in(idx).fire()
    }
    o.ready := fires.reduce(_||_)
    idx += 1
  })

  (arbs.take(nOutputs) zip io.credit_alloc).map { case (a,i) =>
    i.valid := a.io.out(0).fire()
    val sel = a.io.out(0).bits.vc_sel.map(_.reduce(_||_))
    i.bits := Mux1H(sel, a.io.out(0).bits.vc_sel.map(v => OHToUInt(v)))
  }
}
