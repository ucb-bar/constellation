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
    val credit_alloc = MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Output(new OutputCreditAlloc))})
    val switch_sel = MixedVec(allOutParams.map { o => Vec(o.srcMultiplier,
      MixedVec(allInParams.map { i => Vec(i.destMultiplier, Output(Bool())) })) })
  })
  val nInputChannels = allInParams.map(_.nVirtualChannels).sum

  val arbs = allOutParams.map { oP => Module(new GrantHoldArbiter(
    new SwitchAllocReq(outParams, egressParams),
    allInParams.map(_.destMultiplier).reduce(_+_),
    (d: SwitchAllocReq) => d.tail,
    policy = ArbiterPolicy.RoundRobin,
    nOut = oP.srcMultiplier
  )) }
  arbs.foreach(_.io.out.foreach(_.ready := true.B))

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

  for (i <- 0 until nAllOutputs) {
    for (j <- 0 until allOutParams(i).srcMultiplier) {
      idx = 0
      for (m <- 0 until nAllInputs) {
        for (n <- 0 until allInParams(m).destMultiplier) {
          io.switch_sel(i)(j)(m)(n) := arbs(i).io.in(idx).valid && arbs(i).io.chosen(j) === idx.U && arbs(i).io.out(j).valid
          idx += 1
        }
      }
    }
  }

  io.credit_alloc.foreach(_.foreach(_.alloc := false.B))
  io.credit_alloc.foreach(_.foreach(_.tail := false.B))
  (arbs zip io.credit_alloc).zipWithIndex.map { case ((a,i),t) =>
    for (j <- 0 until i.size) {
      for (k <- 0 until a.io.out.size) {
        when (a.io.out(k).valid && a.io.out(k).bits.vc_sel(t)(j)) {
          i(j).alloc := true.B
          i(j).tail := a.io.out(k).bits.tail
        }
      }
    }
  }
}
