package constellation.router

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

import constellation.channel._

class SwitchAllocReq(val outParams: Seq[ChannelParams], val egressParams: Seq[EgressChannelParams])
  (implicit val p: Parameters) extends Bundle with HasRouterOutputParams {
  val vc_sel = MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool()) })
  val tail = Bool()
}

class SwitchArbiter(inN: Int, outN: Int, outParams: Seq[ChannelParams], egressParams: Seq[EgressChannelParams])(implicit val p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Vec(inN, Decoupled(new SwitchAllocReq(outParams, egressParams))))
    val out = Vec(outN, Decoupled(new SwitchAllocReq(outParams, egressParams)))
    val chosen_oh = Vec(outN, Output(UInt(inN.W)))
  })

  val lock = Seq.fill(outN) { RegInit(0.U(inN.W)) }
  val unassigned = Cat(io.in.map(_.valid).reverse) & ~(lock.reduce(_|_))

  val mask = RegInit(0.U(inN.W))
  val choices = Wire(Vec(outN, UInt(inN.W)))

  var sel = PriorityEncoderOH(Cat(unassigned, unassigned & !mask))
  for (i <- 0 until outN) {
    choices(i) := sel | (sel >> inN)
    sel = PriorityEncoderOH(unassigned & ~choices(i))
  }

  io.in.foreach(_.ready := false.B)

  var chosens = 0.U(inN.W)
  val in_tails = Cat(io.in.map(_.bits.tail).reverse)
  for (i <- 0 until outN) {
    val in_valids = Cat((0 until inN).map { j => io.in(j).valid && !chosens(j) }.reverse)
    val chosen = Mux((in_valids & lock(i) & ~chosens).orR, lock(i), choices(i))
    io.chosen_oh(i) := chosen
    io.out(i).valid := (in_valids & chosen).orR
    io.out(i).bits := Mux1H(chosen, io.in.map(_.bits))
    for (j <- 0 until inN) {
      when (chosen(j) && io.out(i).ready) {
        io.in(j).ready := true.B
      }
    }
    chosens = chosens | chosen
    when (io.out(i).fire()) {
      lock(i) := chosen & ~in_tails
    }
  }

  when (io.out(0).fire()) {
    mask := (0 until inN).map { i => (io.chosen_oh(0) >> i) }.reduce(_|_)
  } .otherwise {
    mask := Mux(~mask === 0.U, 0.U, (mask << 1) | 1.U(1.W))
  }
}

class SwitchAllocator(
  val routerParams: RouterParams,
  val inParams: Seq[ChannelParams],
  val outParams: Seq[ChannelParams],
  val ingressParams: Seq[IngressChannelParams],
  val egressParams: Seq[EgressChannelParams]
)(implicit val p: Parameters) extends Module
    with HasRouterParams
    with HasRouterInputParams
    with HasRouterOutputParams {
  val io = IO(new Bundle {
    val req = MixedVec(allInParams.map(u =>
      Vec(u.destSpeedup, Flipped(Decoupled(new SwitchAllocReq(outParams, egressParams))))))
    val credit_alloc = MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Output(new OutputCreditAlloc))})
    val switch_sel = MixedVec(allOutParams.map { o => Vec(o.srcSpeedup,
      MixedVec(allInParams.map { i => Vec(i.destSpeedup, Output(Bool())) })) })
  })
  val nInputChannels = allInParams.map(_.nVirtualChannels).sum

  val arbs = allOutParams.map { oP => Module(new SwitchArbiter(
    allInParams.map(_.destSpeedup).reduce(_+_),
    oP.srcSpeedup,
    outParams,
    egressParams
  ))}
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
    for (j <- 0 until allOutParams(i).srcSpeedup) {
      idx = 0
      for (m <- 0 until nAllInputs) {
        for (n <- 0 until allInParams(m).destSpeedup) {
          io.switch_sel(i)(j)(m)(n) := arbs(i).io.in(idx).valid && arbs(i).io.chosen_oh(j)(idx) && arbs(i).io.out(j).valid
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
