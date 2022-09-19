package constellation.router

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.{Parameters}
import constellation.channel.{ChannelParams, IngressChannelParams, EgressChannelParams}

trait HasRouterCtrlConsts {
  val CTRL_ADDR_SZ = 8
  val CTRL_WORD_SZ = 32

  val CTRL_MAX_ADDR = (1 << CTRL_ADDR_SZ) - 1
  val CTRL_MAX_INS = 16

  // Register addresses/offsets
  val CTRL_EPOCH_CYCLES          = 0
  val CTRL_IN_RATELIMITER_OFFSET = 16
  val CTRL_IN_RATE_OFFSET        = 32
  val CTRL_IN_FIRES_OFFSET       = 48
}

class RouterCtrlBundle extends Bundle with HasRouterCtrlConsts {
  val enable = Input(Bool())
  val write  = Input(Bool())
  val addr   = Input(UInt(CTRL_ADDR_SZ.W))
  val wdata  = Input(UInt(CTRL_WORD_SZ.W))
  val rdata  = Output(UInt(CTRL_WORD_SZ.W))

  def w(t: Int, reg: UInt) = {
    require(t <= CTRL_MAX_ADDR)
    when (enable && write && addr === t.U) { reg := wdata }
  }
  def r(t: Int, reg: UInt) {
    require(t <= CTRL_MAX_ADDR)
    when (enable && addr === t.U) { rdata := reg }
  }
  def rw(t: Int, reg: UInt) {
    w(t, reg)
    r(t, reg)
  }
}

class RouterControlUnit(
  val routerParams: RouterParams,
  val inParams: Seq[ChannelParams],
  val outParams: Seq[ChannelParams],
  val ingressParams: Seq[IngressChannelParams],
  val egressParams: Seq[EgressChannelParams]
)(implicit val p: Parameters) extends Module
    with HasRouterParams
    with HasRouterInputParams
    with HasRouterOutputParams
    with HasRouterCtrlConsts {
  val io = IO(new Bundle {
    val ctrl = new RouterCtrlBundle

    val in_block = Vec(allInParams.size, Output(Bool()))
    val in_fire  = MixedVec(allInParams.map { i => Vec(i.destSpeedup, Input(Bool())) })
  })
  require(allInParams.size <= CTRL_MAX_INS)
  io.ctrl.rdata := 0.U

  val epoch_cycles = RegInit(0.U(CTRL_WORD_SZ.W))
  io.ctrl.rw(CTRL_EPOCH_CYCLES, epoch_cycles)

  val epoch_ctr = RegInit(0.U(CTRL_WORD_SZ.W))
  val next_epoch_ctr = epoch_ctr + 1.U
  val reset_epoch = next_epoch_ctr === epoch_cycles
  epoch_ctr := Mux(reset_epoch, 0.U, next_epoch_ctr)

  for (i <- 0 until nAllInputs) {
    val enable_ratelimiter = RegInit(false.B)
    val rate          = RegInit(0.U(CTRL_WORD_SZ.W))
    val last_fire_ctr = RegInit(0.U(CTRL_WORD_SZ.W))
    val fire_ctr      = RegInit(0.U(CTRL_WORD_SZ.W))

    io.ctrl.rw(CTRL_IN_RATELIMITER_OFFSET + i, enable_ratelimiter)
    io.ctrl.rw(CTRL_IN_RATE_OFFSET        + i, rate)
    io.ctrl.r (CTRL_IN_FIRES_OFFSET       + i, last_fire_ctr)

    val next_fire_ctr = fire_ctr + PopCount(io.in_fire(i))
    fire_ctr := Mux(reset_epoch, 0.U, next_fire_ctr)
    when (reset_epoch) { last_fire_ctr := next_fire_ctr }
    io.in_block(i) := epoch_cycles =/= 0.U && rate < epoch_cycles && fire_ctr >= rate && enable_ratelimiter
  }
}
