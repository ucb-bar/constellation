package astronoc

import chisel3._
import chisel3.util._
import chisel3.util.random.LFSR

import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImpLike, LazyModuleImp}
import freechips.rocketchip.config.{Field, Parameters}

import astronoc._

object SelectFirstN
{
  def apply(in: UInt, n: Int) = {
    val sels = Wire(Vec(n, UInt(in.getWidth.W)))
    var mask = in

    for (i <- 0 until n) {
      sels(i) := PriorityEncoderOH(mask)
      mask = mask & ~sels(i)
    }

    sels
  }
}


class InputGen(idx: Int, prio: Int)(implicit val p: Parameters) extends Module with HasAstroNoCParams {
  val io = IO(new Bundle {
    val out = Decoupled(new Flit)
    val rob_ready = Input(Bool())
    val rob_idx = Input(UInt())
    val tsc = Input(UInt(64.W))
    val fire = Output(Bool())
    val n_flits = Output(UInt(flitIdBits.W))
  })

  val flits_left = RegInit(0.U(flitIdBits.W))
  val flits_fired = RegInit(0.U(flitIdBits.W))
  val head_flit = Reg(new Flit)

  val can_fire = (flits_left === 0.U) && io.rob_ready

  val packet_remaining = (LFSR(10) % maxFlits.U)
  io.out.valid := LFSR(10)(2,0) =/= 0.U && flits_left === 0.U && io.rob_ready
  io.out.bits.head := true.B
  io.out.bits.tail := packet_remaining === 0.U
  io.out.bits.prio := prio.U
  io.out.bits.out_id := LFSR(10) % outputNodes.size.U
  io.out.bits.virt_channel_id := idx.U
  io.out.bits.payload := (io.tsc << 16) | (io.rob_idx << 8)

  io.n_flits := packet_remaining + 1.U
  io.fire := can_fire && io.out.fire()

  when (io.fire && !io.out.bits.tail) {
    flits_left := packet_remaining
    head_flit := io.out.bits
    flits_fired := 1.U
  }
  when (flits_left =/= 0.U) {
    io.out.valid := LFSR(10)(2,0) =/= 0.U
    io.out.bits.head := false.B
    io.out.bits.tail := flits_left === 1.U
    io.out.bits.out_id := head_flit.out_id
    io.out.bits.payload := head_flit.payload | flits_fired
    when (io.out.fire()) {
      flits_fired := flits_fired + 1.U
      flits_left := flits_left - 1.U
    }
  }

}

class NoCTester(inputParams: Seq[ChannelParams], outputParams: Seq[ChannelParams])(implicit val p: Parameters) extends Module with HasAstroNoCParams {
  require(flitPayloadBits >= 64)
  val robSz = 128
  val totalTxs = 1000

  val io = IO(new Bundle {
    val to_noc = MixedVec(inputParams.map { u => new IOChannel(u) })
    val from_noc = MixedVec(outputParams.map { u => Flipped(new IOChannel(u)) })
    val success = Output(Bool())
  })

  val nInputs = inputParams.map(_.nVirtualChannels).sum
  val nOutputs = outputParams.map(_.nVirtualChannels).sum

  val txs = RegInit(0.U(32.W))

  val tsc = RegInit(0.U((flitPayloadBits-16).W))
  tsc := tsc + 1.U

  val idle_counter = RegInit(0.U(11.W))
  val idle = Wire(Bool())
  when (idle) { idle_counter := idle_counter + 1.U }
    .otherwise { idle_counter := 0.U }
  assert(!idle_counter(10))


  class RobEntry extends Bundle {
    val payload = UInt(flitPayloadBits.W)
    val out_id = UInt(log2Ceil(nOutputs).W)
    val n_flits = UInt(flitIdBits.W)
    val flits_returned = UInt(flitIdBits.W)
  }

  val rob = Reg(Vec(robSz, new RobEntry))
  val rob_valids = RegInit(0.U(robSz.W))

  val rob_allocs = WireInit(VecInit(Seq.fill(robSz) { false.B }))
  val rob_frees = WireInit(VecInit(Seq.fill(robSz) { false.B }))
  rob_valids := (rob_valids | rob_allocs.asUInt) & ~rob_frees.asUInt
  idle := rob_allocs.asUInt === 0.U && rob_frees.asUInt === 0.U

  val rob_alloc_ids = SelectFirstN(~rob_valids, nInputs).map(i => OHToUInt(i))
  val rob_alloc_avail = rob_alloc_ids.map { i => !rob_valids(i) }

  txs := txs + PopCount(rob_allocs)
  io.success := txs >= totalTxs.U && rob_valids === 0.U

  var idx = 0
  io.to_noc.map { i =>
    i.flits.zipWithIndex.map { case (f,o) =>
      val igen = Module(new InputGen(idx, o))
      igen.io.rob_idx := rob_alloc_ids(idx)
      igen.io.rob_ready := rob_alloc_avail(idx) && (PopCount(~rob_valids) >= nInputs.U) && tsc >= 10.U && txs < totalTxs.U
      igen.io.tsc := tsc
      f <> igen.io.out
      when (igen.io.fire) {
        rob_allocs(rob_alloc_ids(idx)) := true.B
        rob(rob_alloc_ids(idx)).payload := igen.io.out.bits.payload
        rob(rob_alloc_ids(idx)).out_id := igen.io.out.bits.out_id
        rob(rob_alloc_ids(idx)).n_flits := igen.io.n_flits
        rob(rob_alloc_ids(idx)).flits_returned := 0.U
      }
      idx += 1
    }
  }
  require(idx == nInputs)

  idx = 0
  io.from_noc.zipWithIndex map { case (o,i) =>
    o.flits.map { f =>
      f.ready := LFSR(10)(2,0) =/= 0.U
      when (f.fire()) {
        val rob_idx = f.bits.payload(15,8)

        assert(rob_valids(rob_idx) &&
          (rob(rob_idx).payload === f.bits.payload) &&
          (f.bits.out_id === i.U) &&
          (rob(rob_idx).flits_returned < rob(rob_idx).n_flits))

        rob(rob_idx).flits_returned := rob(rob_idx).flits_returned + 1.U
        rob(rob_idx).payload := rob(rob_idx).payload + 1.U

        when (f.bits.tail) {
          rob_frees(rob_idx) := true.B
        }
        idx += 1
      }
    }
  }
  require(idx == nOutputs)
}

class TestHarness(implicit val p: Parameters) extends Module {
  val io = IO(new Bundle {
    val success = Output(Bool())
  })

  val noc = Module(new NoC)
  val noc_tester = Module(new NoCTester(noc.inputParams, noc.outputParams))
  noc.io.in <> noc_tester.io.to_noc
  noc_tester.io.from_noc <> noc.io.out
  io.success := noc_tester.io.success
}
