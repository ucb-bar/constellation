package constellation.test

import chisel3._
import chisel3.util._
import chisel3.util.random.LFSR

import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config.{Field, Parameters, Config}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

import constellation.noc.{NoCParams, HasNoCParams, NoC}
import constellation.channel._
import constellation.router.{HasRouterCtrlConsts}

trait HasSuccessIO { this: Module =>
  val io = IO(new Bundle {
    val success = Output(Bool())
  })
}

object SelectFirstNUInt
{
  def apply(in: UInt, n: Int): (Vec[UInt], Vec[Bool]) = {
    val sels = Wire(Vec(n, UInt(log2Ceil(in.getWidth).W)))
    val fires = Wire(Vec(n, Bool()))
    var mask = in
    for (i <- 0 until n) {
      sels(i) := PriorityEncoder(mask)
      fires(i) := mask =/= 0.U
      mask = mask & ~(1.U << sels(i))
    }
    (sels, fires)
  }
}

case class NoCTesterParams(
  nocParams: NoCParams = NoCParams(),
  robSz: Int = 128,
  totalTxs: Int = 50000,
  inputFlitStallProbability: Double = 0.0,
  inputPacketStallProbability: Double = 0.0,
  outputStallProbability: Double = 0.0,
  maxFlits: Int = 8,
  constPacketSize: Boolean = false
)

case object NoCTesterKey extends Field[NoCTesterParams](NoCTesterParams())

class Payload extends Bundle {
  val tsc         = UInt(32.W)
  val rob_idx     = UInt(16.W)
  val flits_fired = UInt(16.W)
}

class InputGen(idx: Int, cParams: IngressChannelParams)
  (implicit val p: Parameters) extends Module with HasNoCParams {
  val maxFlits = p(NoCTesterKey).maxFlits
  val inputFlitStallProbability = p(NoCTesterKey).inputFlitStallProbability
  val inputPacketStallProbability = p(NoCTesterKey).inputPacketStallProbability
  val flitIdBits = log2Ceil(maxFlits+1)
  val io = IO(new Bundle {
    val out = Irrevocable(new IngressFlit(cParams.payloadBits))
    val rob_ready = Input(Bool())
    val rob_idx = Input(UInt())
    val tsc = Input(UInt(32.W))
    val fire = Output(Bool())
    val n_flits = Output(UInt(flitIdBits.W))
  })

  val flits_left = RegInit(0.U(flitIdBits.W))
  val flits_fired = RegInit(0.U(flitIdBits.W))
  val egress = Reg(UInt(log2Ceil(nEgresses).W))
  val payload = Reg(new Payload)

  val can_fire = (flits_left === 0.U) && io.rob_ready

  val packet_remaining = if (p(NoCTesterKey).constPacketSize) maxFlits.U else (LFSR(20) % maxFlits.U)
  val random_flit_delay = (LFSR(20) < (inputFlitStallProbability * (1 << 20)).toInt.U)
  val random_packet_delay = (LFSR(20) < (inputPacketStallProbability * (1 << 20)).toInt.U)
  io.out.valid := !random_packet_delay && flits_left === 0.U && io.rob_ready
  io.out.bits.head := true.B
  io.out.bits.tail := packet_remaining === 0.U
  io.out.bits.egress_id := VecInit(cParams.possibleFlows.toSeq.map(_.egressId.U))(LFSR(20) % cParams.possibleFlows.size.U)
  val out_payload = Wire(new Payload)
  io.out.bits.payload := out_payload.asUInt
  out_payload.tsc := io.tsc
  out_payload.rob_idx := io.rob_idx
  out_payload.flits_fired := 0.U

  io.n_flits := packet_remaining + 1.U
  io.fire := can_fire && io.out.fire()

  when (io.fire && !io.out.bits.tail) {
    flits_left := packet_remaining
    payload := out_payload
    egress := io.out.bits.egress_id
    flits_fired := 1.U
  }
  when (flits_left =/= 0.U) {
    io.out.valid := !random_flit_delay
    io.out.bits.head := false.B
    io.out.bits.tail := flits_left === 1.U
    io.out.bits.egress_id := egress
    out_payload := payload
    out_payload.flits_fired := flits_fired

    when (io.out.fire()) {
      flits_fired := flits_fired + 1.U
      flits_left := flits_left - 1.U
    }
  }

}

class NoCTester(inputParams: Seq[IngressChannelParams], outputParams: Seq[EgressChannelParams])(implicit val p: Parameters) extends Module with HasNoCParams {
  val allPayloadBits = (inputParams.map(_.payloadBits) ++ outputParams.map(_.payloadBits)).toSet
  require(allPayloadBits.size == 1 && allPayloadBits.head >= 64)
  val payloadBits = allPayloadBits.head

  val robSz = p(NoCTesterKey).robSz
  val totalTxs = p(NoCTesterKey).totalTxs
  val outputStallProbability = p(NoCTesterKey).outputStallProbability
  val maxFlits = p(NoCTesterKey).maxFlits
  val flitIdBits = log2Ceil(maxFlits+1)

  val io = IO(new Bundle {
    val to_noc = MixedVec(inputParams.map { u => new IngressChannel(u) })
    val from_noc = MixedVec(outputParams.map { u => Flipped(new EgressChannel(u)) })
    val success = Output(Bool())
  })

  val nInputs = inputParams.map(_.nVirtualChannels).sum
  val nOutputs = outputParams.map(_.nVirtualChannels).sum

  val txs = RegInit(0.U(32.W))
  val flits = RegInit(0.U(32.W))
  dontTouch(flits)


  val tsc = RegInit(0.U(32.W))
  tsc := tsc + 1.U

  val idle_counter = RegInit(0.U(11.W))
  val idle = Wire(Bool())
  when (idle) { idle_counter := idle_counter + 1.U }
    .otherwise { idle_counter := 0.U }
  assert(!idle_counter(10))

  val rob_payload = Reg(Vec(robSz, new Payload))
  val rob_egress_id = Reg(Vec(robSz, UInt(log2Ceil(nOutputs).W)))
  val rob_ingress_id = Reg(Vec(robSz, UInt(log2Ceil(nInputs).W)))
  val rob_n_flits = Reg(Vec(robSz, UInt(flitIdBits.W)))
  val rob_flits_returned = Reg(Vec(robSz, UInt(flitIdBits.W)))
  val rob_tscs = Reg(Vec(robSz, UInt(64.W)))
  val rob_valids = RegInit(0.U(robSz.W))
  var rob_allocs = 0.U(robSz.W)
  var rob_frees = 0.U(robSz.W)


  val (rob_alloc_ids, rob_alloc_fires) = SelectFirstNUInt(~rob_valids, nInputs)
  val rob_alloc_avail = rob_alloc_ids.map { i => !rob_valids(i) }
  val success = txs >= totalTxs.U && rob_valids === 0.U
  io.success := RegNext(success, false.B)
  when (success) {
    printf("%d flits, %d cycles\n", flits, tsc)
  }

  val tx_fire = Wire(Vec(nInputs, Bool()))
  io.to_noc.zipWithIndex.map { case (i,idx) =>
    val igen = Module(new InputGen(idx, inputParams(idx)))
    val rob_idx = WireInit(rob_alloc_ids(idx))
    igen.io.rob_idx := rob_idx
    igen.io.rob_ready := (rob_alloc_avail(idx) && rob_alloc_fires(idx) &&
      tsc >= 10.U && txs < totalTxs.U)
    igen.io.tsc := tsc
    i.flit <> Queue(igen.io.out, 1, pipe=true, flow=true)
    when (igen.io.fire) {
      rob_payload        (rob_idx) := igen.io.out.bits.payload.asTypeOf(new Payload)
      rob_egress_id      (rob_idx) := igen.io.out.bits.egress_id
      rob_ingress_id     (rob_idx) := idx.U
      rob_n_flits        (rob_idx) := igen.io.n_flits
      rob_flits_returned (rob_idx) := 0.U
      rob_tscs           (rob_idx) := tsc
    }
    tx_fire(idx) := igen.io.fire
    rob_allocs = rob_allocs | (igen.io.fire << rob_idx)
  }

  val enable_print_latency = PlusArg("noctest_enable_print", default=0, width=1)(0)

  io.from_noc.zipWithIndex map { case (o,i) =>
    o.flit.ready := LFSR(20) >= (outputStallProbability * (1 << 10)).toInt.U
    val out_payload = o.flit.bits.payload.asTypeOf(new Payload)
    val rob_idx = out_payload.rob_idx
    val packet_valid = RegInit(false.B)
    val packet_rob_idx = Reg(UInt(log2Ceil(robSz).W))

    when (o.flit.fire()) {

      assert(rob_valids(rob_idx), cf"out[${i.toString}] unexpected response")
      assert(rob_payload(rob_idx).asUInt === o.flit.bits.payload.asUInt, cf"out[${i.toString}] incorrect payload");
      assert(o.flit.bits.ingress_id === rob_ingress_id(rob_idx), cf"out[${i.toString}] incorrect source")
      assert(i.U === rob_egress_id(rob_idx), cf"out[${i.toString}] incorrect destination")
      assert(rob_flits_returned(rob_idx) < rob_n_flits(rob_idx), cf"out[${i.toString}] too many flits returned")
      assert((!packet_valid && o.flit.bits.head) || rob_idx === packet_rob_idx)

      when (o.flit.bits.head && enable_print_latency) {
        val fmtStr = s"%d, $i, %d\n"
        printf(fmtStr, rob_ingress_id(rob_idx), tsc - out_payload.tsc)
      }

      rob_flits_returned(rob_idx) := rob_flits_returned(rob_idx) + 1.U
      rob_payload(rob_idx).flits_fired := rob_payload(rob_idx).flits_fired + 1.U
      when (o.flit.bits.head) { packet_valid := true.B; packet_rob_idx := rob_idx }
      when (o.flit.bits.tail) { packet_valid := false.B }
    }
    rob_frees = rob_frees | ((o.flit.fire() && o.flit.bits.tail) << rob_idx)
  }


  rob_valids := (rob_valids | rob_allocs) & ~rob_frees
  idle := rob_allocs === 0.U && rob_frees === 0.U
  flits := flits + io.from_noc.map(_.flit.fire().asUInt).reduce(_+&_)
  txs := txs + PopCount(tx_fire)

  for (i <- 0 until robSz) {
    when (rob_valids(i)) {
      assert(tsc - rob_tscs(i) < (16 << 10).U, cf"ROB Entry ${i.toString} took too long")
    }
  }
}



class TestHarness(implicit val p: Parameters) extends Module with HasRouterCtrlConsts with HasSuccessIO {
  val lazyNoC = LazyModule(new NoC(p(NoCTesterKey).nocParams))
  val noc = Module(lazyNoC.module)
  noc.io.router_clocks.foreach(_.clock := clock)
  noc.io.router_clocks.foreach(_.reset := reset)

  noc.io.router_ctrl.foreach{ ctrl =>
    val read = RegInit(false.B)
    val addr = RegInit(0.U(CTRL_ADDR_SZ.W))
    addr := Mux(addr === CTRL_MAX_ADDR.U, 0.U, addr + 1.U)
    when (addr === CTRL_MAX_ADDR.U) { read := true.B }

    ctrl.enable := read
    ctrl.write  := !read
    ctrl.addr   := addr
    ctrl.wdata  := DontCare

    def inRange(i: UInt, lower: Int, bound: Int) = {
      i >= lower.U && i < (lower + bound).U
    }

    when (addr === CTRL_EPOCH_CYCLES.U) {
      ctrl.enable := true.B
      ctrl.wdata  := 100.U
    } .elsewhen (inRange(addr, CTRL_IN_RATELIMITER_OFFSET, CTRL_MAX_INS)) {
      ctrl.enable := true.B
      ctrl.wdata  := 1.U
    } .elsewhen (inRange(addr, CTRL_IN_RATE_OFFSET, CTRL_MAX_INS)) {
      ctrl.enable := true.B
      ctrl.wdata  := 75.U
    }
  }

  val noc_tester = Module(new NoCTester(lazyNoC.allIngressParams, lazyNoC.allEgressParams)(lazyNoC.iP))
  noc.io.ingress <> noc_tester.io.to_noc
  noc_tester.io.from_noc <> noc.io.egress
  io.success := noc_tester.io.success
  ElaborationArtefacts.add("plusArgs", PlusArgArtefacts.serialize_cHeader)
}
