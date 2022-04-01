package constellation.test

import chisel3._
import chisel3.util._
import chisel3.util.random.LFSR

import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config.{Field, Parameters, Config}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

import constellation.noc.{NoCKey, HasNoCParams, NoC}
import constellation.channel._
import constellation.rc.{TLNoC, TLNoCParams}


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

class InputGen(idx: Int, cParams: IngressChannelParams, inputStallProbability: Double, maxFlits: Int)(implicit val p: Parameters) extends Module with HasNoCParams {
  val flitIdBits = log2Ceil(maxFlits+1)
  val io = IO(new Bundle {
    val out = Decoupled(new IOFlit(cParams))
    val rob_ready = Input(Bool())
    val rob_idx = Input(UInt())
    val tsc = Input(UInt(64.W))
    val fire = Output(Bool())
    val n_flits = Output(UInt(flitIdBits.W))
  })

  val flits_left = RegInit(0.U(flitIdBits.W))
  val flits_fired = RegInit(0.U(flitIdBits.W))
  val head_flit = Reg(new IOFlit(cParams))

  val can_fire = (flits_left === 0.U) && io.rob_ready

  val packet_remaining = (LFSR(20) % maxFlits.U)
  val random_delay = LFSR(20) < (inputStallProbability * (1 << 10)).toInt.U
  io.out.valid := !random_delay && flits_left === 0.U && io.rob_ready
  io.out.bits.head := true.B
  io.out.bits.tail := packet_remaining === 0.U
  io.out.bits.egress_id := LFSR(20) % nEgresses.U
  io.out.bits.payload := (io.tsc << 16) | (io.rob_idx << 8)

  io.n_flits := packet_remaining + 1.U
  io.fire := can_fire && io.out.fire()

  when (io.fire && !io.out.bits.tail) {
    flits_left := packet_remaining
    head_flit := io.out.bits
    flits_fired := 1.U
  }
  when (flits_left =/= 0.U) {
    io.out.valid := !random_delay
    io.out.bits.head := false.B
    io.out.bits.tail := flits_left === 1.U
    io.out.bits.egress_id := head_flit.egress_id
    io.out.bits.payload := head_flit.payload | flits_fired
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

  val robSz = 128
  val totalTxs = 50000
  val inputStallProbability = 0.0
  val outputStallProbability = 0.0
  val maxFlits = 8
  val flitIdBits = log2Ceil(maxFlits+1)

  val io = IO(new Bundle {
    val to_noc = MixedVec(inputParams.map { u => new TerminalChannel(u) })
    val from_noc = MixedVec(outputParams.map { u => Flipped(new TerminalChannel(u)) })
    val success = Output(Bool())
  })

  val nInputs = inputParams.map(_.nVirtualChannels).sum
  val nOutputs = outputParams.map(_.nVirtualChannels).sum

  val txs = RegInit(0.U(32.W))
  val flits = RegInit(0.U(32.W))
  dontTouch(flits)

  val tsc = RegInit(0.U((payloadBits-16).W))
  tsc := tsc + 1.U

  val idle_counter = RegInit(0.U(11.W))
  val idle = Wire(Bool())
  when (idle) { idle_counter := idle_counter + 1.U }
    .otherwise { idle_counter := 0.U }
  assert(!idle_counter(10))


  class RobEntry extends Bundle {
    val payload = UInt(payloadBits.W)
    val egress_id = UInt(log2Ceil(nOutputs).W)
    val n_flits = UInt(flitIdBits.W)
    val flits_returned = UInt(flitIdBits.W)
  }

  val rob_payload = Reg(Vec(robSz, UInt(payloadBits.W)))
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
    val igen = Module(new InputGen(idx, inputParams(idx), inputStallProbability, maxFlits))
    val rob_idx = WireInit(rob_alloc_ids(idx))
    igen.io.rob_idx := rob_idx
    igen.io.rob_ready := (rob_alloc_avail(idx) && rob_alloc_fires(idx) &&
      tsc >= 10.U && txs < totalTxs.U)
    igen.io.tsc := tsc
    i.flit <> igen.io.out
    when (igen.io.fire) {
      rob_payload        (rob_idx) := igen.io.out.bits.payload
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
    val rob_idx = o.flit.bits.payload(15,8)
    val packet_valid = RegInit(false.B)
    val packet_rob_idx = Reg(UInt(log2Ceil(robSz).W))

    when (o.flit.fire()) {

      assert(rob_valids(rob_idx), s"out[$i] unexpected response")
      assert(rob_payload(rob_idx) === o.flit.bits.payload, s"out[$i] incorrect payload");
      assert(o.flit.bits.egress_id === i.U && o.flit.bits.egress_id === rob_egress_id(rob_idx), s"out[$i] incorrect destination")
      assert(rob_flits_returned(rob_idx) < rob_n_flits(rob_idx), s"out[$i] too many flits returned")
      assert((!packet_valid && o.flit.bits.head) || rob_idx === packet_rob_idx)

      when (o.flit.bits.head && enable_print_latency) {
        printf(s"%d, $i, %d\n", rob_ingress_id(rob_idx), tsc - (o.flit.bits.payload >> 16))
      }

      rob_flits_returned(rob_idx) := rob_flits_returned(rob_idx) + 1.U
      rob_payload(rob_idx) := rob_payload(rob_idx) + 1.U
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
      assert(tsc - rob_tscs(i) < (16 << 10).U, s"ROB Entry $i took too long")
    }
  }
}



class TestHarness(implicit val p: Parameters) extends Module {
  val io = IO(new Bundle {
    val success = Output(Bool())
  })
  val lazyNoC = LazyModule(new NoC)
  val noc = Module(lazyNoC.module)
  noc.io.router_clocks.foreach(_.clock := clock)
  noc.io.router_clocks.foreach(_.reset := reset)
  val noc_tester = Module(new NoCTester(lazyNoC.allIngressParams, lazyNoC.allEgressParams))
  noc.io.ingress <> noc_tester.io.to_noc
  noc_tester.io.from_noc <> noc.io.egress
  io.success := noc_tester.io.success
  ElaborationArtefacts.add("plusArgs", PlusArgArtefacts.serialize_cHeader)
}

case class TLNoCTesterParams(
  inNodeMapping: Seq[Int],
  outNodeMapping: Seq[Int],
  txns: Int = 1000
)

case object TLNoCTesterKey extends Field[TLNoCTesterParams](TLNoCTesterParams(Nil, Nil))

class WithTLNoCTesterParams(p: TLNoCTesterParams) extends Config((site, here, up) => {
  case TLNoCTesterKey => p
})

class TLNoCTester(implicit p: Parameters) extends LazyModule {
  val tParams = p(TLNoCTesterKey)
  val txns = tParams.txns
  val inNodeMapping = tParams.inNodeMapping
  val outNodeMapping = tParams.outNodeMapping
  val nManagers = outNodeMapping.size
  val nClients = inNodeMapping.size
  val xbar = LazyModule(new TLNoC(TLNoCParams("test", inNodeMapping, outNodeMapping, Some(p(NoCKey)))))

  val fuzzers = (0 until nClients) map { n =>
    val fuzz = LazyModule(new TLFuzzer(txns))
    xbar.node := TLDelayer(0.1) := fuzz.node
    fuzz
  }

  (0 until nManagers) foreach { n =>
    val ram = LazyModule(new TLRAM(AddressSet(0x0+0x400*n, 0x3ff)))
    DisableMonitors { implicit p => ram.node := TLFragmenter(4, 256) } := TLDelayer(0.1) := xbar.node
  }

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val finished = Output(Bool())
    })
    io.finished := fuzzers.last.module.io.finished
  }
}

class TLTestHarness(implicit val p: Parameters) extends Module {
  val io = IO(new Bundle {
    val success = Output(Bool())
  })
  val tester = Module(LazyModule(new TLNoCTester).module)
  io.success := tester.io.finished

  // Dummy plusarg to avoid breaking verilator builds with emulator.cc
  val useless_plusarg = PlusArg("useless_plusarg", width=1)
  dontTouch(useless_plusarg)
  ElaborationArtefacts.add("plusArgs", PlusArgArtefacts.serialize_cHeader)
}
