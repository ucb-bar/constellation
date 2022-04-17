package constellation.test

import chisel3._
import chisel3.util._
import chisel3.util.random.LFSR
import chisel3.experimental.IntParam

import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config.{Field, Parameters, Config}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

import constellation.noc.{NoCKey, HasNoCParams, NoC}
import constellation.channel._
import constellation.rc.{TLNoC, TLNoCParams}
import constellation.router.{HasRouterCtrlConsts}

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
  robSz: Int = 128,
  totalTxs: Int = 50000,
  inputStallProbability: Double = 0.0,
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

class InputGen(idx: Int, cParams: IngressChannelParams)(implicit val p: Parameters) extends Module with HasNoCParams {
  val maxFlits = p(NoCTesterKey).maxFlits
  val inputStallProbability = p(NoCTesterKey).inputStallProbability
  val flitIdBits = log2Ceil(maxFlits+1)
  val io = IO(new Bundle {
    val out = Decoupled(new IngressFlit(cParams))
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
  val random_delay = LFSR(20) < (inputStallProbability * (1 << 20)).toInt.U
  io.out.valid := !random_delay && flits_left === 0.U && io.rob_ready
  io.out.bits.head := true.B
  io.out.bits.tail := packet_remaining === 0.U
  io.out.bits.egress_id := LFSR(20) % nEgresses.U
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
    io.out.valid := !random_delay
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

  val io = IO(new Bundle { // C++ model should keep this IO, everything else becomes verilog though
    val to_noc = MixedVec(inputParams.map { u => new IngressChannel(u) })
    val from_noc = MixedVec(outputParams.map { u => Flipped(new EgressChannel(u)) })
    val success = Output(Bool())
  })

  /**
   * injectionRate: number of packets to inject per 100 cycles
   */
  class BlackBoxIngressUnit(ingressId: Int, injectionRate: Int)
    extends BlackBox(Map(
      "NUM_INGRESSES" -> inputParams.length
      "INGRESS_ID" -> IntParam(ingressId)
      "INJECTION_RATE" -> IntParam(injectionRate) // TODO (ANIMESH) -- THIS DOESNT WORK
      "NUM_FLITS" -> p(NoCTesterKey).maxFlits
      ) {
    val io = IO(new Bundle {
      val clock = Input(Clock())
      val reset = Input(Bool())

      // see verilog for output pseudocode

    })
    addResource("/vsrc/IngressUnit.v")
    addResource("/csrc/IngressUnit.cpp")
  }

}



class TestHarness(implicit val p: Parameters) extends Module with HasRouterCtrlConsts {
  val io = IO(new Bundle {
    val success = Output(Bool())
  })
  val lazyNoC = LazyModule(new NoC)
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
