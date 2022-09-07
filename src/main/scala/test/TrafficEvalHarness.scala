package constellation.test

import chisel3._
import chisel3.experimental.IntParam
import chisel3.util._
import chisel3.util.random.LFSR

import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config.{Field, Parameters, Config}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

import constellation.noc.{NoCParams, HasNoCParams, NoC}
import constellation.channel._
import constellation.router.{HasRouterCtrlConsts}

import scala.collection.immutable.ListMap

class TrafficEvalIngress(ingress_id: Int, config_str: String) extends BlackBox(Map(
  "INGRESS_ID" -> IntParam(ingress_id),
  "CONFIG_STR" -> config_str
))
    with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val current_cycle = Input(UInt(64.W))
    val flit_out = new Bundle {
      val ready = Input(Bool())
      val valid = Output(Bool())
      val head = Output(Bool())
      val tail = Output(Bool())
      val egress_id = Output(UInt(64.W))
      val unique_id = Output(UInt(64.W))
    }
  })
  addResource("/csrc/netrace/netrace.h")
  addResource("/vsrc/TrafficEval.v")
  addResource("/csrc/TrafficEval.cpp")
  addResource("/csrc/TrafficEval.h")
}

class TrafficEvalEgress(egress_id: Int, config_str: String) extends BlackBox(Map(
  "EGRESS_ID" -> IntParam(egress_id),
  "CONFIG_STR" -> config_str
))
    with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val current_cycle = Input(UInt(64.W))
    val flit_in = new Bundle {
      val ready = Output(Bool())
      val valid = Input(Bool())
      val head = Input(Bool())
      val tail = Input(Bool())
      val ingress_id = Input(UInt(64.W))
      val unique_id = Input(UInt(64.W))
    }
    val success = Output(Bool())
    val fatal = Output(Bool())
  })
  addResource("/csrc/netrace/netrace.h")
  addResource("/vsrc/TrafficEval.v")
  addResource("/csrc/TrafficEval.cpp")
  addResource("/csrc/TrafficEval.h")
}


case class NoCEvalParams(
  nocParams: NoCParams = NoCParams(),
  warmupCycles: Int = 5000,
  measurementCycles: Int = 20000,
  drainTimeoutCycles: Int = 100000,
  flitsPerPacket: Int = 4,
  flows: (Int, Int) => Double = (a: Int, b: Int) => 0.0,
  requiredThroughput: Double = 0.0,
  requiredMedianLatency: Int = 99999,
  requiredMaxLatency: Int = 99999,
  netraceEnable: Boolean = false,
  netraceRegion: Int = 2, // this is the PARSEC region-of-interest
  netraceTrace: String = "blackscholes_64c_simsmall.tra.bz2",
  netraceIgnoreDependencies: Boolean = false
) {
  def toConfigStr = s"""# Default generated trafficeval config
warmup                  $warmupCycles
measurement             $measurementCycles
drain                   $drainTimeoutCycles
flits_per_packet        $flitsPerPacket
required_throughput     $requiredThroughput
required_median_latency $requiredMedianLatency
required_max_latency    $requiredMaxLatency
netrace_enable          $netraceEnable
netrace_trace           $netraceTrace
netrace_region          $netraceRegion
netrace_ignore_dependencies $netraceIgnoreDependencies
""" + nocParams.flows.map { f =>
    s"flow             ${f.ingressId} ${f.egressId} ${flows(f.ingressId, f.egressId)}"
  }.mkString("\n")
}

case object NoCEvalKey extends Field[NoCEvalParams](NoCEvalParams())

class EvalHarness(implicit val p: Parameters) extends Module with HasSuccessIO {
  val lazyNoC = LazyModule(new NoC(p(NoCEvalKey).nocParams))
  val noc = Module(lazyNoC.module)
  noc.io.router_clocks.foreach(_.clock := clock)
  noc.io.router_clocks.foreach(_.reset := reset)

  noc.io.router_ctrl.foreach{ ctrl =>
    ctrl.enable := false.B
    ctrl.write := DontCare
    ctrl.addr := DontCare
    ctrl.wdata := DontCare
  }
  io.success := false.B
  val cycle = RegInit(0.U(64.W))
  cycle := cycle + 1.U

  val configStr = p(NoCEvalKey).toConfigStr
  ElaborationArtefacts.add("noceval.cfg", configStr)

  noc.io.ingress.zipWithIndex.map { case (in,i) =>
    val ingress = Module(new TrafficEvalIngress(i, configStr))
    ingress.io.clock := clock
    ingress.io.reset := reset
    ingress.io.current_cycle := cycle

    // This queue handles the delayed response from the ingress unit
    // and restores the decoupled handshake
    val flit_q = Module(new Queue(in.flit.bits.cloneType, 1, flow=true, pipe=true))
    ingress.io.flit_out.ready := flit_q.io.count === 0.U && in.flit.ready
    flit_q.io.enq.valid := ingress.io.flit_out.valid
    flit_q.io.enq.bits.head := ingress.io.flit_out.head
    flit_q.io.enq.bits.tail := ingress.io.flit_out.tail
    flit_q.io.enq.bits.egress_id := ingress.io.flit_out.egress_id
    flit_q.io.enq.bits.payload := ingress.io.flit_out.unique_id

    in.flit <> flit_q.io.deq
  }
  noc.io.egress.zipWithIndex.map { case (out,i) =>
    val egress = Module(new TrafficEvalEgress(i, configStr))
    egress.io.clock := clock
    egress.io.reset := reset
    egress.io.current_cycle := cycle
    out.flit.ready := egress.io.flit_in.ready
    egress.io.flit_in.valid := out.flit.valid
    egress.io.flit_in.head := out.flit.bits.head
    egress.io.flit_in.tail := out.flit.bits.tail
    egress.io.flit_in.ingress_id := out.flit.bits.ingress_id
    egress.io.flit_in.unique_id := out.flit.bits.payload

    when (egress.io.success) { io.success := true.B }
    assert(!egress.io.fatal)
  }

  ElaborationArtefacts.add("plusArgs", PlusArgArtefacts.serialize_cHeader)
}
