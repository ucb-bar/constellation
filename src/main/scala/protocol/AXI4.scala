package constellation.protocol

import chisel3._
import chisel3.util._

import constellation.channel._
import constellation.noc._

import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.amba.axi4._

trait AXI4FieldHelper {
  val wideBundle: AXI4BundleParameters
}
class AXI4MasterToNoC(
  edgeIn: AXI4EdgeParameters, edgesOut: Seq[AXI4EdgeParameters],
  sourceStart: Int, sourceSize: Int,
  wideBundle: AXI4BundleParameters,
  slaveToEgressOffset: Int => Int,
  flitWidth: Int,
  awQueueDepth: Int
)(implicit p: Parameters) extends Module {
  // Can only keep 1 in-flight per ID because NoC can't guarantee
  // FIFO between bursts with the same ID
  val maxFlightPerId = 1

  val io = IO(new Bundle {
    val axi4 = Flipped(new AXI4Bundle(wideBundle))
    val flits = new Bundle {
      val aw = Decoupled(new IngressFlit(flitWidth))
      val w = Decoupled(new IngressFlit(flitWidth))
      val ar = Decoupled(new IngressFlit(flitWidth))
      val r = Flipped(Decoupled(new EgressFlit(flitWidth)))
      val b = Flipped(Decoupled(new EgressFlit(flitWidth)))
    }
  })

  val port_addrs = edgesOut.map(_.slave.slaves.map(_.address).flatten)
  val routingMask = AddressDecoder(port_addrs)
  val route_addrs = port_addrs.map(seq => AddressSet.unify(seq.map(_.widen(~routingMask)).distinct))
  val outputPorts = route_addrs.map(seq => (addr: UInt) => seq.map(_.contains(addr)).reduce(_||_))

  // To route W need to record where AW went
  class AWInBundle extends Bundle {
    val out = UInt(edgesOut.size.W)
    val id = UInt(wideBundle.idBits.W)
  }
  val awIn = Module(new Queue(new AWInBundle, awQueueDepth, flow=true))

  val requestARIO = VecInit(outputPorts.map { o => o(io.axi4.ar.bits.addr) })
  val requestAWIO = VecInit(outputPorts.map { o => o(io.axi4.aw.bits.addr) })

  // W follows path dicated by AW Q
  awIn.io.enq.bits.out := requestAWIO.asUInt
  awIn.io.enq.bits.id  := io.axi4.aw.bits.id | sourceStart.U
  val requestWIO = awIn.io.deq.bits.out.asBools

  val in = Wire(new AXI4Bundle(wideBundle))
  in :<> io.axi4

  // Handle size = 1 gracefully (Chisel3 empty range is broken)
  def trim(id: UInt, size: Int) = if (size <= 1) 0.U else id(log2Ceil(size)-1, 0)

  // Manipulate the AXI IDs to differentiate masters
  in.aw.bits.id := io.axi4.aw.bits.id | sourceStart.U
  in.ar.bits.id := io.axi4.ar.bits.id | sourceStart.U
  io.axi4.r.bits.id := trim(in.r.bits.id, sourceSize)
  io.axi4.b.bits.id := trim(in.b.bits.id, sourceSize)

  // Block A[RW] if we switch ports, to ensure responses stay ordered (also: beware the dining philosophers)
  val endId = edgeIn.master.endId
  val arFIFOMap = WireInit(VecInit(Seq.fill(endId) { true.B }))
  val awFIFOMap = WireInit(VecInit(Seq.fill(endId) { true.B }))
  val arSel = UIntToOH(io.axi4.ar.bits.id, endId)
  val awSel = UIntToOH(io.axi4.aw.bits.id, endId)
  val rSel  = UIntToOH(io.axi4.r .bits.id, endId)
  val bSel  = UIntToOH(io.axi4.b .bits.id, endId)
  val arTag = OHToUInt(requestARIO.asUInt, edgesOut.size)
  val awTag = OHToUInt(requestAWIO.asUInt, edgesOut.size)

  for (master <- edgeIn.master.masters) {
    def idTracker(port: UInt, req_fire: Bool, resp_fire: Bool) = {
      if (master.maxFlight == Some(0)) {
        true.B
      } else {
        val legalFlight = master.maxFlight.getOrElse(maxFlightPerId+1)
        val flight = legalFlight min maxFlightPerId
        val canOverflow = legalFlight > flight
        val count = RegInit(0.U(log2Ceil(flight+1).W))
        val last = Reg(UInt(log2Ceil(edgesOut.size).W))
        count := count + req_fire.asUInt - resp_fire.asUInt
        assert (!resp_fire || count =/= 0.U)
        assert (!req_fire  || count =/= flight.U)
        when (req_fire) { last := port }
        // No need to track where it went if we cap it at 1 request
        val portMatch = if (flight == 1) { true.B } else { last === port }
          (count === 0.U || portMatch) && (!canOverflow.B || count =/= flight.U)
      }
    }

    for (id <- master.id.start until master.id.end) {
      arFIFOMap(id) := idTracker(
        arTag,
        arSel(id) && io.axi4.ar.fire(),
        rSel(id) && io.axi4.r.fire() && io.axi4.r.bits.last)
      awFIFOMap(id) := idTracker(
        awTag,
        awSel(id) && io.axi4.aw.fire(),
        bSel(id) && io.axi4.b.fire())
    }
  }

  val allowAR = arFIFOMap(io.axi4.ar.bits.id)
  in.ar.valid := io.axi4.ar.valid && allowAR
  io.axi4.ar.ready := in.ar.ready && allowAR

  // Block AW if we cannot record the W destination
  val allowAW = awFIFOMap(io.axi4.aw.bits.id)
  in.aw.valid := io.axi4.aw.valid && awIn.io.enq.ready && allowAW
  io.axi4.aw.ready := in.aw.ready && awIn.io.enq.ready && allowAW
  awIn.io.enq.valid := io.axi4.aw.valid && in.aw.ready && allowAW

  // Block W if we do not have an AW destination
  in.w.valid := io.axi4.w.valid && awIn.io.deq.valid
  io.axi4.w.ready := in.w.ready && awIn.io.deq.valid
  awIn.io.deq.ready := io.axi4.w.valid && io.axi4.w.bits.last && in.w.ready

  io.flits.aw.valid := in.aw.valid
  in.aw.ready := io.flits.aw.ready
  io.flits.aw.bits.head := true.B
  io.flits.aw.bits.tail := true.B
  io.flits.aw.bits.payload := in.aw.bits.asUInt
  io.flits.aw.bits.egress_id := Mux1H(requestAWIO.zipWithIndex.map { case (r, i) =>
    r -> (slaveToEgressOffset(i) + 0).U
  })

  val w_first = RegInit(true.B)
  io.flits.w.valid := in.w.valid
  in.w.ready := io.flits.w.ready
  io.flits.w.bits.head := w_first
  io.flits.w.bits.tail := in.w.bits.last
  io.flits.w.bits.payload := Cat(awIn.io.deq.bits.id, in.w.bits.asUInt)
  io.flits.w.bits.egress_id := Mux1H(requestWIO.zipWithIndex.map { case (r, i) =>
    r -> (slaveToEgressOffset(i) + 1).U
  })
  when (io.flits.w.fire()) { w_first := in.w.bits.last }

  io.flits.ar.valid := in.ar.valid
  in.ar.ready := io.flits.ar.ready
  io.flits.ar.bits.head := true.B
  io.flits.ar.bits.tail := true.B
  io.flits.ar.bits.payload := in.ar.bits.asUInt
  io.flits.ar.bits.egress_id := Mux1H(requestARIO.zipWithIndex.map { case (r, i) =>
    r -> (slaveToEgressOffset(i) + 2).U
  })

  in.b.valid := io.flits.b.valid
  io.flits.b.ready := in.b.ready
  in.b.bits := io.flits.b.bits.payload.asTypeOf(in.b.bits)

  in.r.valid := io.flits.r.valid
  io.flits.r.ready := in.r.ready
  in.r.bits := io.flits.r.bits.payload.asTypeOf(in.r.bits)
}

class AXI4SlaveToNoC(
  edgeOut: AXI4EdgeParameters, edgesIn: Seq[AXI4EdgeParameters],
  wideBundle: AXI4BundleParameters,
  masterToEgressOffset: Int => Int,
  flitWidth: Int,
  awQueueDepth: Int
)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val axi4 = new AXI4Bundle(wideBundle)
    val flits = new Bundle {
      val aw = Flipped(Decoupled(new EgressFlit(flitWidth)))
      val w = Flipped(Decoupled(new EgressFlit(flitWidth)))
      val ar = Flipped(Decoupled(new EgressFlit(flitWidth)))
      val r = Decoupled(new IngressFlit(flitWidth))
      val b = Decoupled(new IngressFlit(flitWidth))
    }
  })
  val out = Wire(new AXI4Bundle(wideBundle))
  val out_w_in_head = Wire(Bool())
  val out_w_in_id = Wire(UInt((1 << wideBundle.idBits).W))
  io.axi4 :<> out

  // Grab the port ID mapping
  val inputIdRanges = AXI4Xbar.mapInputIds(edgesIn.map(_.master))

  val requestROI  = inputIdRanges.map { i => i.contains(io.axi4.r.bits.id) }
  val requestBOI  = inputIdRanges.map { i => i.contains(io.axi4.b.bits.id) }

  // Store the incoming AW messages in a buffer, 1 entry per source ID
  // Order the AW/W based on the arriving order of W bursts
  val aw_val = RegInit(VecInit(0.U((1 << wideBundle.idBits).W).asBools))
  val aw_buf = Reg(Vec(1 << wideBundle.idBits, new AXI4BundleAW(wideBundle)))
  val w_fire = RegInit(false.B)
  val aw_q   = Module(new Queue(new AXI4BundleAW(wideBundle), awQueueDepth, pipe=true))

  io.axi4.aw <> aw_q.io.deq

  out.aw.ready       := !aw_val(out.aw.bits.id)
  when (out.aw.fire()) {
    aw_val(out.aw.bits.id) := true.B
    aw_buf(out.aw.bits.id) := out.aw.bits
  }

  aw_q.io.enq.valid := out.w.valid && Mux1H(out_w_in_id, aw_val) && out_w_in_head
  aw_q.io.enq.bits  :=                Mux1H(out_w_in_id, aw_buf)
  when (aw_q.io.enq.fire()) {
    aw_val(OHToUInt(out_w_in_id)) := false.B
    w_fire := true.B
  }

  out.w.ready        := io.axi4.w.ready && (
    w_fire || (out_w_in_head && aw_q.io.enq.ready && Mux1H(out_w_in_id, aw_val))
  )
  io.axi4.w.bits      := out.w.bits
  io.axi4.w.valid     := out.w.valid && (
    w_fire || (out_w_in_head && aw_q.io.enq.ready && Mux1H(out_w_in_id, aw_val))
  )

  when (out.w.fire() && out.w.bits.last) { w_fire := false.B }

  val r_first = RegInit(true.B)
  io.flits.r.valid := out.r.valid
  out.r.ready := io.flits.r.ready
  io.flits.r.bits.head := r_first
  io.flits.r.bits.tail := out.r.bits.last
  io.flits.r.bits.payload := out.r.bits.asUInt
  io.flits.r.bits.egress_id := Mux1H(requestROI.zipWithIndex.map { case (r, i) =>
    r -> (masterToEgressOffset(i) + 0).U
  })
  when (io.flits.r.fire()) { r_first := out.r.bits.last }

  io.flits.b.valid := out.b.valid
  out.b.ready := io.flits.b.ready
  io.flits.b.bits.head := true.B
  io.flits.b.bits.tail := true.B
  io.flits.b.bits.payload := out.b.bits.asUInt
  io.flits.b.bits.egress_id := Mux1H(requestBOI.zipWithIndex.map { case (r, i) =>
    r -> (masterToEgressOffset(i) + 1).U
  })

  out_w_in_head := io.flits.w.bits.head
  out_w_in_id := UIntToOH(io.flits.w.bits.payload.asUInt >> out.w.bits.getWidth)

  out.aw.valid := io.flits.aw.valid
  io.flits.aw.ready := out.aw.ready
  out.aw.bits := io.flits.aw.bits.payload.asTypeOf(out.aw.bits)

  out.w.valid := io.flits.w.valid
  io.flits.w.ready := out.w.ready
  out.w.bits := io.flits.w.bits.payload.asTypeOf(out.w.bits)

  out.ar.valid := io.flits.ar.valid
  io.flits.ar.ready := out.ar.ready
  out.ar.bits := io.flits.ar.bits.payload.asTypeOf(out.ar.bits)
}

class AXI4InterconnectInterface(edgesIn: Seq[AXI4EdgeParameters], edgesOut: Seq[AXI4EdgeParameters])(implicit val p: Parameters) extends Bundle {
  val in = MixedVec(edgesIn.map { e => Flipped(new AXI4Bundle(e.bundle)) })
  val out = MixedVec(edgesOut.map { e => new AXI4Bundle(e.bundle) })
}


// BEGIN: AXI4ProtocolParams
case class AXI4ProtocolParams(
  edgesIn: Seq[AXI4EdgeParameters],
  edgesOut: Seq[AXI4EdgeParameters],
  edgeInNodes: Seq[Int],
  edgeOutNodes: Seq[Int],
  awQueueDepth: Int
) extends ProtocolParams {
  // END: AXI4ProtocolParams
  val wideBundle = AXI4BundleParameters.union(edgesIn.map(_.bundle) ++ edgesOut.map(_.bundle))
  val inputIdRanges = AXI4Xbar.mapInputIds(edgesIn.map(_.master))
  val minPayloadWidth = {
    val b = new AXI4Bundle(wideBundle)
    Seq(b.aw, b.ar, b.w, b.b, b.r).map { t => t.bits match {
      case b: AXI4BundleAW => b.getWidth
      case b: AXI4BundleW => b.getWidth + wideBundle.idBits
      case b: AXI4BundleAR => b.getWidth
      case b: AXI4BundleR => b.getWidth
      case b: AXI4BundleB => b.getWidth
    }}.max
  }
  val ingressNodes = (edgeInNodes.map(u => Seq.fill(3) (u)) ++ edgeOutNodes.map(u => Seq.fill(2) (u))).flatten
  val egressNodes = (edgeInNodes.map(u => Seq.fill(2) (u)) ++ edgeOutNodes.map(u => Seq.fill(3) (u))).flatten
  val nVirtualNetworks = 5
  val vNetBlocking = (blocker: Int, blockee: Int) => blocker < blockee
  val flows = (0 until edgeInNodes.size).map { i => (0 until edgeOutNodes.size).map { o =>
    Seq(FlowParams(i * 3 + 0                    , o * 3 + 0 + edgesIn.size * 2, 4), // AW
        FlowParams(i * 3 + 1                    , o * 3 + 1 + edgesIn.size * 2, 3), // W
        FlowParams(i * 3 + 2                    , o * 3 + 2 + edgesIn.size * 2, 2), // AR
        FlowParams(o * 2 + 0 + edgesIn.size * 3 , i * 2 + 0                   , 1), // R
        FlowParams(o * 2 + 1 + edgesIn.size * 3 , i * 2 + 1                   , 0)  // B
    )
  }}.flatten.flatten

  def genIO()(implicit p: Parameters): Data = new AXI4InterconnectInterface(edgesIn, edgesOut)
  def interface(terminals: NoCTerminalIO,
    ingressOffset: Int, egressOffset: Int, protocol: Data)(implicit p: Parameters) = {
    val ingresses = terminals.ingress
    val egresses = terminals.egress
    protocol match {
      case protocol: AXI4InterconnectInterface => {
        edgesIn.zipWithIndex.map { case (e,i) =>
          val nif_master = Module(new AXI4MasterToNoC(
            e, edgesOut,
            inputIdRanges(i).start, inputIdRanges(i).size,
            wideBundle,
            (s) => s * 3 + edgesIn.size * 2 + egressOffset,
            minPayloadWidth,
            awQueueDepth
          ))
          nif_master.io.axi4 <> protocol.in(i)
          ingresses(i * 3 + 0).flit <> nif_master.io.flits.aw
          ingresses(i * 3 + 1).flit <> nif_master.io.flits.w
          ingresses(i * 3 + 2).flit <> nif_master.io.flits.ar
          nif_master.io.flits.r <> egresses(i * 2 + 0).flit
          nif_master.io.flits.b <> egresses(i * 2 + 1).flit
        }
        edgesOut.zipWithIndex.map { case (e,i) =>
          val nif_slave = Module(new AXI4SlaveToNoC(
            e, edgesIn,
            wideBundle,
            (s) => s * 2 + egressOffset,
            minPayloadWidth,
            awQueueDepth
          ))
          protocol.out(i) <> nif_slave.io.axi4
          ingresses(i * 2 + 0 + edgesIn.size * 3).flit <> nif_slave.io.flits.r
          ingresses(i * 2 + 1 + edgesIn.size * 3).flit <> nif_slave.io.flits.b
          nif_slave.io.flits.aw <> egresses(i * 3 + 0 + edgesIn.size * 2).flit
          nif_slave.io.flits.w  <> egresses(i * 3 + 1 + edgesIn.size * 2).flit
          nif_slave.io.flits.ar <> egresses(i * 3 + 2 + edgesIn.size * 2).flit
        }
      }
    }
  }
}

class AXI4NoCNode(implicit valName: ValName) extends AXI4NexusNode(
  masterFn  = { seq =>
    seq(0).copy(
      echoFields    = BundleField.union(seq.flatMap(_.echoFields)),
      requestFields = BundleField.union(seq.flatMap(_.requestFields)),
      responseKeys  = seq.flatMap(_.responseKeys).distinct,
      masters = (AXI4Xbar.mapInputIds(seq) zip seq) flatMap { case (range, port) =>
        port.masters map { master => master.copy(id = master.id.shift(range.start)) }
      }
    )
  },
  slaveFn = { seq =>
    seq(0).copy(
      responseFields = BundleField.union(seq.flatMap(_.responseFields)),
      requestKeys    = seq.flatMap(_.requestKeys).distinct,
      minLatency = seq.map(_.minLatency).min,
      slaves = seq.flatMap { port =>
        require (port.beatBytes == seq(0).beatBytes,
          s"Xbar data widths don't match: ${port.slaves.map(_.name)} has ${port.beatBytes}B vs ${seq(0).slaves.map(_.name)} has ${seq(0).beatBytes}B")
        port.slaves
      }
    )
  }
)

abstract class AXI4NoCModuleImp(outer: LazyModule) extends LazyModuleImp(outer) {
  val edgesIn: Seq[AXI4EdgeParameters]
  val edgesOut: Seq[AXI4EdgeParameters]
  val nodeMapping: DiplomaticNetworkNodeMapping
  val nocName: String
  val awQueueDepth: Int
  lazy val inNames  = nodeMapping.genUniqueName(edgesIn.map(_.master.masters.map(_.name)))
  lazy val outNames = nodeMapping.genUniqueName(edgesOut.map(_.slave.slaves.map(_.name)))
  lazy val edgeInNodes = nodeMapping.getNodesIn(inNames)
  lazy val edgeOutNodes = nodeMapping.getNodesOut(outNames)
  lazy val protocolParams = AXI4ProtocolParams(
    edgesIn = edgesIn,
    edgesOut = edgesOut,
    edgeInNodes = edgeInNodes.flatten,
    edgeOutNodes = edgeOutNodes.flatten,
    awQueueDepth = awQueueDepth
  )

  def printNodeMappings() {
    println(s"Constellation: AXI4NoC $nocName inwards mapping:")
    for ((n, i) <- inNames zip edgeInNodes) {
      val node = i.map(_.toString).getOrElse("X")
      println(s"  $node <- $n")
    }

    println(s"Constellation: AXI4NoC $nocName outwards mapping:")
    for ((n, i) <- outNames zip edgeOutNodes) {
      val node = i.map(_.toString).getOrElse("X")
      println(s"  $node <- $n")
    }
  }
}

// private AXI4 NoC
// BEGIN: AXI4NoCParams
case class AXI4NoCParams(
  nodeMappings: DiplomaticNetworkNodeMapping,
  nocParams: NoCParams,
  awQueueDepth: Int = 2
)
class AXI4NoC(params: AXI4NoCParams, name: String = "test")(implicit p: Parameters) extends LazyModule {
  // END: AXI4NoCParams
  val node = new AXI4NoCNode
  lazy val module = new AXI4NoCModuleImp(this) {
    val (io_in, edgesIn) = node.in.unzip
    val (io_out, edgesOut) = node.out.unzip
    val nodeMapping = params.nodeMappings
    val nocName = name
    val awQueueDepth = params.awQueueDepth

    printNodeMappings()

    val noc = Module(new ProtocolNoC(ProtocolNoCParams(
      params.nocParams.copy(hasCtrl = false, nocName = name),
      Seq(protocolParams)
    )))

    noc.io.protocol(0) match {
      case protocol: AXI4InterconnectInterface => {
        (protocol.in zip io_in).foreach { case (l,r) => l <> r }
        (io_out zip protocol.out).foreach { case (l,r) => l <> r }
      }
    }
  }
}
