package constellation.rc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.unittest._
import freechips.rocketchip.amba.axi4._

import constellation.noc.{NoC, NoCParams, NoCTerminalIO}
import constellation.channel._
import constellation.topology.{TerminalPlane}

case class AXI4NoCParams(
  nocName: String,
  nodeMappings: ConstellationDiplomaticNetworkNodeMapping,
  privateNoC: NoCParams,
  awQueueDepth: Int = 2
) {
}


class AXI4NoC(params: AXI4NoCParams)(implicit p: Parameters) extends LazyModule {
  val nocName = params.nocName
  val inNodeMapping = params.nodeMappings.inNodeMapping
  val outNodeMapping = params.nodeMappings.outNodeMapping

  // Ordering of protocol channels is
  // - b
  // - r
  // - ar
  // - w
  // - aw

  // Can only keep 1 in-flight per ID because NoC can't guarantee
  // FIFO between bursts with the same ID
  val maxFlightPerId = 1
  val awQueueDepth = params.awQueueDepth
  val nocParams = params.privateNoC

  val node = new AXI4NexusNode(
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
  ){
    override def circuitIdentity = outputs == 1 && inputs == 1
  }


  override lazy val module = new LazyModuleImp(this) {
    val (io_in, edgesIn) = node.in.unzip
    val (io_out, edgesOut) = node.out.unzip


    edgesOut.foreach { edgeOut =>
      // All pairs of slaves must promise that they will never interleave data
      val slaves = edgeOut.slave.slaves
      require (slaves(0).interleavedId.isDefined)
      slaves.foreach { s => require (s.interleavedId == slaves(0).interleavedId) }
    }

    // We need an intermediate size of bundle with the widest possible identifiers
    val wide_bundle = AXI4BundleParameters.union(io_in.map(_.params) ++ io_out.map(_.params))

    // Grab the port ID mapping
    val inputIdRanges = AXI4Xbar.mapInputIds(edgesIn.map(_.master))

    // Find a good mask for address decoding
    val port_addrs = edgesOut.map(_.slave.slaves.map(_.address).flatten)
    val routingMask = AddressDecoder(port_addrs)
    val route_addrs = port_addrs.map(seq => AddressSet.unify(seq.map(_.widen(~routingMask)).distinct))
    val outputPorts = route_addrs.map(seq => (addr: UInt) => seq.map(_.contains(addr)).reduce(_ || _))

    // To route W we need to record where the AWs went
    class AWInBundle extends Bundle {
      val out = UInt(io_out.size.W)
      val id = UInt(wide_bundle.idBits.W)
    }
    val awIn  = Seq.fill(io_in .size) { Module(new Queue(new AWInBundle, awQueueDepth, flow = true)) }

    val requestARIO = io_in.map  { i => VecInit(outputPorts.map   { o => o(i.ar.bits.addr) }) }
    val requestAWIO = io_in.map  { i => VecInit(outputPorts.map   { o => o(i.aw.bits.addr) }) }
    val requestROI  = io_out.map { o => inputIdRanges.map { i => i.contains(o.r.bits.id) } }
    val requestBOI  = io_out.map { o => inputIdRanges.map { i => i.contains(o.b.bits.id) } }

    // W follows the path dictated by the AW Q
    for (i <- 0 until io_in.size) {
      awIn(i).io.enq.bits.out := requestAWIO(i).asUInt
      awIn(i).io.enq.bits.id  := io_in(i).aw.bits.id | inputIdRanges(i).start.U
    }
    val requestWIO = awIn.map { q => if (io_out.size > 1) q.io.deq.bits.out.asBools else Seq(true.B) }


    // Transform input bundles
    val in = Wire(Vec(io_in.size, AXI4Bundle(wide_bundle)))
    for (i <- 0 until in.size) {
      in(i) :<> io_in(i)

      // Handle size = 1 gracefully (Chisel3 empty range is broken)
      def trim(id: UInt, size: Int) = if (size <= 1) 0.U else id(log2Ceil(size)-1, 0)
      // Manipulate the AXI IDs to differentiate masters
      val r = inputIdRanges(i)
      in(i).aw.bits.id := io_in(i).aw.bits.id | r.start.U
      in(i).ar.bits.id := io_in(i).ar.bits.id | r.start.U
      io_in(i).r.bits.id := trim(in(i).r.bits.id, r.size)
      io_in(i).b.bits.id := trim(in(i).b.bits.id, r.size)

      // Block A[RW] if we switch ports, to ensure responses stay ordered (also: beware the dining philosophers)
      val endId = edgesIn(i).master.endId
      val arFIFOMap = WireInit(VecInit(Seq.fill(endId) { true.B }))
      val awFIFOMap = WireInit(VecInit(Seq.fill(endId) { true.B }))
      val arSel = UIntToOH(io_in(i).ar.bits.id, endId)
      val awSel = UIntToOH(io_in(i).aw.bits.id, endId)
      val rSel  = UIntToOH(io_in(i).r .bits.id, endId)
      val bSel  = UIntToOH(io_in(i).b .bits.id, endId)
      val arTag = OHToUInt(requestARIO(i).asUInt, io_out.size)
      val awTag = OHToUInt(requestAWIO(i).asUInt, io_out.size)

      for (master <- edgesIn(i).master.masters) {
        def idTracker(port: UInt, req_fire: Bool, resp_fire: Bool) = {
          if (master.maxFlight == Some(0)) {
            true.B
          } else {
            val legalFlight = master.maxFlight.getOrElse(maxFlightPerId+1)
            val flight = legalFlight min maxFlightPerId
            val canOverflow = legalFlight > flight
            val count = RegInit(0.U(log2Ceil(flight+1).W))
            val last = Reg(UInt(log2Ceil(io_out.size).W))
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
            arSel(id) && io_in(i).ar.fire(),
            rSel(id) && io_in(i).r.fire() && io_in(i).r.bits.last)
          awFIFOMap(id) := idTracker(
            awTag,
            awSel(id) && io_in(i).aw.fire(),
            bSel(id) && io_in(i).b.fire())
        }
      }

      val allowAR = arFIFOMap(io_in(i).ar.bits.id)
      in(i).ar.valid := io_in(i).ar.valid && allowAR
      io_in(i).ar.ready := in(i).ar.ready && allowAR

      // Block AW if we cannot record the W destination
      val allowAW = awFIFOMap(io_in(i).aw.bits.id)
      in(i).aw.valid := io_in(i).aw.valid && awIn(i).io.enq.ready && allowAW
      io_in(i).aw.ready := in(i).aw.ready && awIn(i).io.enq.ready && allowAW
      awIn(i).io.enq.valid := io_in(i).aw.valid && in(i).aw.ready && allowAW

      // Block W if we do not have an AW destination
      in(i).w.valid := io_in(i).w.valid && awIn(i).io.deq.valid
      io_in(i).w.ready := in(i).w.ready && awIn(i).io.deq.valid
      awIn(i).io.deq.ready := io_in(i).w.valid && io_in(i).w.bits.last && in(i).w.ready
    }

    // Transform output bundles
    val out = Wire(Vec(io_out.size, AXI4Bundle(wide_bundle)))
    val out_w_in_head = Seq.fill(out.size) { Wire(Bool()) }
    val out_w_in_id   = Seq.fill(out.size) { Wire(UInt((1 << wide_bundle.idBits).W)) }
    for (i <- 0 until out.size) {
      io_out(i) :<> out(i)

      // Store the incoming AW messages in a buffer, 1 entry per source ID
      // Order the AW/W based on the arriving order of W bursts
      val aw_val = RegInit(VecInit(0.U((1 << wide_bundle.idBits).W).asBools))
      val aw_buf = Reg(Vec(1 << wide_bundle.idBits, new AXI4BundleAW(wide_bundle)))
      val w_fire = RegInit(false.B)
      val aw_q   = Module(new Queue(new AXI4BundleAW(wide_bundle), awQueueDepth, pipe=true))

      io_out(i).aw <> aw_q.io.deq

      out(i).aw.ready       := !aw_val(out(i).aw.bits.id)
      when (out(i).aw.fire()) {
        aw_val(out(i).aw.bits.id) := true.B
        aw_buf(out(i).aw.bits.id) := out(i).aw.bits
      }

      aw_q.io.enq.valid := out(i).w.valid && Mux1H(out_w_in_id(i), aw_val) && out_w_in_head(i)
      aw_q.io.enq.bits  :=                   Mux1H(out_w_in_id(i), aw_buf)
      when (aw_q.io.enq.fire()) {
        aw_val(OHToUInt(out_w_in_id(i))) := false.B
        w_fire := true.B
      }

      out(i).w.ready        := io_out(i).w.ready && (
        w_fire || (out_w_in_head(i) && aw_q.io.enq.ready && Mux1H(out_w_in_id(i), aw_val))
      )
      io_out(i).w.bits      := out(i).w.bits
      io_out(i).w.valid     := out(i).w.valid && (
        w_fire || (out_w_in_head(i) && aw_q.io.enq.ready && Mux1H(out_w_in_id(i), aw_val))
      )

      when (out(i).w.fire() && out(i).w.bits.last) { w_fire := false.B }
    }


    def isAWIn (i: Int) = i <  in.size * 3 && i % 3 == 0
    def isAWOut(o: Int) = o >= in.size * 2 && (o - in.size*2) % 3 == 0

    def isWIn  (i: Int) = i <  in.size * 3 && i % 3 == 1
    def isWOut (o: Int) = o >= in.size * 2 && (o - in.size*2) % 3 == 1

    def isARIn (i: Int) = i <  in.size * 3 && i % 3 == 2
    def isAROut(o: Int) = o >= in.size * 2 && (o - in.size*2) % 3 == 2

    def isRIn  (i: Int) = i >= in.size * 3 && (i - in.size*3) % 2 == 0
    def isROut (o: Int) = o <  in.size * 2 && o % 2 == 0

    def isBIn  (i: Int) = i >= in.size * 3 && (i - in.size*3) % 2 == 1
    def isBOut (o: Int) = o <  in.size * 2 && o % 2 == 1

    def ingressVNets(i: Int) = {
      if (isAWIn(i)) {
        4
      } else if (isWIn(i)) {
        3
      } else if (isARIn(i)) {
        2
      } else if (isRIn(i)) {
        1
      } else {
        require(isBIn(i))
        0
      }
    }
    def egressVNets(i: Int) = {
      if (isAWOut(i)) {
        4
      } else if (isWOut(i)) {
        3
      } else if (isAROut(i)) {
        2
      } else if (isROut(i)) {
        1
      } else {
        require(isBOut(i))
        0
      }
    }

    val wb = new AXI4Bundle(wide_bundle)
    val payloadWidth = Seq(
      wb.aw.bits.getWidth,
      wb.w .bits.getWidth + wide_bundle.idBits, // We need to send idbits with W
      wb.ar.bits.getWidth,
      wb.r .bits.getWidth,
      wb.b .bits.getWidth
    ).max

    val nocParams = params.privateNoC

    def getIndex(nodeMapping: Seq[String], l: String) = {
      val matches = nodeMapping.map(k => l.contains(k))
      require(matches.filter(i => i).size == 1, s"Unable to find valid mapping for $l")
      matches.indexWhere(i => i)
    }

    def genUniqueName(all: Seq[Seq[String]]) = {
      all.zipWithIndex.map { case (strs, i) =>
        val matches = all.take(i).map(_.mkString).count(_ == strs.mkString)
        strs.map(s => s"${s}[${matches}]").mkString(",")
      }
    }

    require(in.size == inNodeMapping.size,
      s"AXI4 Inwards count at $nocName must match mapping size ${in.size} != ${inNodeMapping.size}")
    require(out.size == outNodeMapping.size,
      s"AXI4 Outwards count at $nocName must match mapping size ${out.size} != ${outNodeMapping.size}")

    val inNames  = genUniqueName(edgesIn.map(_.master.masters.map(_.name)))
    val outNames = genUniqueName(edgesOut.map(_.slave.slaves.map(_.name)))

    val flowParams = (0 until in.size).map { i => (0 until out.size).map { o =>
      val inN = inNames(i)
      val outN = outNames(o)

      val iId = getIndex(inNodeMapping.keys.toSeq, inN)
      val oId = getIndex(outNodeMapping.keys.toSeq, outN)

      val aw = FlowParams(iId * 3    , in.size * 2 + oId * 3    , 4)
      val w  = FlowParams(iId * 3 + 1, in.size * 2 + oId * 3 + 1, 3)
      val ar = FlowParams(iId * 3 + 2, in.size * 2 + oId * 3 + 2, 2)

      val r  = FlowParams(in.size * 3 + oId * 2    , iId * 2    , 1)
      val b  = FlowParams(in.size * 3 + oId * 2 + 1, iId * 2 + 1, 0)

      Seq(aw, w, ar, r, b)
    }}.flatten.flatten

    val ingressParams = (inNodeMapping.values.map(i => Seq(i, i, i)) ++ outNodeMapping.values.map(i => Seq(i, i)))
      .toSeq
      .flatten.zipWithIndex.map { case (i,iId) => UserIngressParams(
        destId = i,
        payloadBits = payloadWidth
      )}
    val egressParams = (inNodeMapping.values.map(i => Seq(i, i)) ++ outNodeMapping.values.map(i => Seq(i, i, i)))
      .toSeq
      .flatten.zipWithIndex.map { case (e,eId) => UserEgressParams(
        srcId = e,
        payloadBits = payloadWidth
      )}

    val noc = Module(LazyModule(new NoC(nocParams.copy(
      routerParams = (i: Int) => nocParams.routerParams(i).copy(payloadBits=payloadWidth),
      ingresses = ingressParams,
      egresses = egressParams,
      flows = flowParams,
        nocName = nocName,
      hasCtrl = false
    ))).module)

    noc.io.router_clocks.foreach(_.clock := clock)
    noc.io.router_clocks.foreach(_.reset := reset)

    def toFlit(
      i: IrrevocableIO[Data], o: IrrevocableIO[IngressFlit],
      last: Bool, egress: UInt, extra: UInt) = {
      val first = RegInit(true.B)

      o.valid := i.valid
      i.ready := o.ready

      o.bits.head := first
      o.bits.tail := last
      o.bits.egress_id := egress
      o.bits.payload := i.bits.asUInt | (extra << i.bits.getWidth)
      when (i.fire()) { first := last }
    }

    def fromFlit(i: IrrevocableIO[EgressFlit], o: IrrevocableIO[Data]) = {
      i.ready := o.ready
      o.valid := i.valid
      o.bits := i.bits.payload.asTypeOf(o.bits)
    }

    val ingresses: Seq[IngressChannel] = noc.io.ingress
    val egresses: Seq[EgressChannel] = noc.io.egress

    for (i <- 0 until in.size) {
      val idx = getIndex(inNodeMapping.keys.toSeq, inNames(i))
      val inAW = ingresses (idx*3)
      val inW  = ingresses (idx*3+1)
      val inAR = ingresses (idx*3+2)
      val outR = egresses  (idx*2)
      val outB = egresses  (idx*2+1)

      println(s"Constellation AXI4NoC $nocName:  in $i @ ${inNodeMapping.values.toSeq(i)}: ${inNames(i)}")
      println(s"Constellation AXI4NoC $nocName:   ingress (${idx*3} ${idx*3+1} ${idx*3+2}) egress (${idx*2} ${idx*2+1})")

      val inAWEgress = Mux1H(requestAWIO(i).zipWithIndex.map { case (r,i) =>
        r -> (in.size*2 + getIndex(outNodeMapping.keys.toSeq, outNames(i))*3 + 0).U
      })
      val inWEgress  = Mux1H(requestWIO (i).zipWithIndex.map { case (r,i) =>
        r -> (in.size*2 + getIndex(outNodeMapping.keys.toSeq, outNames(i))*3 + 1).U
      })
      val inAREgress = Mux1H(requestARIO(i).zipWithIndex.map { case (r,i) =>
        r -> (in.size*2 + getIndex(outNodeMapping.keys.toSeq, outNames(i))*3 + 2).U
      })

      toFlit(in(i).aw,  inAW.flit, true.B           , inAWEgress, 0.U)
      toFlit(in(i).w ,  inW.flit , in(i).w.bits.last, inWEgress , awIn(i).io.deq.bits.id)
      toFlit(in(i).ar,  inAR.flit, true.B           , inAREgress, 0.U)

      fromFlit(outR.flit, in(i).r)
      fromFlit(outB.flit, in(i).b)

    }

    for (i <- 0 until out.size) {
      val idx = getIndex(outNodeMapping.keys.toSeq, outNames(i))
      val outAW = egresses (in.size*2+idx*3)
      val outW  = egresses (in.size*2+idx*3+1)
      val outAR = egresses (in.size*2+idx*3+2)
      val inR   = ingresses(in.size*3+idx*2)
      val inB   = ingresses(in.size*3+idx*2+1)

      println(s"Constellation AXI4NoC $nocName: out $i @ ${outNodeMapping.values.toSeq(i)}: ${outNames(i)}")
      println(s"Constellation AXI4NoC $nocName:   ingress (${in.size*3+idx*2} ${in.size*3+idx*2+1}) egress (${in.size*2+idx*3} ${in.size*2+idx*3+1} ${in.size*2+idx*3+2})")

      out_w_in_head(i) := outW.flit.bits.head
      out_w_in_id  (i) := UIntToOH(outW.flit.bits.payload.asUInt >> out(i).w.bits.getWidth)

      val inREgress  = Mux1H(requestROI (i).zipWithIndex.map { case (r,i) =>
        r -> (getIndex(inNodeMapping.keys.toSeq, inNames(i))*2 + 0).U
      })
      val inBEgress  = Mux1H(requestBOI (i).zipWithIndex.map { case (r,i) =>
        r -> (getIndex(inNodeMapping.keys.toSeq, inNames(i))*2 + 1).U
      })


      fromFlit(outAW.flit, out(i).aw)
      fromFlit(outW.flit , out(i).w)
      fromFlit(outAR.flit, out(i).ar)

      toFlit(out(i).r, inR.flit, out(i).r.bits.last, inREgress, 0.U)
      toFlit(out(i).b, inB.flit, true.B            , inBEgress, 0.U)
    }

  }

}
