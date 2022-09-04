package constellation.rc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util._

import constellation.noc.{NoC, NoCParams, NoCTerminalIO}
import constellation.channel._
import constellation.topology.{TerminalPlane}

import scala.collection.immutable.ListMap

case class TLNoCParams(
  nocName: String,
  nodeMappings: ConstellationDiplomaticNetworkNodeMapping,
  // if set, generates a private noc using the config,
  // else use globalNoC params to connect to global interconnect
  privateNoC: Option[NoCParams],
  explicitPayloadWidth: Option[Int] = None, // if unset, chooses minimum with based on TL bundle width
  globalTerminalChannels: Option[() => BundleBridgeSink[NoCTerminalIO]] = None,
)

case class ConstellationDiplomaticNetworkNodeMapping(
  inNodeMapping: ListMap[String, Int] = ListMap[String, Int](),
  outNodeMapping: ListMap[String, Int] = ListMap[String, Int]()
)

class TLNoC(params: TLNoCParams)(implicit p: Parameters) extends TLXbar {
  val nocName = params.nocName
  val inNodeMapping = params.nodeMappings.inNodeMapping
  val outNodeMapping = params.nodeMappings.outNodeMapping
  val privateNoC = params.privateNoC
  val explicitPayloadWidth = params.explicitPayloadWidth
  val globalSink: Option[BundleBridgeSink[NoCTerminalIO]] = if (privateNoC.isEmpty) {
    Some(params.globalTerminalChannels.get())
  } else {
    None
  }

  override lazy val module = new LazyModuleImp(this) {
    val (io_in, edgesIn) = node.in.unzip
    val (io_out, edgesOut) = node.out.unzip
    val nIn = edgesIn.size
    val nOut = edgesOut.size

    // Not every master need connect to every slave on every channel; determine which connections are necessary
    val reachableIO = edgesIn.map { cp => edgesOut.map { mp =>
      cp.client.clients.exists { c => mp.manager.managers.exists { m =>
        c.visibility.exists { ca => m.address.exists { ma =>
          ca.overlaps(ma)}}}}
      }.toVector}.toVector
    val probeIO = (edgesIn zip reachableIO).map { case (cp, reachableO) =>
      (edgesOut zip reachableO).map { case (mp, reachable) =>
        reachable && cp.client.anySupportProbe && mp.manager.managers.exists(_.regionType >= RegionType.TRACKED)
      }.toVector}.toVector
    val releaseIO = (edgesIn zip reachableIO).map { case (cp, reachableO) =>
      (edgesOut zip reachableO).map { case (mp, reachable) =>
        reachable && cp.client.anySupportProbe && mp.manager.anySupportAcquireB
      }.toVector}.toVector

    val connectAIO = reachableIO
    val connectBIO = probeIO
    val connectCIO = releaseIO
    val connectDIO = reachableIO
    val connectEIO = releaseIO

    def transpose[T](x: Seq[Seq[T]]) = if (x.isEmpty) Nil else Vector.tabulate(x(0).size) { i => Vector.tabulate(x.size) { j => x(j)(i) } }
    val connectAOI = transpose(connectAIO)
    val connectBOI = transpose(connectBIO)
    val connectCOI = transpose(connectCIO)
    val connectDOI = transpose(connectDIO)
    val connectEOI = transpose(connectEIO)

    // Grab the port ID mapping
    val inputIdRanges = TLXbar.mapInputIds(edgesIn.map(_.client))
    val outputIdRanges = TLXbar.mapOutputIds(edgesOut.map(_.manager))

    // We need an intermediate size of bundle with the widest possible identifiers
    val wide_bundle = TLBundleParameters.union(io_in.map(_.params) ++ io_out.map(_.params))

    // Handle size = 1 gracefully (Chisel3 empty range is broken)
    def trim(id: UInt, size: Int): UInt = if (size <= 1) 0.U else id(log2Ceil(size)-1, 0)

    // Transform input bundle sources (sinks use global namespace on both sides)
    val in = Wire(Vec(io_in.size, TLBundle(wide_bundle)))
    for (i <- 0 until in.size) {
      val r = inputIdRanges(i)

      if (connectAIO(i).exists(x=>x)) {
        val a_q = Module(new Queue(in(i).a.bits.cloneType, 1, pipe=true, flow=true))
        a_q.io.enq :<> io_in(i).a
        a_q.io.enq.bits.source := io_in(i).a.bits.source | r.start.U
        in(i).a <> a_q.io.deq
      } else {
        in(i).a.valid      := false.B
        in(i).a.bits       := DontCare
        io_in(i).a.ready      := true.B
        io_in(i).a.bits       := DontCare
      }

      if (connectBIO(i).exists(x=>x)) {
        io_in(i).b :<> in(i).b
        io_in(i).b.bits.source := trim(in(i).b.bits.source, r.size)
      } else {
        in(i).b.ready := true.B
        in(i).b.bits  := DontCare
        io_in(i).b.valid := false.B
        io_in(i).b.bits  := DontCare
      }

      if (connectCIO(i).exists(x=>x)) {
        val c_q = Module(new Queue(in(i).c.bits.cloneType, 1, pipe=true, flow=true))
        c_q.io.enq :<> io_in(i).c
        c_q.io.enq.bits.source := io_in(i).c.bits.source | r.start.U
        in(i).c <> c_q.io.deq
      } else {
        in(i).c.valid := false.B
        in(i).c.bits  := DontCare
        io_in(i).c.ready := true.B
        io_in(i).c.bits  := DontCare
      }

      if (connectDIO(i).exists(x=>x)) {
        io_in(i).d :<> in(i).d
        io_in(i).d.bits.source := trim(in(i).d.bits.source, r.size)
      } else {
        in(i).d.ready := true.B
        in(i).d.bits  := DontCare
        io_in(i).d.valid := false.B
        io_in(i).d.bits  := DontCare
      }

      if (connectEIO(i).exists(x=>x)) {
        val e_q = Module(new Queue(in(i).e.bits.cloneType, 1, pipe=true, flow=true))
        e_q.io.enq :<> io_in(i).e
        in(i).e <> e_q.io.deq
      } else {
        in(i).e.valid := false.B
        in(i).e.bits  := DontCare
        io_in(i).e.ready := true.B
        io_in(i).e.bits  := DontCare
      }
    }

    // Transform output bundle sinks (sources use global namespace on both sides)
    val out = Wire(Vec(io_out.size, TLBundle(wide_bundle)))
    for (o <- 0 until out.size) {
      val r = outputIdRanges(o)

      if (connectAOI(o).exists(x=>x)) {
        io_out(o).a :<> out(o).a
      } else {
        out(o).a.ready      := true.B
        out(o).a.bits       := DontCare
        io_out(o).a.valid      := false.B
        io_out(o).a.bits       := DontCare
      }

      if (connectBOI(o).exists(x=>x)) {
        val b_q = Module(new Queue(out(o).b.bits.cloneType, 1, pipe=true, flow=true))
        b_q.io.enq :<> io_out(o).b
        out(o).b <> b_q.io.deq
      } else {
        out(o).b.valid := false.B
        out(o).b.bits  := DontCare
        io_out(o).b.ready := true.B
        io_out(o).b.bits  := DontCare
      }

      if (connectCOI(o).exists(x=>x)) {
        io_out(o).c :<> out(o).c
      } else {
        out(o).c.ready := true.B
        out(o).c.bits  := DontCare
        io_out(o).c.valid := false.B
        io_out(o).c.bits  := DontCare
      }

      if (connectDOI(o).exists(x=>x)) {
        val d_q = Module(new Queue(out(o).d.bits.cloneType, 1, pipe=true, flow=true))
        d_q.io.enq :<> io_out(o).d
        d_q.io.enq.bits.sink := io_out(o).d.bits.sink | r.start.U
        out(o).d <> d_q.io.deq
      } else {
        out(o).d.valid := false.B
        out(o).d.bits  := DontCare
        io_out(o).d.ready := true.B
        io_out(o).d.bits  := DontCare
      }

      if (connectEOI(o).exists(x=>x)) {
        io_out(o).e :<> out(o).e
        io_out(o).e.bits.sink := trim(out(o).e.bits.sink, r.size)
      } else {
        out(o).e.ready := true.B
        out(o).e.bits  := DontCare
        io_out(o).e.valid := false.B
        io_out(o).e.bits  := DontCare
      }
    }
        // Filter a list to only those elements selected
    def filter[T](data: Seq[T], mask: Seq[Boolean]) = (data zip mask).filter(_._2).map(_._1)

    // Based on input=>output connectivity, create per-input minimal address decode circuits
    val requiredAC = (connectAIO ++ connectCIO).distinct
    val outputPortFns: Map[Vector[Boolean], Seq[UInt => Bool]] = requiredAC.map { connectO =>
      val port_addrs = edgesOut.map(_.manager.managers.flatMap(_.address))
      val routingMask = AddressDecoder(filter(port_addrs, connectO))
      val route_addrs = port_addrs.map(seq => AddressSet.unify(seq.map(_.widen(~routingMask)).distinct))
      (connectO, route_addrs.map(seq => (addr: UInt) => seq.map(_.contains(addr)).reduce(_ || _)))
    }.toMap


    val addressA = (in zip edgesIn) map { case (i, e) => e.address(i.a.bits) }
    val addressC = (in zip edgesIn) map { case (i, e) => e.address(i.c.bits) }

    def unique(x: Vector[Boolean]): Bool = (x.filter(x=>x).size <= 1).B
    val requestAIO = (connectAIO zip addressA) map { case (c, i) =>
      outputPortFns(c).zipWithIndex.map { case (o, j) => c(j).B && (unique(c) || o(i)) } }
    val requestCIO = (connectCIO zip addressC) map { case (c, i) =>
      outputPortFns(c).zipWithIndex.map { case (o, j) => c(j).B && (unique(c) || o(i)) } }
    val requestBOI = out.map { o => inputIdRanges.map  { i => i.contains(o.b.bits.source) } }
    val requestDOI = out.map { o => inputIdRanges.map  { i => i.contains(o.d.bits.source) } }
    val requestEIO = in.map  { i => outputIdRanges.map { o => o.contains(i.e.bits.sink) } }

    val firstAI = (in  zip edgesIn)  map { case (i, e) => e.first(i.a) }
    val firstBO = (out zip edgesOut) map { case (o, e) => e.first(o.b) }
    val firstCI = (in  zip edgesIn)  map { case (i, e) => e.first(i.c) }
    val firstDO = (out zip edgesOut) map { case (o, e) => e.first(o.d) }
    val firstEI = (in  zip edgesIn)  map { case (i, e) => e.first(i.e) }

    val lastAI = (in  zip edgesIn)  map { case (i, e) => e.last(i.a) }
    val lastBO = (out zip edgesOut) map { case (o, e) => e.last(o.b) }
    val lastCI = (in  zip edgesIn)  map { case (i, e) => e.last(i.c) }
    val lastDO = (out zip edgesOut) map { case (o, e) => e.last(o.d) }
    val lastEI = (in  zip edgesIn)  map { case (i, e) => e.last(i.e) }

    val requestAIIds = VecInit(requestAIO.map(OHToUInt(_)))
    val requestCIIds = VecInit(requestCIO.map(OHToUInt(_)))
    val requestBOIds = VecInit(requestBOI.map(OHToUInt(_)))
    val requestDOIds = VecInit(requestDOI.map(OHToUInt(_)))
    val requestEIIds = VecInit(requestEIO.map(OHToUInt(_)))

    def isAIn (i: Int) = i <  in.size * 3 && i % 3 == 0
    def isAOut(o: Int) = o >= in.size * 2 && (o - in.size*2) % 3 == 0

    def isBIn (i: Int) = i >= in.size * 3 && (i - in.size*3) % 2 == 0
    def isBOut(o: Int) = o <  in.size * 2 && o % 2 == 0

    def isCIn (i: Int) = i <  in.size * 3 && i % 3 == 1
    def isCOut(o: Int) = o >= in.size * 2 && (o - in.size*2) % 3 == 1

    def isDIn (i: Int) = i >= in.size * 3 && (i - in.size*3) % 2 == 1
    def isDOut(o: Int) = o <  in.size * 2 && o % 2 == 1

    def isEIn (i: Int) = i <  in.size * 3 && i % 3 == 2
    def isEOut(o: Int) = o >= in.size * 2 && (o - in.size*2) % 3 == 2

    def ingressVNets(i: Int) = {
      if (isAIn(i)) {
        4
      } else if (isBIn(i)) {
        3
      } else if (isCIn(i)) {
        2
      } else if (isDIn(i)) {
        1
      } else {
        require(isEIn(i))
        0
      }
    }

    def egressVNets(i: Int) = {
      if (isAOut(i)) {
        4
      } else if (isBOut(i)) {
        3
      } else if (isCOut(i)) {
        2
      } else if (isDOut(i)) {
        1
      } else {
        require(isEOut(i))
        0
      }
    }

    def getBodyFields(b: Data) = b match {
      case b: TLBundleA => Seq(b.mask, b.data, b.corrupt)
      case b: TLBundleB => Seq(b.mask, b.data, b.corrupt)
      case b: TLBundleC => Seq(        b.data, b.corrupt)
      case b: TLBundleD => Seq(        b.data, b.corrupt)
      case b: TLBundleE => Seq()
      case _ => throw new Exception("wrong type")
    }
    def getConstFields(b: Data) = b match {
      case b: TLBundleA => Seq(b.opcode, b.param, b.size, b.source, b.address, b.user, b.echo                  )
      case b: TLBundleB => Seq(b.opcode, b.param, b.size, b.source, b.address                                  )
      case b: TLBundleC => Seq(b.opcode, b.param, b.size, b.source, b.address, b.user, b.echo                  )
      case b: TLBundleD => Seq(b.opcode, b.param, b.size, b.source,            b.user, b.echo, b.sink, b.denied)
      case b: TLBundleE => Seq(                                                                b.sink          )
      case _ => throw new Exception("wrong type")
    }

    def bodyWidth(b: Data) = getBodyFields(b).map(_.getWidth).sum
    def constWidth(b: Data) = getConstFields(b).map(_.getWidth).sum

    def split(b: Data) = (
      Cat(getBodyFields(b) .filter(_.getWidth > 0).map(_.asUInt)),
      Cat(getConstFields(b).filter(_.getWidth > 0).map(_.asUInt))
    )


    def combine(b: Data, const: UInt, body: UInt, body_valid: Bool) {
      def assign(i: UInt, sigs: Seq[Data]) = {
        var t = i
        for (s <- sigs.reverse) {
          s := t.asTypeOf(s.cloneType)
          t = t >> s.getWidth
        }
      }
      assign(const, getConstFields(b))
      assign(body, getBodyFields(b))
      when (!body_valid) {
        b match {
          case b: TLBundleA => b.mask := ~(0.U(b.mask.getWidth.W))
          case b: TLBundleB => b.mask := ~(0.U(b.mask.getWidth.W))
          case _ =>
        }
      }
    }

    val wb = new TLBundle(wide_bundle)
    val payloadWidth = Seq(wb.a, wb.b, wb.c, wb.d, wb.e)
      .map(_.bits)
      .map(b => bodyWidth(b) max constWidth(b))
      .max
    val debugPrintLatencies = false
    val actualPayloadWidth = payloadWidth + (if (debugPrintLatencies) 64 else 0)

    def splitToFlit(
      i: DecoupledIO[Data], o: IrrevocableIO[IngressFlit],
      first: Bool, last: Bool, egress: UInt, has_body: Bool, debug: UInt
    ) = {
      val is_body = RegInit(false.B)
      o.valid := i.valid
      i.ready := o.ready && (is_body || !has_body)

      val (body, const) = split(i.bits)

      o.bits.head := first && !is_body
      o.bits.tail := last && (is_body || !has_body)
      o.bits.egress_id := egress
      o.bits.payload := Mux(is_body, body, const) | debug << payloadWidth

      when (o.fire() && o.bits.head) { is_body := true.B }
      when (o.fire() && o.bits.tail) { is_body := false.B }
    }
    def combineFromFlit(i: IrrevocableIO[EgressFlit], o: DecoupledIO[Data]) = {
      val is_const = RegInit(true.B)
      val const = Reg(UInt(constWidth(o.bits).W))

      i.ready := (is_const && !i.bits.tail) || o.ready
      o.valid := (!is_const || i.bits.tail) && i.valid
      combine(o.bits, Mux(i.bits.head, i.bits.payload, const), i.bits.payload, !i.bits.head)
      when (i.fire() && i.bits.head) { is_const := false.B; const := i.bits.payload }
      when (i.fire() && i.bits.tail) { is_const := true.B }
    }

    val nIngresses = in.size * 3 + out.size * 2
    val nEgresses = out.size * 3 + in.size * 2

    def getIndex(nodeMapping: Seq[String], l: String) = {
      val matches = nodeMapping.map(k => l.contains(k))
      require(matches.filter(i => i).size == 1, s"$nocName unable to find valid mapping for $l\n$nodeMapping")
      matches.indexWhere(i => i)
    }

    def genUniqueName(all: Seq[Seq[String]]) = {
      all.zipWithIndex.map { case (strs, i) =>
        val matches = all.take(i).map(_.mkString).count(_ == strs.mkString)
        strs.map(s => s"${s}[${matches}]").mkString(",")
      }
    }

    val inNames  = genUniqueName(edgesIn.map(_.master.masters.map(_.name)))
    val outNames = genUniqueName(edgesOut.map(_.slave.slaves.map(_.name)))

    require(in.size == inNodeMapping.size,
      s"TL Inwards count at $nocName must match mapping size ${in.size} != ${inNodeMapping.size}. In: ${inNames}")
    require(out.size == outNodeMapping.size,
      s"TL Outwards count at $nocName must match mapping size ${out.size} != ${outNodeMapping.size}. Out: ${outNames}")

    val noc = privateNoC.map { nocParams =>
      // If desired, create a private noc
      val flowParams = (0 until in.size).map { i => (0 until out.size).map { o =>
        val inN = inNames(i)
        val outN = outNames(o)

        val iId = getIndex(inNodeMapping.keys.toSeq, inN)
        val oId = getIndex(outNodeMapping.keys.toSeq, outN)
        val outFifo = edgesOut(o).slave.slaves.map(_.fifoId.isDefined).reduce(_||_)

        val a = connectAIO(iId)(oId).option(FlowParams(iId * 3    , in.size * 2 + oId * 3    , 4))
        val c = connectCIO(iId)(oId).option(FlowParams(iId * 3 + 1, in.size * 2 + oId * 3 + 1, 2))
        val e = connectEIO(iId)(oId).option(FlowParams(iId * 3 + 2, in.size * 2 + oId * 3 + 2, 0))

        val b = connectBOI(oId)(iId).option(FlowParams(in.size * 3 + oId * 2    , iId * 2    , 3))
        val d = connectDOI(oId)(iId).option(FlowParams(in.size * 3 + oId * 2 + 1, iId * 2 + 1, 1))
        (a ++ b ++ c ++ d ++ e)
      }}.flatten.flatten

      val ingressParams = (inNodeMapping.values.map(i => Seq(i, i, i)) ++ outNodeMapping.values.map(i => Seq(i, i)))
        .toSeq
        .flatten.zipWithIndex.map { case (i,iId) => UserIngressParams(
          destId = i,
          payloadBits = explicitPayloadWidth.map(e => e * ((actualPayloadWidth + e - 1) / e)).getOrElse(actualPayloadWidth)
        )}
      val egressParams = (inNodeMapping.values.map(i => Seq(i, i)) ++ outNodeMapping.values.map(i => Seq(i, i, i)))
        .toSeq
        .flatten.zipWithIndex.map { case (e,eId) => UserEgressParams(
          srcId = e,
          payloadBits = explicitPayloadWidth.map(e => e * ((actualPayloadWidth + e - 1) / e)).getOrElse(actualPayloadWidth)
        )}

      Module(LazyModule(new NoC(nocParams.copy(
        routerParams = (i: Int) => nocParams.routerParams(i).copy(payloadBits = explicitPayloadWidth.getOrElse(actualPayloadWidth)),
        ingresses = ingressParams,
        egresses = egressParams,
        flows = flowParams,
        nocName = nocName,
        hasCtrl = false
      ))).module)
    }
    // If we have privateNoC
    noc.map { n =>
      n.io.router_clocks.foreach(_.clock := clock)
      n.io.router_clocks.foreach(_.reset := reset)
    }

    val ingresses: Seq[IngressChannel] = noc.map(_.io.ingress).getOrElse(globalSink.get.in(0)._1.ingress)
    val egresses: Seq[EgressChannel] = noc.map(_.io.egress).getOrElse(globalSink.get.in(0)._1.egress)

    for (t <- ingresses) { require(t.payloadBits >= actualPayloadWidth, s"${t.payloadBits} <= $actualPayloadWidth") }
    for (t <-  egresses) { require(t.payloadBits >= actualPayloadWidth, s"${t.payloadBits} <= $actualPayloadWidth") }

    val tsc = RegInit(0.U(32.W))
    tsc := tsc + 1.U
    def debugPrint(channel: String, term: EgressChannel) = {
      when (term.flit.fire() && term.flit.bits.tail) {
        val payload = term.flit.bits.payload
        printf(s"TLNoC, $channel, %d, %d, %d\n",
          tsc - payload(payloadWidth+32-1,payloadWidth),
          payload(payloadWidth+48-1,payloadWidth+32),
          payload(actualPayloadWidth-1, payloadWidth + 48))
      }
    }

    for (i <- 0 until in.size) {
      val idx = getIndex(inNodeMapping.keys.toSeq, inNames(i))
      val inA  = ingresses (idx*3)
      val outB = egresses  (idx*2)
      val inC  = ingresses (idx*3+1)
      val outD = egresses  (idx*2+1)
      val inE  = ingresses (idx*3+2)

      println(s"Constellation TLNoC $nocName: in  $i @ ${inNodeMapping.values.toSeq(i)}: ${inNames(i)}")
      println(s"Constellation TLNoC $nocName:   ingress (${idx*3} ${idx*3+1} ${idx*3+2}) egress (${idx*2} ${idx*2+1})")

      val inAEgress = Mux1H(requestAIO(i).zipWithIndex.map { case (r,i) =>
        r -> (in.size*2 + getIndex(outNodeMapping.keys.toSeq, outNames(i))*3 + 0).U
      })
      val inCEgress = Mux1H(requestCIO(i).zipWithIndex.map { case (r,i) =>
        r -> (in.size*2 + getIndex(outNodeMapping.keys.toSeq, outNames(i))*3 + 1).U
      })
      val inEEgress = Mux1H(requestEIO(i).zipWithIndex.map { case (r,i) =>
        r -> (in.size*2 + getIndex(outNodeMapping.keys.toSeq, outNames(i))*3 + 2).U
      })

      splitToFlit(
        in(i).a, inA.flit, firstAI(i), lastAI(i), inAEgress,
        edgesIn(i).hasData(in(i).a.bits) || (~in(i).a.bits.mask =/= 0.U),
        Cat(requestAIIds(i), i.U(16.W), tsc)
      )
      splitToFlit(
        in(i).c, inC.flit, firstCI(i), lastCI(i), inCEgress,
        edgesIn(i).hasData(in(i).c.bits),
        Cat(requestCIIds(i), i.U(16.W), tsc)
      )
      splitToFlit(
        in(i).e, inE.flit, firstEI(i), lastEI(i), inEEgress,
        edgesIn(i).hasData(in(i).e.bits),
        Cat(requestEIIds(i), i.U(16.W), tsc)
      )

      combineFromFlit (outB.flit, in(i).b)
      combineFromFlit (outD.flit, in(i).d)

      if (debugPrintLatencies) {
        debugPrint("B", outB)
        debugPrint("D", outD)
      }
    }

    for (i <- 0 until out.size) {
      val idx = getIndex(outNodeMapping.keys.toSeq, outNames(i))
      val outA  = egresses  (in.size*2+idx*3)
      val inB   = ingresses (in.size*3+idx*2)
      val outC  = egresses  (in.size*2+idx*3+1)
      val inD   = ingresses (in.size*3+idx*2+1)
      val outE  = egresses  (in.size*2+idx*3+2)

      println(s"Constellation TLNoC $nocName: out $i @ ${outNodeMapping.values.toSeq(i)}: ${outNames(i)}")
      println(s"Constellation TLNoC $nocName:   ingress (${in.size*3+idx*2} ${in.size*3+idx*2+1}) egress (${in.size*2+idx*3} ${in.size*2+idx*3+1} ${in.size*2+idx*3+2})")

      val inBEgress = Mux1H(requestBOI(i).zipWithIndex.map { case (r,i) =>
        r -> (getIndex(inNodeMapping.keys.toSeq, inNames(i))*2 + 0).U
      })
      val inDEgress = Mux1H(requestDOI(i).zipWithIndex.map { case (r,i) =>
        r -> (getIndex(inNodeMapping.keys.toSeq, inNames(i))*2 + 1).U
      })

      combineFromFlit (outA.flit, out(i).a)
      combineFromFlit (outC.flit, out(i).c)
      combineFromFlit (outE.flit, out(i).e)

      splitToFlit(
        out(i).b, inB.flit, firstBO(i), lastBO(i), inBEgress,
        edgesOut(i).hasData(out(i).b.bits) || (~out(i).b.bits.mask =/= 0.U),
        Cat(requestBOIds(i), i.U(16.W), tsc)
      )
      splitToFlit(
        out(i).d, inD.flit, firstDO(i), lastDO(i), inDEgress,
        edgesOut(i).hasData(out(i).d.bits),
        Cat(requestDOIds(i), i.U(16.W), tsc)
      )

      if (debugPrintLatencies) {
        debugPrint("A", outA)
        debugPrint("C", outC)
        debugPrint("E", outE)
      }
    }
  }
}

