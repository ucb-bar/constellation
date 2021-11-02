package constellation

import chisel3._
import chisel3.util._

import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util._
import constellation.topology.MasterAllocTables


class TLNoC(inNodeMapping: Seq[Int], outNodeMapping: Seq[Int])(implicit p: Parameters) extends TLXbar {
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
        in(i).a :<> io_in(i).a
        in(i).a.bits.source := io_in(i).a.bits.source | r.start.U
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
        in(i).c :<> io_in(i).c
        in(i).c.bits.source := io_in(i).c.bits.source | r.start.U
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
        in(i).e :<> io_in(i).e
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
        out(o).b :<> io_out(o).b
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
        out(o).d :<> io_out(o).d
        out(o).d.bits.sink := io_out(o).d.bits.sink | r.start.U
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
    val requestAIO = (connectAIO zip addressA) map { case (c, i) => outputPortFns(c).map { o => unique(c) || o(i) } }
    val requestCIO = (connectCIO zip addressC) map { case (c, i) => outputPortFns(c).map { o => unique(c) || o(i) } }
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

    require(in.size == inNodeMapping.size,
      s"TL Inwards count must match mapping size ${in.size} != ${inNodeMapping.size}")
    require(out.size == outNodeMapping.size,
      s"TL Outwards count must match mapping size ${out.size} != ${outNodeMapping.size}")


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


    def connectivity(src: Int, dst: Int, vNetId: Int) = {
      if (isAIn(src) && isAOut(dst)) {
        connectAIO(src/3)((dst-in.size*2)/3) && vNetId == 4
      } else if (isBIn(src) && isBOut(dst)) {
        connectBOI((src-in.size*3)/2)(dst/2) && vNetId == 3
      } else if (isCIn(src) && isCOut(dst)) {
        connectCIO(src/3)((dst-in.size*2)/3) && vNetId == 2
      } else if (isDIn(src) && isDOut(dst)) {
        connectDOI((src-in.size*3)/2)(dst/2) && vNetId == 1
      } else if (isEIn(src) && isEOut(dst)) {
        connectEIO(src/3)((dst-in.size*2)/3) && vNetId == 0
      } else {
        false
      }
    }

    val debugPrintLatencies = false
    val wb = new TLBundle(wide_bundle)
    val payloadWidth = Seq(wb.a, wb.b, wb.c, wb.d, wb.e).map(_.bits.getWidth).max

    val noc = Module(new NoC()(p.alterPartial({
      case NoCKey =>
        p(NoCKey).copy(
          flitPayloadBits = payloadWidth + (if (debugPrintLatencies) 64 else 0),
          ingressNodes = (Seq.tabulate (in.size) { i => Seq.fill(3) { inNodeMapping(i) } } ++
            Seq.tabulate(out.size) { i => Seq.fill(2) { outNodeMapping(i) } }).flatten,
          egressNodes = (Seq.tabulate (in.size) { i => Seq.fill(2) { inNodeMapping(i) } } ++
            Seq.tabulate(out.size) { i => Seq.fill(3) { outNodeMapping(i) } }).flatten,
          terminalConnectivity = connectivity
        )
    })))

    val tsc = RegInit(0.U(32.W))
    tsc := tsc + 1.U
    def debugPrint(channel: String, payload: UInt, fire: Bool) = {
      when (fire) {
        printf(s"TLNoC, $channel, %d, %d, %d\n",
          tsc - payload(payloadWidth+32-1,payloadWidth),
          payload(payloadWidth+48-1,payloadWidth+32),
          payload >> (payloadWidth + 48))
      }
    }

    for (i <- 0 until in.size) {
      val inA  = noc.io.ingress (i*3)
      val outB = noc.io.egress  (i*2)
      val inC  = noc.io.ingress (i*3+1)
      val outD = noc.io.egress  (i*2+1)
      val inE  = noc.io.ingress (i*3+2)

      inA.flit.valid := in(i).a.valid
      in(i).a.ready := inA.flit.ready
      inA.flit.bits.head := firstAI(i)
      inA.flit.bits.tail := lastAI(i)
      inA.flit.bits.vnet_id := 4.U
      inA.flit.bits.egress_id := (in.size*2+0).U +& (requestAIIds(i) * 3.U)
      inA.flit.bits.virt_channel_id := 0.U
      inA.flit.bits.payload := in(i).a.bits.asUInt | (Cat(requestAIIds(i), i.U(16.W), tsc) << payloadWidth)

      in(i).b.valid := outB.flit.valid
      outB.flit.ready := in(i).b.ready
      in(i).b.bits := outB.flit.bits.payload.asTypeOf(new TLBundleB(wide_bundle))

      inC.flit.valid := in(i).c.valid
      in(i).c.ready := inC.flit.ready
      inC.flit.bits.head := firstCI(i)
      inC.flit.bits.tail := lastCI(i)
      inC.flit.bits.vnet_id := 2.U
      inC.flit.bits.egress_id := (in.size*2+1).U +& (requestCIIds(i) * 3.U)
      inC.flit.bits.virt_channel_id := 2.U
      inC.flit.bits.payload := in(i).c.bits.asUInt | (Cat(requestCIIds(i), i.U(16.W), tsc) << payloadWidth)

      in(i).d.valid := outD.flit.valid
      outD.flit.ready := in(i).d.ready
      in(i).d.bits := outD.flit.bits.asTypeOf(new TLBundleD(wide_bundle))

      inE.flit.valid := in(i).e.valid
      in(i).e.ready := inE.flit.ready
      inE.flit.bits.head := firstEI(i)
      inE.flit.bits.tail := lastEI(i)
      inE.flit.bits.vnet_id := 0.U
      inE.flit.bits.egress_id := (in.size*2+2).U +& (requestEIIds(i) * 3.U)
      inE.flit.bits.virt_channel_id := 0.U
      inE.flit.bits.payload := in(i).e.bits.asUInt | (Cat(requestEIIds(i), i.U(16.W), tsc) << payloadWidth)

      if (debugPrintLatencies) {
        debugPrint("B", outB.flit.bits.payload, in(i).b.fire() && edgesIn(i).last(in(i).b))
        debugPrint("D", outD.flit.bits.payload, in(i).d.fire() && edgesIn(i).last(in(i).d))
      }
    }

    for (i <- 0 until out.size) {
      val outA  = noc.io.egress  (in.size*2+i*3)
      val inB   = noc.io.ingress (in.size*3+i*2)
      val outC  = noc.io.egress  (in.size*2+i*3+1)
      val inD   = noc.io.ingress (in.size*3+i*2+1)
      val outE  = noc.io.egress  (in.size*2+i*3+2)

      out(i).a.valid := outA.flit.valid
      outA.flit.ready := out(i).a.ready
      out(i).a.bits := outA.flit.bits.payload.asTypeOf(new TLBundleA(wide_bundle))

      inB.flit.valid := out(i).b.valid
      out(i).b.ready := inB.flit.ready
      inB.flit.bits.head := firstBO(i)
      inB.flit.bits.tail := lastBO(i)
      inB.flit.bits.vnet_id := 3.U
      inB.flit.bits.egress_id := 0.U +& (requestBOIds(i) * 2.U)
      inB.flit.bits.virt_channel_id := 0.U
      inB.flit.bits.payload := out(i).b.bits.asUInt | (Cat(requestBOIds(i), i.U(16.W), tsc << payloadWidth))

      out(i).c.valid := outC.flit.valid
      outC.flit.ready := out(i).c.ready
      out(i).c.bits := outC.flit.bits.payload.asTypeOf(new TLBundleC(wide_bundle))

      inD.flit.valid := out(i).d.valid
      out(i).d.ready := inD.flit.ready
      inD.flit.bits.head := firstDO(i)
      inD.flit.bits.tail := lastDO(i)
      inD.flit.bits.vnet_id := 1.U
      inD.flit.bits.egress_id := 1.U +& (requestDOIds(i) * 2.U)
      inD.flit.bits.virt_channel_id := 0.U
      inD.flit.bits.payload := out(i).d.bits.asUInt | (Cat(requestDOIds(i), i.U(16.W), tsc << payloadWidth))

      out(i).e.valid := outE.flit.valid
      outE.flit.ready := out(i).e.ready
      out(i).e.bits := outE.flit.bits.payload.asTypeOf(new TLBundleE(wide_bundle))

      if (debugPrintLatencies) {
        debugPrint("A", outA.flit.bits.payload, out(i).a.fire() && edgesOut(i).last(out(i).a))
        debugPrint("C", outC.flit.bits.payload, out(i).c.fire() && edgesOut(i).last(out(i).c))
        debugPrint("E", outE.flit.bits.payload, out(i).e.fire() && edgesOut(i).last(out(i).e))
      }
    }
  }
}

case class ConstellationSystemBusParams(
  params: SystemBusParams,
  inNodeMapping: Seq[Int],
  outNodeMapping: Seq[Int]
) extends TLBusWrapperInstantiationLike {
  def instantiate(context: HasTileLinkLocations, loc: Location[TLBusWrapper])(implicit p: Parameters): SystemBus = {
    val constellation = LazyModule(new ConstellationSystemBus(params, inNodeMapping, outNodeMapping))
    constellation.suggestName(loc.name)
    context.tlBusWrapperLocationMap += (loc -> constellation)
    constellation
  }
}


class ConstellationSystemBus(params: SystemBusParams, inNodeMapping: Seq[Int], outNodeMapping: Seq[Int])
  (implicit p: Parameters) extends SystemBus(params) {
  val system_bus_noc = LazyModule(new TLNoC(inNodeMapping, outNodeMapping))

  override val inwardNode: TLInwardNode = system_bus_noc.node
  override val outwardNode: TLOutwardNode = system_bus_noc.node
  override def busView: TLEdge = system_bus_noc.node.edges.in.head

}

class WithConstellationNoCSystemBus(inNodeMapping: Seq[Int], outNodeMapping: Seq[Int])
    extends Config(
  new WithNNonblockingVirtualNetworksWithSharing(5, 2) ++
  new Config((site, here, up) => {
    case TLNetworkTopologyLocated(InSubsystem) =>
      up(TLNetworkTopologyLocated(InSubsystem), site).map(topo =>
        topo match {
          case j: JustOneBusTopologyParams =>
            new TLBusWrapperTopology(j.instantiations.map(inst => inst match {
              case (SBUS, sbus_params: SystemBusParams) =>
                (SBUS, ConstellationSystemBusParams(sbus_params, inNodeMapping, outNodeMapping))
              case a => a
            }), j.connections)
          case x => x
        }
      )
  })
)
