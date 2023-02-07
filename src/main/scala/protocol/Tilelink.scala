package constellation.protocol

import chisel3._
import chisel3.util._

import constellation.channel._
import constellation.noc._
import constellation.soc.{CanAttachToGlobalNoC}

import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.tilelink._

import scala.collection.immutable.{ListMap}

trait TLFieldHelper {
  def getBodyFields(b: TLChannel): Seq[Data] = b match {
    case b: TLBundleA => Seq(b.mask, b.data, b.corrupt)
    case b: TLBundleB => Seq(b.mask, b.data, b.corrupt)
    case b: TLBundleC => Seq(        b.data, b.corrupt)
    case b: TLBundleD => Seq(        b.data, b.corrupt)
    case b: TLBundleE => Seq()
  }
  def getConstFields(b: TLChannel): Seq[Data] = b match {
    case b: TLBundleA => Seq(b.opcode, b.param, b.size, b.source, b.address, b.user, b.echo                  )
    case b: TLBundleB => Seq(b.opcode, b.param, b.size, b.source, b.address                                  )
    case b: TLBundleC => Seq(b.opcode, b.param, b.size, b.source, b.address, b.user, b.echo                  )
    case b: TLBundleD => Seq(b.opcode, b.param, b.size, b.source,            b.user, b.echo, b.sink, b.denied)
    case b: TLBundleE => Seq(                                                                b.sink          )
  }
  def minTLPayloadWidth(b: TLChannel): Int = Seq(getBodyFields(b), getConstFields(b)).map(_.map(_.getWidth).sum).max
  def minTLPayloadWidth(b: TLBundle): Int = Seq(b.a, b.b, b.c, b.d, b.e).map(t => minTLPayloadWidth(t.bits)).max
}

abstract class TLChannelToNoC[T <: TLChannel](gen: => T, edge: TLEdge, idToEgress: Int => Int)(implicit val p: Parameters) extends Module with TLFieldHelper {
  val flitWidth = minTLPayloadWidth(gen)
  val io = IO(new Bundle {
    val protocol = Flipped(Decoupled(gen))
    val flit = Decoupled(new IngressFlit(flitWidth))
  })
  def unique(x: Vector[Boolean]): Bool = (x.filter(x=>x).size <= 1).B

  // convert decoupled to irrevocable
  val q = Module(new Queue(gen, 1, pipe=true, flow=true))
  q.io.enq <> io.protocol
  val protocol = q.io.deq

  val has_body = Wire(Bool())
  val body_fields = getBodyFields(protocol.bits)
  val const_fields = getConstFields(protocol.bits)
  val head = edge.first(protocol.bits, protocol.fire())
  val tail = edge.last(protocol.bits, protocol.fire())
  def requestOH: Seq[Bool]

  val body  = Cat( body_fields.filter(_.getWidth > 0).map(_.asUInt))
  val const = Cat(const_fields.filter(_.getWidth > 0).map(_.asUInt))

  val is_body = RegInit(false.B)
  io.flit.valid := protocol.valid
  protocol.ready := io.flit.ready && (is_body || !has_body)

  io.flit.bits.head       := head && !is_body
  io.flit.bits.tail       := tail && (is_body || !has_body)
  io.flit.bits.egress_id  := Mux1H(requestOH.zipWithIndex.map { case (r, i) =>
    r -> idToEgress(i).U
  })
  io.flit.bits.payload    := Mux(is_body, body, const)

  when (io.flit.fire() && io.flit.bits.head) { is_body := true.B }
  when (io.flit.fire() && io.flit.bits.tail) { is_body := false.B }
}

abstract class TLChannelFromNoC[T <: TLChannel](gen: => T)(implicit val p: Parameters) extends Module with TLFieldHelper {
  val flitWidth = minTLPayloadWidth(gen)
  val io = IO(new Bundle {
    val protocol = Decoupled(gen)
    val flit = Flipped(Decoupled(new EgressFlit(flitWidth)))
  })

  val protocol = io.protocol
  val body_fields = getBodyFields(protocol.bits)
  val const_fields = getConstFields(protocol.bits)

  val is_const = RegInit(true.B)
  val const_reg = Reg(UInt(const_fields.map(_.getWidth).sum.W))
  val const = Mux(io.flit.bits.head, io.flit.bits.payload, const_reg)
  io.flit.ready := (is_const && !io.flit.bits.tail) || io.protocol.ready
  io.protocol.valid := (!is_const || io.flit.bits.tail) && io.flit.valid

  def assign(i: UInt, sigs: Seq[Data]) = {
    var t = i
    for (s <- sigs.reverse) {
      s := t.asTypeOf(s.cloneType)
      t = t >> s.getWidth
    }
  }
  assign(const, const_fields)
  assign(io.flit.bits.payload, body_fields)

  when (io.flit.fire() && io.flit.bits.head) { is_const := false.B; const_reg := io.flit.bits.payload }
  when (io.flit.fire() && io.flit.bits.tail) { is_const := true.B }
}

trait HasAddressDecoder {
  // Filter a list to only those elements selected
  def filter[T](data: Seq[T], mask: Seq[Boolean]) = (data zip mask).filter(_._2).map(_._1)

  val edgeIn: TLEdge
  val edgesOut: Seq[TLEdge]
  lazy val reacheableIO = edgesOut.map { mp =>
    edgeIn.client.clients.exists { c => mp.manager.managers.exists { m =>
      c.visibility.exists { ca => m.address.exists { ma =>
        ca.overlaps(ma)
      }}
    }}
  }.toVector
  lazy val releaseIO = (edgesOut zip reacheableIO).map { case (mp, reachable) =>
    reachable && edgeIn.client.anySupportProbe && mp.manager.anySupportAcquireB
  }.toVector
  def outputPortFn(connectIO: Seq[Boolean]) = {
    val port_addrs = edgesOut.map(_.manager.managers.flatMap(_.address))
    val routingMask = AddressDecoder(filter(port_addrs, connectIO))
    val route_addrs = port_addrs.map(seq => AddressSet.unify(seq.map(_.widen(~routingMask)).distinct))
    route_addrs.map(seq => (addr: UInt) => seq.map(_.contains(addr)).reduce(_||_))
  }
}

class TLAToNoC(
  val edgeIn: TLEdge,
  val edgesOut: Seq[TLEdge],
  bundle: TLBundleParameters,
  slaveToAEgress: Int => Int
)(implicit p: Parameters) extends TLChannelToNoC(new TLBundleA(bundle), edgeIn, slaveToAEgress)(p) with HasAddressDecoder {
  has_body := edgeIn.hasData(protocol.bits) || (~protocol.bits.mask =/= 0.U)
  lazy val connectAIO = reacheableIO
  lazy val requestOH = outputPortFn(connectAIO).zipWithIndex.map { case (o, j) =>
    connectAIO(j).B && (unique(connectAIO) || o(protocol.bits.address))
  }
}

class TLAFromNoC(edgeOut: TLEdge, bundle: TLBundleParameters)(implicit p: Parameters) extends TLChannelFromNoC(new TLBundleA(bundle))(p) {
  when (io.flit.bits.head) { io.protocol.bits.mask := ~(0.U(io.protocol.bits.mask.getWidth.W)) }
}

class TLBToNoC(
  edgeOut: TLEdge,
  edgesIn: Seq[TLEdge],
  bundle: TLBundleParameters,
  masterToBIngress: Int => Int
)(implicit p: Parameters) extends TLChannelToNoC(new TLBundleB(bundle), edgeOut, masterToBIngress)(p) {
  has_body := edgeOut.hasData(protocol.bits) || (~protocol.bits.mask =/= 0.U)
  lazy val inputIdRanges = TLXbar.mapInputIds(edgesIn.map(_.client))
  lazy val requestOH = inputIdRanges.map { i => i.contains(protocol.bits.source) }
}

class TLBFromNoC(edgeIn: TLEdge, bundle: TLBundleParameters)(implicit p: Parameters) extends TLChannelFromNoC(new TLBundleB(bundle))(p) {
  when (io.flit.bits.head) { io.protocol.bits.mask := ~(0.U(io.protocol.bits.mask.getWidth.W)) }
}

class TLCToNoC(
  val edgeIn: TLEdge,
  val edgesOut: Seq[TLEdge],
  bundle: TLBundleParameters,
  slaveToCEgress: Int => Int
)(implicit p: Parameters) extends TLChannelToNoC(new TLBundleC(bundle), edgeIn, slaveToCEgress)(p) with HasAddressDecoder {
  has_body := edgeIn.hasData(protocol.bits)
  lazy val connectCIO = releaseIO
  lazy val requestOH = outputPortFn(connectCIO).zipWithIndex.map {
    case (o, j) => connectCIO(j).B && (unique(connectCIO) || o(protocol.bits.address))
  }
}

class TLCFromNoC(edgeOut: TLEdge, bundle: TLBundleParameters)(implicit p: Parameters) extends TLChannelFromNoC(new TLBundleC(bundle))(p)

class TLDToNoC(
  edgeOut: TLEdge,
  edgesIn: Seq[TLEdge],
  bundle: TLBundleParameters,
  masterToDIngress: Int => Int
)(implicit p: Parameters) extends TLChannelToNoC(new TLBundleD(bundle), edgeOut, masterToDIngress)(p) {
  has_body := edgeOut.hasData(protocol.bits)
  lazy val inputIdRanges = TLXbar.mapInputIds(edgesIn.map(_.client))
  lazy val requestOH = inputIdRanges.map { i => i.contains(protocol.bits.source) }
}

class TLDFromNoC(edgeIn: TLEdge, bundle: TLBundleParameters)(implicit p: Parameters) extends TLChannelFromNoC(new TLBundleD(bundle))(p)

class TLEToNoC(
  val edgeIn: TLEdge,
  val edgesOut: Seq[TLEdge],
  bundle: TLBundleParameters,
  slaveToEEgress: Int => Int
)(implicit p: Parameters) extends TLChannelToNoC(new TLBundleE(bundle), edgeIn, slaveToEEgress)(p) {
  has_body := edgeIn.hasData(protocol.bits)
  lazy val outputIdRanges = TLXbar.mapOutputIds(edgesOut.map(_.manager))
  lazy val requestOH = outputIdRanges.map { o => o.contains(protocol.bits.sink) }
}
class TLEFromNoC(edgeOut: TLEdge, bundle: TLBundleParameters)(implicit p: Parameters) extends TLChannelFromNoC(new TLBundleE(bundle))(p)

class TLMasterToNoC(
  edgeIn: TLEdge, edgesOut: Seq[TLEdge],
  sourceStart: Int, sourceSize: Int,
  wideBundle: TLBundleParameters,
  slaveToEgressOffset: Int => Int,
  flitWidth: Int
)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val tilelink = Flipped(new TLBundle(wideBundle))
    val flits = new Bundle {
      val a = Decoupled(new IngressFlit(flitWidth))
      val b = Flipped(Decoupled(new EgressFlit(flitWidth)))
      val c = Decoupled(new IngressFlit(flitWidth))
      val d = Flipped(Decoupled(new EgressFlit(flitWidth)))
      val e = Decoupled(new IngressFlit(flitWidth))
    }
  })
  // Handle size = 1 gracefully (Chisel3 empty range is broken)
  def trim(id: UInt, size: Int): UInt = if (size <= 1) 0.U else id(log2Ceil(size)-1, 0)

  val a = Module(new TLAToNoC(edgeIn, edgesOut, wideBundle, (i) => slaveToEgressOffset(i) + 0))
  val b = Module(new TLBFromNoC(edgeIn, wideBundle))
  val c = Module(new TLCToNoC(edgeIn, edgesOut, wideBundle, (i) => slaveToEgressOffset(i) + 1))
  val d = Module(new TLDFromNoC(edgeIn, wideBundle))
  val e = Module(new TLEToNoC(edgeIn, edgesOut, wideBundle, (i) => slaveToEgressOffset(i) + 2))
  a.io.protocol <> io.tilelink.a
  a.io.protocol.bits.source := io.tilelink.a.bits.source | sourceStart.U
  io.tilelink.b <> b.io.protocol
  io.tilelink.b.bits.source := trim(b.io.protocol.bits.source, sourceSize)
  c.io.protocol <> io.tilelink.c
  c.io.protocol.bits.source := io.tilelink.c.bits.source | sourceStart.U
  io.tilelink.d <> d.io.protocol
  io.tilelink.d.bits.source := trim(d.io.protocol.bits.source, sourceSize)
  e.io.protocol <> io.tilelink.e

  io.flits.a <> a.io.flit
  b.io.flit  <> io.flits.b
  io.flits.c <> c.io.flit
  d.io.flit  <> io.flits.d
  io.flits.e <> e.io.flit
}

class TLSlaveToNoC(
  edgeOut: TLEdge, edgesIn: Seq[TLEdge],
  sourceStart: Int, sourceSize: Int,
  wideBundle: TLBundleParameters,
  masterToEgressOffset: Int => Int,
  flitWidth: Int
)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val tilelink = new TLBundle(wideBundle)
    val flits = new Bundle {
      val a = Flipped(Decoupled(new EgressFlit(flitWidth)))
      val b = Decoupled(new IngressFlit(flitWidth))
      val c = Flipped(Decoupled(new EgressFlit(flitWidth)))
      val d = Decoupled(new IngressFlit(flitWidth))
      val e = Flipped(Decoupled(new EgressFlit(flitWidth)))
    }
  })

  // Handle size = 1 gracefully (Chisel3 empty range is broken)
  def trim(id: UInt, size: Int): UInt = if (size <= 1) 0.U else id(log2Ceil(size)-1, 0)

  val a = Module(new TLAFromNoC(edgeOut, wideBundle))
  val b = Module(new TLBToNoC(edgeOut, edgesIn, wideBundle, (i) => masterToEgressOffset(i) + 0))
  val c = Module(new TLCFromNoC(edgeOut, wideBundle))
  val d = Module(new TLDToNoC(edgeOut, edgesIn, wideBundle, (i) => masterToEgressOffset(i) + 1))
  val e = Module(new TLEFromNoC(edgeOut, wideBundle))
  io.tilelink.a <> a.io.protocol
  b.io.protocol <> io.tilelink.b
  io.tilelink.c <> c.io.protocol
  d.io.protocol <> io.tilelink.d
  d.io.protocol.bits.sink := io.tilelink.d.bits.sink | sourceStart.U
  io.tilelink.e <> e.io.protocol
  io.tilelink.e.bits.sink := trim(e.io.protocol.bits.sink, sourceSize)

  a.io.flit  <> io.flits.a
  io.flits.b <> b.io.flit
  c.io.flit  <> io.flits.c
  io.flits.d <> d.io.flit
  e.io.flit  <> io.flits.e
}

class TileLinkInterconnectInterface(edgesIn: Seq[TLEdge], edgesOut: Seq[TLEdge])(implicit val p: Parameters) extends Bundle {
  val in = MixedVec(edgesIn.map { e => Flipped(new TLBundle(e.bundle)) })
  val out = MixedVec(edgesOut.map { e => new TLBundle(e.bundle) })
}

// BEGIN: TileLinkProtocolParams
case class TileLinkProtocolParams(
  edgesIn: Seq[TLEdge],
  edgesOut: Seq[TLEdge],
  edgeInNodes: Seq[Int],
  edgeOutNodes: Seq[Int]
) extends ProtocolParams with TLFieldHelper {
  // END: TileLinkProtocolParams
  require(edgesIn.size == edgeInNodes.size && edgesOut.size == edgeOutNodes.size)
  val wideBundle = TLBundleParameters.union(edgesIn.map(_.bundle) ++ edgesOut.map(_.bundle))
  val inputIdRanges = TLXbar.mapInputIds(edgesIn.map(_.client))
  val outputIdRanges = TLXbar.mapOutputIds(edgesOut.map(_.manager))

  val minPayloadWidth = minTLPayloadWidth(new TLBundle(wideBundle))
  val ingressNodes = (edgeInNodes.map(u => Seq.fill(3) (u)) ++ edgeOutNodes.map(u => Seq.fill (2) {u})).flatten
  val egressNodes = (edgeInNodes.map(u => Seq.fill(2) (u)) ++ edgeOutNodes.map(u => Seq.fill (3) {u})).flatten
  val nVirtualNetworks = 5
  val vNetBlocking = (blocker: Int, blockee: Int) => blocker < blockee
  val flows = edgesIn.zipWithIndex.map { case (edgeIn, ii) => edgesOut.zipWithIndex.map { case (edgeOut, oi) =>
    val reachable = edgeIn.client.clients.exists { c => edgeOut.manager.managers.exists { m =>
      c.visibility.exists { ca => m.address.exists { ma =>
        ca.overlaps(ma)
      }}
    }}
    val probe = edgeIn.client.anySupportProbe && edgeOut.manager.managers.exists(_.regionType >= RegionType.TRACKED)
    val release = edgeIn.client.anySupportProbe && edgeOut.manager.anySupportAcquireB
    ( (if (reachable) Some(FlowParams(ii * 3 + 0                   , oi * 3 + 0 + edgesIn.size * 2, 4)) else None) ++ // A
      (if (probe    ) Some(FlowParams(oi * 2 + 0 + edgesIn.size * 3, ii * 2 + 0                   , 3)) else None) ++ // B
      (if (release  ) Some(FlowParams(ii * 3 + 1                   , oi * 3 + 1 + edgesIn.size * 2, 2)) else None) ++ // C
      (if (reachable) Some(FlowParams(oi * 2 + 1 + edgesIn.size * 3, ii * 2 + 1                   , 1)) else None) ++ // D
      (if (release  ) Some(FlowParams(ii * 3 + 2                   , oi * 3 + 2 + edgesIn.size * 2, 0)) else None))   // E
  }}.flatten.flatten
  def genIO()(implicit p: Parameters): Data = new TileLinkInterconnectInterface(edgesIn, edgesOut)

  def interface(terminals: NoCTerminalIO,
    ingressOffset: Int, egressOffset: Int, protocol: Data)(implicit p: Parameters) = {
    val ingresses = terminals.ingress
    val egresses = terminals.egress
    protocol match {
      case protocol: TileLinkInterconnectInterface => {
        edgesIn.zipWithIndex.map { case (e,i) =>
          val nif_master = Module(new TLMasterToNoC(
            e, edgesOut, inputIdRanges(i).start, inputIdRanges(i).size,
            wideBundle,
            (s) => s * 3 + edgesIn.size * 2 + egressOffset,
            minPayloadWidth
          ))
          nif_master.io.tilelink := DontCare
          nif_master.io.tilelink.a.valid := false.B
          nif_master.io.tilelink.c.valid := false.B
          nif_master.io.tilelink.e.valid := false.B

          nif_master.io.tilelink.a <> protocol.in(i).a
          protocol.in(i).d <> nif_master.io.tilelink.d

          if (protocol.in(i).params.hasBCE) {
            protocol.in(i).b <> nif_master.io.tilelink.b
            nif_master.io.tilelink.c <> protocol.in(i).c
            protocol.in(i).e <> nif_master.io.tilelink.e
          }

          ingresses(i * 3 + 0).flit <> nif_master.io.flits.a
          ingresses(i * 3 + 1).flit <> nif_master.io.flits.c
          ingresses(i * 3 + 2).flit <> nif_master.io.flits.e
          nif_master.io.flits.b <> egresses(i * 2 + 0).flit
          nif_master.io.flits.d <> egresses(i * 2 + 1).flit
        }
        edgesOut.zipWithIndex.map { case (e,i) =>
          val nif_slave = Module(new TLSlaveToNoC(
            e, edgesIn, outputIdRanges(i).start, outputIdRanges(i).size,
            wideBundle,
            (s) => s * 2 + egressOffset,
            minPayloadWidth
          ))
          nif_slave.io.tilelink := DontCare
          nif_slave.io.tilelink.b.valid := false.B
          nif_slave.io.tilelink.d.valid := false.B

          protocol.out(i).a <> nif_slave.io.tilelink.a
          nif_slave.io.tilelink.d <> protocol.out(i).d

          if (protocol.out(i).params.hasBCE) {
            nif_slave.io.tilelink.b <> protocol.out(i).b
            protocol.out(i).c <> nif_slave.io.tilelink.c
            nif_slave.io.tilelink.e <> protocol.out(i).e
          }

          ingresses(i * 2 + 0 + edgesIn.size * 3).flit <> nif_slave.io.flits.b
          ingresses(i * 2 + 1 + edgesIn.size * 3).flit <> nif_slave.io.flits.d
          nif_slave.io.flits.a <> egresses(i * 3 + 0 + edgesIn.size * 2).flit
          nif_slave.io.flits.c <> egresses(i * 3 + 1 + edgesIn.size * 2).flit
          nif_slave.io.flits.e <> egresses(i * 3 + 2 + edgesIn.size * 2).flit
        }
      }
    }
  }
}

abstract class TLNoCLike(implicit p: Parameters) extends LazyModule {
  val node = new TLNexusNode(
    clientFn  = { seq =>
      seq(0).v1copy(
        echoFields    = BundleField.union(seq.flatMap(_.echoFields)),
        requestFields = BundleField.union(seq.flatMap(_.requestFields)),
        responseKeys  = seq.flatMap(_.responseKeys).distinct,
        minLatency = seq.map(_.minLatency).min,
        clients = (TLXbar.mapInputIds(seq) zip seq) flatMap { case (range, port) =>
          port.clients map { client => client.v1copy(
            sourceId = client.sourceId.shift(range.start)
          )}
        }
      )
    },
    managerFn = { seq =>
      val fifoIdFactory = TLXbar.relabeler()
      seq(0).v1copy(
        responseFields = BundleField.union(seq.flatMap(_.responseFields)),
        requestKeys = seq.flatMap(_.requestKeys).distinct,
        minLatency = seq.map(_.minLatency).min,
        endSinkId = TLXbar.mapOutputIds(seq).map(_.end).max,
        managers = seq.flatMap { port =>
          require (port.beatBytes == seq(0).beatBytes,
            s"TLNoC (data widths don't match: ${port.managers.map(_.name)} has ${port.beatBytes}B vs ${seq(0).managers.map(_.name)} has ${seq(0).beatBytes}B")
          val fifoIdMapper = fifoIdFactory()
          port.managers map { manager => manager.v1copy(
            fifoId = manager.fifoId.map(fifoIdMapper(_))
          )}
        }
      )
    }
  )
}

abstract class TLNoCModuleImp(outer: LazyModule) extends LazyModuleImp(outer) {
  val edgesIn: Seq[TLEdge]
  val edgesOut: Seq[TLEdge]
  val nodeMapping: DiplomaticNetworkNodeMapping
  val nocName: String
  lazy val inNames  = nodeMapping.genUniqueName(edgesIn.map(_.master.masters.map(_.name)))
  lazy val outNames = nodeMapping.genUniqueName(edgesOut.map(_.slave.slaves.map(_.name)))
  lazy val edgeInNodes = nodeMapping.getNodesIn(inNames)
  lazy val edgeOutNodes = nodeMapping.getNodesOut(outNames)
  lazy val protocolParams = TileLinkProtocolParams(
    edgesIn = edgesIn,
    edgesOut = edgesOut,
    edgeInNodes = edgeInNodes.flatten,
    edgeOutNodes = edgeOutNodes.flatten
  )
  def printNodeMappings() {
    println(s"Constellation: TLNoC $nocName inwards mapping:")
    for ((n, i) <- inNames zip edgeInNodes) {
      val node = i.map(_.toString).getOrElse("X")
      println(s"  $node <- $n")
    }

    println(s"Constellation: TLNoC $nocName outwards mapping:")
    for ((n, i) <- outNames zip edgeOutNodes) {
      val node = i.map(_.toString).getOrElse("X")
      println(s"  $node <- $n")
    }
  }
}

// Instantiates a private TLNoC. Replaces the TLXbar
// BEGIN: TLNoCParams
case class TLNoCParams(
  nodeMappings: DiplomaticNetworkNodeMapping,
  nocParams: NoCParams = NoCParams()
)
class TLNoC(params: TLNoCParams, name: String = "test")(implicit p: Parameters) extends TLNoCLike {
  // END: TLNoCParams
  lazy val module = new TLNoCModuleImp(this) {
    val (io_in, edgesIn) = node.in.unzip
    val (io_out, edgesOut) = node.out.unzip
    val nodeMapping = params.nodeMappings
    val nocName = name

    printNodeMappings()
    val noc = Module(new ProtocolNoC(ProtocolNoCParams(
      params.nocParams.copy(hasCtrl = false, nocName=name),
      Seq(protocolParams)
    )))

    noc.io.protocol(0) match {
      case protocol: TileLinkInterconnectInterface => {
        (protocol.in zip io_in).foreach { case (l,r) => l <> r }
        (io_out zip protocol.out).foreach { case (l,r) => l <> r }
      }
    }
  }
}


// Maps this interconnect onto a global NoC
class TLGlobalNoC(params: TLNoCParams, name: String = "test")(implicit p: Parameters) extends TLNoCLike {
  lazy val module = new TLNoCModuleImp(this) with CanAttachToGlobalNoC {
    val (io_in, edgesIn) = node.in.unzip
    val (io_out, edgesOut) = node.out.unzip
    val nodeMapping = params.nodeMappings
    val nocName = name

    printNodeMappings()
    val io_global = IO(Flipped(protocolParams.genIO()))
    io_global match {
      case protocol: TileLinkInterconnectInterface => {
        (protocol.in zip io_in).foreach { case (l,r) => l <> r }
        (io_out zip protocol.out).foreach { case (l,r) => l <> r }
      }
    }

  }
}
