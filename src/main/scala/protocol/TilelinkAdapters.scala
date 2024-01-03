package constellation.protocol

import chisel3._
import chisel3.util._

import constellation.channel._
import constellation.noc._
import constellation.soc.{CanAttachToGlobalNoC}

import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.tilelink._

import scala.collection.immutable.{ListMap}

abstract class TLChannelToNoC[T <: TLChannel](gen: => T, edge: TLEdge, idToEgress: Int => Int)(implicit val p: Parameters) extends Module with TLFieldHelper {
  val flitWidth = minTLPayloadWidth(gen)
  val io = IO(new Bundle {
    val protocol = Flipped(Decoupled(gen))
    val flit = Decoupled(new IngressFlit(flitWidth))
  })
  def unique(x: Vector[Boolean]): Bool = (x.filter(x=>x).size <= 1).B

  // convert decoupled to irrevocable
  val q = Module(new Queue(gen, 1, pipe=true, flow=true))
  val protocol = q.io.deq

  val has_body = Wire(Bool())
  val body_fields = getBodyFields(protocol.bits)
  val const_fields = getConstFields(protocol.bits)
  val head = edge.first(protocol.bits, protocol.fire)
  val tail = edge.last(protocol.bits, protocol.fire)
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

  when (io.flit.fire && io.flit.bits.head) { is_body := true.B }
  when (io.flit.fire && io.flit.bits.tail) { is_body := false.B }
}

abstract class TLChannelFromNoC[T <: TLChannel](gen: => T)(implicit val p: Parameters) extends Module with TLFieldHelper {
  val flitWidth = minTLPayloadWidth(gen)
  val io = IO(new Bundle {
    val protocol = Decoupled(gen)
    val flit = Flipped(Decoupled(new EgressFlit(flitWidth)))
  })

  // Handle size = 1 gracefully (Chisel3 empty range is broken)
  def trim(id: UInt, size: Int): UInt = if (size <= 1) 0.U else id(log2Ceil(size)-1, 0)

  val protocol = Wire(Decoupled(gen))
  val body_fields = getBodyFields(protocol.bits)
  val const_fields = getConstFields(protocol.bits)

  val is_const = RegInit(true.B)
  val const_reg = Reg(UInt(const_fields.map(_.getWidth).sum.W))
  val const = Mux(io.flit.bits.head, io.flit.bits.payload, const_reg)
  io.flit.ready := (is_const && !io.flit.bits.tail) || protocol.ready
  protocol.valid := (!is_const || io.flit.bits.tail) && io.flit.valid

  def assign(i: UInt, sigs: Seq[Data]) = {
    var t = i
    for (s <- sigs.reverse) {
      s := t.asTypeOf(s.cloneType)
      t = t >> s.getWidth
    }
  }
  assign(const, const_fields)
  assign(io.flit.bits.payload, body_fields)

  when (io.flit.fire && io.flit.bits.head) { is_const := false.B; const_reg := io.flit.bits.payload }
  when (io.flit.fire && io.flit.bits.tail) { is_const := true.B }
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
  slaveToAEgress: Int => Int,
  sourceStart: Int
)(implicit p: Parameters) extends TLChannelToNoC(new TLBundleA(bundle), edgeIn, slaveToAEgress)(p) with HasAddressDecoder {
  has_body := edgeIn.hasData(protocol.bits) || (~protocol.bits.mask =/= 0.U)
  lazy val connectAIO = reacheableIO
  lazy val requestOH = outputPortFn(connectAIO).zipWithIndex.map { case (o, j) =>
    connectAIO(j).B && (unique(connectAIO) || o(protocol.bits.address))
  }
  q.io.enq <> io.protocol
  q.io.enq.bits.source := io.protocol.bits.source | sourceStart.U
}

class TLAFromNoC(edgeOut: TLEdge, bundle: TLBundleParameters)(implicit p: Parameters) extends TLChannelFromNoC(new TLBundleA(bundle))(p) {
  io.protocol <> protocol
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
  q.io.enq <> io.protocol
}

class TLBFromNoC(edgeIn: TLEdge, bundle: TLBundleParameters, sourceSize: Int)(implicit p: Parameters) extends TLChannelFromNoC(new TLBundleB(bundle))(p) {
  io.protocol <> protocol
  io.protocol.bits.source := trim(protocol.bits.source, sourceSize)
  when (io.flit.bits.head) { io.protocol.bits.mask := ~(0.U(io.protocol.bits.mask.getWidth.W)) }
}

class TLCToNoC(
  val edgeIn: TLEdge,
  val edgesOut: Seq[TLEdge],
  bundle: TLBundleParameters,
  slaveToCEgress: Int => Int,
  sourceStart: Int
)(implicit p: Parameters) extends TLChannelToNoC(new TLBundleC(bundle), edgeIn, slaveToCEgress)(p) with HasAddressDecoder {
  has_body := edgeIn.hasData(protocol.bits)
  lazy val connectCIO = releaseIO
  lazy val requestOH = outputPortFn(connectCIO).zipWithIndex.map {
    case (o, j) => connectCIO(j).B && (unique(connectCIO) || o(protocol.bits.address))
  }
  q.io.enq <> io.protocol
  q.io.enq.bits.source := io.protocol.bits.source | sourceStart.U
}

class TLCFromNoC(edgeOut: TLEdge, bundle: TLBundleParameters)(implicit p: Parameters) extends TLChannelFromNoC(new TLBundleC(bundle))(p) {
  io.protocol <> protocol
}

class TLDToNoC(
  edgeOut: TLEdge,
  edgesIn: Seq[TLEdge],
  bundle: TLBundleParameters,
  masterToDIngress: Int => Int,
  sourceStart: Int
)(implicit p: Parameters) extends TLChannelToNoC(new TLBundleD(bundle), edgeOut, masterToDIngress)(p) {
  has_body := edgeOut.hasData(protocol.bits)
  lazy val inputIdRanges = TLXbar.mapInputIds(edgesIn.map(_.client))
  lazy val requestOH = inputIdRanges.map { i => i.contains(protocol.bits.source) }
  q.io.enq <> io.protocol
  q.io.enq.bits.sink := io.protocol.bits.sink | sourceStart.U
}

class TLDFromNoC(edgeIn: TLEdge, bundle: TLBundleParameters, sourceSize: Int)(implicit p: Parameters) extends TLChannelFromNoC(new TLBundleD(bundle))(p)
{
  io.protocol <> protocol
  io.protocol.bits.source := trim(protocol.bits.source, sourceSize)
}

class TLEToNoC(
  val edgeIn: TLEdge,
  val edgesOut: Seq[TLEdge],
  bundle: TLBundleParameters,
  slaveToEEgress: Int => Int
)(implicit p: Parameters) extends TLChannelToNoC(new TLBundleE(bundle), edgeIn, slaveToEEgress)(p) {
  has_body := edgeIn.hasData(protocol.bits)
  lazy val outputIdRanges = TLXbar.mapOutputIds(edgesOut.map(_.manager))
  lazy val requestOH = outputIdRanges.map { o => o.contains(protocol.bits.sink) }
  q.io.enq <> io.protocol
}
class TLEFromNoC(edgeOut: TLEdge, bundle: TLBundleParameters, sourceSize: Int)(implicit p: Parameters) extends TLChannelFromNoC(new TLBundleE(bundle))(p) {
  io.protocol <> protocol
  io.protocol.bits.sink := trim(protocol.bits.sink, sourceSize)
}
