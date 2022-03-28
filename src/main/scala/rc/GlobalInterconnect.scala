package constellation.rc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util._

import constellation.{NoC, NoCKey, NoCTerminalIO}
import constellation.channel.{TerminalChannel, UserIngressParams, UserEgressParams}
import constellation.topology.{TerminalPlaneTopology}

import scala.collection.immutable.ListMap

case class GlobalTLNoCParams(
  busMap: ListMap[TLBusWrapperLocation, (Seq[Int], Seq[Int])] = ListMap[TLBusWrapperLocation, (Seq[Int], Seq[Int])](),
  payloadWidth: Int = 80)

case object InstantiateGlobalTLInterconnect extends Field[GlobalTLNoCParams](GlobalTLNoCParams())

trait CanHaveGlobalTLInterconnect { this: BaseSubsystem =>
  def globalNoCWidth = p(InstantiateGlobalTLInterconnect).payloadWidth
  def busMap = p(InstantiateGlobalTLInterconnect).busMap
  def supportedBuses = busMap.keys.toList
  def hasGlobalTLInterconnect = supportedBuses.size > 0
  def inNodeMapping(bus: TLBusWrapperLocation) = busMap(bus)._1
  def outNodeMapping(bus: TLBusWrapperLocation) = busMap(bus)._2
  def nIngresses(bus: TLBusWrapperLocation) = {
    val (in, out) = (inNodeMapping(bus), outNodeMapping(bus))
    in.size * 3 + out.size * 2
  }
  def nEgresses(bus: TLBusWrapperLocation) = {
    val (in, out) = (inNodeMapping(bus), outNodeMapping(bus))
    out.size * 3 + in.size * 2
  }
  val (ingressTerminalOffset, egressTerminalOffset) = p(NoCKey).topology match {
    case t: TerminalPlaneTopology => (t.base.nNodes, t.base.nNodes * 2)
    case _ => (0, 0)
  }
  def ingressOffset(bus: TLBusWrapperLocation) = supportedBuses.map(b => nIngresses(b))
    .scanLeft(0)(_ + _).toSeq(supportedBuses.indexOf(bus))
  def egressOffset(bus: TLBusWrapperLocation) = supportedBuses.map(b => nEgresses(b))
    .scanLeft(0)(_ + _).toSeq(supportedBuses.indexOf(bus))
  def vNetOffset(bus: TLBusWrapperLocation) = supportedBuses.indexOf(bus) * 5


  val (ingressParams, egressParams) = supportedBuses.map(bus => {
    val (in, out) = (inNodeMapping(bus), outNodeMapping(bus))
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
        vNetId == 4
      } else if (isBIn(src) && isBOut(dst)) {
        vNetId == 3
      } else if (isCIn(src) && isCOut(dst)) {
        vNetId == 2
      } else if (isDIn(src) && isDOut(dst)) {
        vNetId == 1
      } else if (isEIn(src) && isEOut(dst)) {
        vNetId == 0
      } else {
        false
      }
    }

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

    val ingressParams = (inNodeMapping(bus).map(i => Seq(i, i, i)) ++ outNodeMapping(bus).map(i => Seq(i, i)))
      .flatten.zipWithIndex.map { case (i, iId) => UserIngressParams(
        destId = i + ingressTerminalOffset,
        possibleEgresses = (0 until nEgresses(bus))
          .filter(e => connectivity(iId, e, ingressVNets(iId)))
          .map(_ + egressOffset(bus))
          .toSet,
        vNetId = ingressVNets(iId) + vNetOffset(bus),
        payloadBits = globalNoCWidth
      )}
    val egressParams = (inNodeMapping(bus).map(i => Seq(i, i)) ++ outNodeMapping(bus).map(i => Seq(i, i, i)))
      .flatten.zipWithIndex.map { case (e, eId) => UserEgressParams(
        srcId = e + egressTerminalOffset,
        payloadBits = globalNoCWidth
      )}
    (ingressParams, egressParams)
  }).unzip

  val noc = hasGlobalTLInterconnect.option(LazyModule(new NoC()(p.alterPartial({
    case NoCKey => p(NoCKey).copy(
      routerParams = (i: Int) => p(NoCKey).routerParams(i).copy(payloadBits = globalNoCWidth),
      ingresses = ingressParams.flatten,
      egresses = egressParams.flatten,
      nocName = "global_tl"
    )
  }))))
  if (hasGlobalTLInterconnect) {
    require(p(NoCKey).nVirtualNetworks >= 5 * supportedBuses.size)
  }


  def getSubnetTerminalChannels(bus: TLBusWrapperLocation): BundleBridgeSink[NoCTerminalIO] = {
    val source = this { BundleBridgeSource[NoCTerminalIO](() =>
      new NoCTerminalIO(
        noc.get.globalIngressParams.drop(ingressOffset(bus)).take(nIngresses(bus)),
        noc.get.globalEgressParams .drop( egressOffset(bus)).take( nEgresses(bus))
      )
    )}
    val sink = BundleBridgeSink[NoCTerminalIO]()
    sink := source
    this { InModuleBody { /* connect source to noc */
      (noc.get.module.io.ingress.drop(ingressOffset(bus)).take(nIngresses(bus)) zip source.out(0)._1.ingress)
        .map(t => {
          t._1 <> t._2
          t._1.flit.bits.egress_id := t._2.flit.bits.egress_id + egressOffset(bus).U
        })
      (noc.get.module.io.egress .drop( egressOffset(bus)).take(nEgresses (bus)) zip source.out(0)._1.egress)
        .map(t => {
          t._2 <> t._1
          t._2.flit.bits.egress_id := t._1.flit.bits.egress_id - egressOffset(bus).U
        })
    } }
    sink
  }

}

