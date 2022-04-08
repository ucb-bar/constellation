package constellation.rc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util._
import freechips.rocketchip.prci._

import constellation.noc.{NoC, NoCTerminalIO, NoCParams, NoCKey}
import constellation.channel.{TerminalChannel, UserIngressParams, UserEgressParams, FlowParams}
import constellation.topology.{TerminalPlaneTopology}

import scala.collection.immutable.{ListSet}

case class GlobalTLNoCParams(
  supportedBuses: ListSet[TLBusWrapperLocation] = ListSet[TLBusWrapperLocation](),
  payloadWidth: Int = 80,
  nocParams: NoCParams = NoCParams()
)

case object GlobalTLInterconnectKey extends Field[GlobalTLNoCParams](GlobalTLNoCParams())

trait CanHaveGlobalTLInterconnect { this: BaseSubsystem =>
  val globalTLParams = p(GlobalTLInterconnectKey)
  val nocParams = globalTLParams.nocParams
  val globalNoCWidth = globalTLParams.payloadWidth
  val supportedBuses = globalTLParams.supportedBuses.toSeq
  val hasGlobalTLInterconnect = supportedBuses.size > 0
  val nodeMappings = supportedBuses.map(b => b -> p(ConstellationTLNetworkNodeMappingKey(b))).toMap
  def inNodeMapping(bus: TLBusWrapperLocation) = nodeMappings(bus).inNodeMapping
  def outNodeMapping(bus: TLBusWrapperLocation) = nodeMappings(bus).outNodeMapping
  def nIngresses(bus: TLBusWrapperLocation) = {
    val (in, out) = (inNodeMapping(bus), outNodeMapping(bus))
    in.size * 3 + out.size * 2
  }
  def nEgresses(bus: TLBusWrapperLocation) = {
    val (in, out) = (inNodeMapping(bus), outNodeMapping(bus))
    out.size * 3 + in.size * 2
  }
  val (ingressTerminalOffset, egressTerminalOffset) = nocParams.topology match {
    case t: TerminalPlaneTopology => (t.base.nNodes, t.base.nNodes * 2)
    case _ => (0, 0)
  }
  def ingressOffset(bus: TLBusWrapperLocation) = supportedBuses.map(b => nIngresses(b))
    .scanLeft(0)(_ + _).toSeq(supportedBuses.indexOf(bus))
  def egressOffset(bus: TLBusWrapperLocation) = supportedBuses.map(b => nEgresses(b))
    .scanLeft(0)(_ + _).toSeq(supportedBuses.indexOf(bus))
  def vNetOffset(bus: TLBusWrapperLocation) = supportedBuses.indexOf(bus) * 5

  val (ingressParams, egressParams, flowParams) = supportedBuses.map(bus => {
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
    val flowParams = (0 until in.size).map { iId => (0 until out.size).map { oId => {
      val a = FlowParams(iId * 3    , in.size * 2 + oId * 3    , 4)
      val c = FlowParams(iId * 3 + 1, in.size * 2 + oId * 3 + 1, 2)
      val e = FlowParams(iId * 3 + 2, in.size * 2 + oId * 3 + 2, 0)

      val b = FlowParams(in.size * 3 + oId * 2    , iId * 2    , 3)
      val d = FlowParams(in.size * 3 + oId * 2 + 1, iId * 2 + 1, 1)
      Seq(a, b, c, d, e)
    }}}.flatten.flatten.map(f => f.copy(
      vNetId = f.vNetId + vNetOffset(bus),
      ingressId = f.ingressId + ingressOffset(bus),
      egressId = f.egressId + egressOffset(bus)
    ))

    val ingressParams = (inNodeMapping(bus).values.map(i => Seq(i, i, i)) ++ outNodeMapping(bus).values.map(i => Seq(i, i)))
      .flatten.zipWithIndex.map { case (i, iId) => UserIngressParams(
        destId = i + ingressTerminalOffset,
        vNetId = ingressVNets(iId) + vNetOffset(bus),
        payloadBits = globalNoCWidth
      )}
    val egressParams = (inNodeMapping(bus).values.map(i => Seq(i, i)) ++ outNodeMapping(bus).values.map(i => Seq(i, i, i)))
      .flatten.zipWithIndex.map { case (e, eId) => UserEgressParams(
        srcId = e + egressTerminalOffset,
        vNetId = egressVNets(eId) + vNetOffset(bus),
        payloadBits = globalNoCWidth
      )}
    (ingressParams, egressParams, flowParams)
  }).unzip3

  lazy val nocClockDomain = this { LazyModule(new ClockSinkDomain) }
  nocClockDomain.clockNode := locateTLBusWrapper(SBUS).fixedClockNode
  val nocP = p.alterPartial({
    case NoCKey => nocParams.copy(
      routerParams = (i: Int) => nocParams.routerParams(i).copy(payloadBits = globalNoCWidth),
      ingresses = ingressParams.flatten,
      egresses = egressParams.flatten,
      flows = flowParams.flatten,
      nocName = "global_tl",
      hasCtrl = true
    )
  })
  val noc = nocClockDomain { hasGlobalTLInterconnect.option(LazyModule(new NoC()(nocP))) }
  val global_noc_ctrl = nocClockDomain { hasGlobalTLInterconnect.option(
    (0 until nocParams.topology.nNodes).map { i => LazyModule(new TLNoCControl(0x3000000, i, "global-tl"))}
  ) }

  if (hasGlobalTLInterconnect) {
    require(nocParams.nVirtualNetworks >= 5 * supportedBuses.size)
    val tlbus = locateTLBusWrapper(NBUS)
    global_noc_ctrl.get.zipWithIndex.foreach { case (c,i) =>
      tlbus.toVariableWidthSlave(Some(s"noc-ctrl-$i")) { c.node } }
  }


  // TODO: for now use the implicit clock/reset
  nocClockDomain { InModuleBody {
    val clockBundle = nocClockDomain.clockBundle
    noc.map(_.module.clock := clockBundle.clock)
    noc.map(_.module.reset := clockBundle.reset)
    noc.map(_.module.io.router_clocks.foreach(_.clock := clockBundle.clock))
    noc.map(_.module.io.router_clocks.foreach(_.reset := clockBundle.reset))
    if (noc.isDefined)
      (noc.get.module.io.router_ctrl zip global_noc_ctrl.get).map { case (r,l) => l.module.io.ctrl <> r }
  } }



  def getSubnetTerminalChannels(bus: TLBusWrapperLocation): BundleBridgeSink[NoCTerminalIO] = {
    val source = nocClockDomain { BundleBridgeSource[NoCTerminalIO](() => {
      val ingressParams = noc.get.allIngressParams.drop(ingressOffset(bus)).take(nIngresses(bus))
      val egressParams  = noc.get.allEgressParams .drop( egressOffset(bus)).take( nEgresses(bus))

      new NoCTerminalIO(ingressParams, egressParams)(nocP)}
    )}
    val sink = BundleBridgeSink[NoCTerminalIO]()
    sink := source
    nocClockDomain { InModuleBody { /* connect source to noc */
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

