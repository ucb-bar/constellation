package constellation.protocol

import chisel3._
import chisel3.util._

import constellation.channel._
import constellation.noc._
import constellation.router.{RouterCtrlBundle}

import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._

import scala.collection.immutable.{ListMap}

case class DiplomaticNetworkNodeMapping(
  inNodeMapping: ListMap[String, Int] = ListMap[String, Int](),
  outNodeMapping: ListMap[String, Int] = ListMap[String, Int]()
) {
  def genUniqueName(all: Seq[Seq[String]]) = {
    all.zipWithIndex.map { case (strs, i) =>
      val matches = all.take(i).map(_.mkString).count(_ == strs.mkString)
      strs.map(s => s"${s}[${matches}]").mkString(",")
    }
  }
  def getNode(nodeMapping: ListMap[String, Int], l: String): Int = {
    val keys = nodeMapping.keys.toSeq
    val matches = keys.map(k => l.contains(k))
    require(matches.filter(i => i).size == 1, s"unable to find valid mapping for $l\n$nodeMapping")
    val index = matches.indexWhere(i => i)
    nodeMapping.values.toSeq(index)
  }
  def getNodeIn(l: String): Int = getNode(inNodeMapping, l)
  def getNodeOut(l: String): Int = getNode(outNodeMapping, l)

}


trait ProtocolParams {
  val nProtocolTerminals: Int
  val minPayloadWidth: Int
  val ingressNodes: Seq[Int]
  val egressNodes: Seq[Int]
  val nVirtualNetworks: Int
  val vNetBlocking: (Int, Int) => Boolean
  val flows: Seq[FlowParams]
  def genIO()(implicit p: Parameters): Data
  def interface(terminals: NoCTerminalIO, ingressOffset: Int, egressOffset: Int, protocol: Data)(implicit p: Parameters)
}

case class ProtocolNoCParams(
  nocParams: NoCParams,
  protocolParams: Seq[ProtocolParams]
)
class ProtocolNoC(params: ProtocolNoCParams)(implicit p: Parameters) extends Module {
  val protocolParams  = params.protocolParams
  val minPayloadWidth = protocolParams.map(_.minPayloadWidth).max
  val ingressOffsets  = protocolParams.map(_.ingressNodes.size).scanLeft(0)(_+_)
  val egressOffsets   = protocolParams.map(_.egressNodes.size).scanLeft(0)(_+_)
  val vNetOffsets     = protocolParams.map(_.nVirtualNetworks).scanLeft(0)(_+_)

  val nocParams = params.nocParams.copy(
    ingresses = protocolParams.map(_.ingressNodes).flatten.map(i =>
      UserIngressParams(i, payloadBits=minPayloadWidth)),
    egresses = protocolParams.map(_.egressNodes).flatten.map(i =>
      UserEgressParams(i, payloadBits=minPayloadWidth)),
    routerParams = (i) => params.nocParams.routerParams(i).copy(payloadBits=minPayloadWidth),
    vNetBlocking = (blocker, blockee) => {
      def protocolId(i: Int) = vNetOffsets.drop(1).indexWhere(_ > i)
      if (protocolId(blocker) == protocolId(blockee)) {
        protocolParams(protocolId(blocker)).vNetBlocking(
          blocker - vNetOffsets(protocolId(blocker)),
          blockee - vNetOffsets(protocolId(blockee))
        )
      } else {
        true
      }
    },
    flows = protocolParams.zipWithIndex.map { case (u,i) =>
      u.flows.map(f => f.copy(
        ingressId = f.ingressId + ingressOffsets(i),
        egressId = f.egressId + egressOffsets(i),
        vNetId = f.vNetId + vNetOffsets(i)
      ))
    }.flatten
  )
  val io = IO(new Bundle {
    val ctrl = if (nocParams.hasCtrl) Vec(nocParams.topology.nNodes, new RouterCtrlBundle) else Nil
    val protocol = MixedVec(protocolParams.map { u => u.genIO() })
  })
  val noc = Module(LazyModule(new NoC(nocParams)).module)
  noc.io.router_clocks.foreach(_.clock := clock)
  noc.io.router_clocks.foreach(_.reset := reset)
  (noc.io.router_ctrl zip io.ctrl).foreach { case (l, r) => l <> r }
  (protocolParams zip io.protocol).zipWithIndex.foreach { case ((u, io), x) =>
    val terminals = Wire(new NoCTerminalIO(
      noc.io.ingressParams.drop(ingressOffsets(x)).take(u.ingressNodes.size),
      noc.io.egressParams .drop(egressOffsets(x)) .take(u.egressNodes.size)
    ))
    (terminals.ingress zip noc.io.ingress.drop(ingressOffsets(x))).map { case (l,r) => l <> r }
    (terminals.egress  zip  noc.io.egress.drop (egressOffsets(x))).map { case (l,r) => l <> r }
    u.interface(
      terminals,
      ingressOffsets(x),
      egressOffsets(x),
      io)
  }
}
