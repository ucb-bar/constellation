package constellation

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import chiseltest._
import chiseltest.formal._
import org.scalatest.flatspec.AnyFlatSpec


object PacketState extends ChiselEnum {
  val Start, InTransmission, Received = Value
}

class NocModelChoices(nNodes: Int) extends Bundle {
  require(nNodes > 1)
  private val w = log2Ceil(nNodes)
  val startNode = UInt(w.W)
  val dstNode= UInt(w.W)
  val nextNode = UInt(w.W)
}

class NocModelStatus extends Bundle {
  val hopCount = UInt(32.W)
  val packetReceived = Bool()
}

// TODO: if we make the default relations in the topology package chisel functions, then we do not
//       need to duplicate them here
object Relations {
  private def adjacent(a: UInt, b: UInt): Bool =
    (a > b && a - b === 1.U) || (a > b && a - b === 1.U)
  type Routing = (UInt, UInt) => (UInt) => (UInt, UInt, UInt) => Bool
  type Topology = (UInt, UInt) => (UInt, UInt) => Bool
  def routingBidirectionalLine(nX: UInt, nY: UInt)(nodeId: UInt)(srcId: UInt, nxtId: UInt, dstId: UInt): Bool = {
    Mux(nodeId < nxtId, dstId >= nxtId, dstId <= nxtId) && nxtId =/= srcId
  }
  def topologyBidirectionalLine(nX: UInt, nY: UInt)(src: UInt, dst: UInt): Bool =
    (src > dst && src - dst === 1.U) || (dst > src && dst - src === 1.U)
  def routingMesh2DMinimal(nX: UInt, nY: UInt)(nodeId: UInt)(srcId: UInt, nxtId: UInt, dstId: UInt): Bool = {
    val (nxtX, nxtY)   = (nxtId % nX , nxtId / nX)
    val (nodeX, nodeY) = (nodeId % nX, nodeId / nX)
    val (dstX, dstY)   = (dstId % nX , dstId / nX)
    val (srcX, srcY)   = (srcId % nX , srcId / nX)

    val xR: Bool = Mux(nodeX < nxtX, dstX >= nxtX, Mux(nodeX > nxtX, dstX <= nxtX, nodeX === nxtX))
    val yR: Bool = Mux(nodeY < nxtY, dstY >= nxtY, Mux(nodeY > nxtY, dstY <= nxtY, nodeY === nxtY))
    xR && yR
  }
  def topologyMesh2D(nX: UInt, nY: UInt)(src: UInt, dst: UInt): Bool = {
    val (srcX, srcY) = (src % nX, src / nX)
    val (dstX, dstY) = (dst % nX, dst / nX)
    (srcX === dstX && adjacent(srcY, dstY)) || (srcY === dstY && adjacent(srcX, dstX))
  }
}

case class NocConfig(nX: Int, nY: Int, topology: Relations.Topology, routing: Relations.Routing) {
  def nNodes: Int = nX * nY
}


class NocModel(conf: NocConfig) extends Module {
  val nNodes = conf.nNodes
  require(nNodes > 1)

  // I/O
  val choices = IO(Input(new NocModelChoices(nNodes)))
  val status = IO(Output(new NocModelStatus))

  // all choices need to be valid node ids
  def isValidNodeId(id: UInt): Bool = id < nNodes.U
  assume(isValidNodeId(choices.startNode))
  assume(isValidNodeId(choices.dstNode))
  assume(isValidNodeId(choices.nextNode))

  // state of the packet routing
  import PacketState._
  val NodeId = UInt(log2Ceil(nNodes).W)
  val state = RegInit(PacketState.Start)
  val currentNode = Reg(NodeId)
  val dstNode = Reg(NodeId)
  val srcNode = Reg(NodeId)


  val hopCount = RegInit(0.U(32.W))
  status.hopCount := hopCount
  status.packetReceived := state === Received

  def nodeExists(rel: UInt => Bool): Bool = {
    Cat((0 until nNodes).map(ii => rel(ii.U))) =/= 0.U
  }

  when(state === Start) {
    // we start at an arbitrary node and route to an arbitrary node
    currentNode := choices.startNode
    srcNode := choices.startNode
    dstNode := choices.dstNode
    val triviallyReceived = choices.startNode === choices.dstNode
    state := Mux(triviallyReceived, Received, InTransmission)
  } .elsewhen(state === InTransmission) {
    // count the number of hops
    hopCount := hopCount + 1.U

    // check that a next node exists
    val adjacentNodeExists = nodeExists(ii => ii =/= currentNode && conf.topology(conf.nX.U, conf.nY.U)(currentNode, ii))
    assert(adjacentNodeExists,
      "We are stuck trying to route from %d to %d! there is no neighboring node to %d in the topology!", srcNode, dstNode, currentNode
    )

    // check that a routing path exists
    val routeAvailable = nodeExists(ii =>
      ii =/= currentNode && conf.topology(conf.nX.U, conf.nY.U)(currentNode, ii) &&
      conf.routing(conf.nX.U, conf.nY.U)(currentNode)(srcNode, ii, dstNode)
    )
    assert(routeAvailable, "We are stuck! there is no route from %d to %d through %d", srcNode, dstNode, currentNode)

    // we want to pick an adjacent node that is allowed by the routing table
    val nextNodeChoice = choices.nextNode
    when(routeAvailable) {
      assume(conf.topology(conf.nX.U, conf.nY.U)(currentNode, nextNodeChoice),
        "next node needs to be adjacent in the topology")
      assume(conf.routing(conf.nX.U, conf.nY.U)(currentNode)(srcNode, nextNodeChoice, dstNode),
        "hop needs to be allowed by the routing table")
    }
    currentNode := nextNodeChoice

    // we will be done if the next hop gets us to our destination
    when(nextNodeChoice === dstNode) {
      state := Received
    }
  }
}

class NocModelProperties(makeModel: => NocModel, maxHops: Int) extends Module {
  val model = Module(makeModel)
  val choices = IO(Input(chiselTypeOf(model.choices)))
  choices <> model.choices


  // check that the packet will be routed after at max N hops
  require(maxHops > 0)
  assert(model.status.hopCount <= maxHops.U)
}

class NocModelTests extends AnyFlatSpec with ChiselScalatestTester with Formal {
  behavior of "NocModel"

  it should "route packet after at max 4 hops with a bidirectional line and 5 nodes" in {
    val conf = NocConfig(5, 1, Relations.topologyBidirectionalLine, Relations.routingBidirectionalLine)
    verify(new NocModelProperties(new NocModel(conf), maxHops = 4), Seq(BoundedCheck(10), BtormcEngineAnnotation))
  }

  it should "find a packet that cannot be routed in 4 hops with a bidirectional line and 6 nodes" in {
    assertThrows[FailedBoundedCheckException] {
      val conf = NocConfig(6, 1, Relations.topologyBidirectionalLine, Relations.routingBidirectionalLine)
      verify(new NocModelProperties(new NocModel(conf), maxHops = 4), Seq(BoundedCheck(10), BtormcEngineAnnotation))
    }
    // checkout the VCD in:
    // test_run_dir/NocModel_should_find_a_packet_that_cannot_be_routed_in_4_hops_with_a_bidirectional_line_and_6_nodes/NocModelProperties.vcd
  }

  // TODO: fix problem in firrtl SMT backend
  it should "route packet after at max 10 hops with in a 4 x 4 network" in {
    val conf = NocConfig(4, 4, Relations.topologyMesh2D, Relations.routingMesh2DMinimal)
    verify(new NocModelProperties(new NocModel(conf), maxHops = 10), Seq(BoundedCheck(12), BtormcEngineAnnotation))
  }


}
