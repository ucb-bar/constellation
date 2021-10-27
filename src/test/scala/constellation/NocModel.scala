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

class NocModel(nNodes: Int) extends Module {
  require(nNodes > 1)
  def routingRel(nodeId: UInt)(srcId: UInt, nxtId: UInt, dstId: UInt): Bool = {
    Mux(nodeId < nxtId, dstId >= nxtId, dstId <= nxtId) && nxtId =/= srcId
  }
  def topologyRel(src: UInt, dest: UInt): Bool = (dest - src).abs === 1.U

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

  when(state === Start) {
    // we start at an arbitrary node and route to an arbitrary node
    currentNode := choices.startNode
    srcNode := choices.startNode
    dstNode := choices.dstNode
    state := InTransmission
  } .elsewhen(state === InTransmission) {
    // count the number of hops
    hopCount := hopCount + 1.U

    // we want to pick an adjacent node that is allowed by the routing table
    val nextNodeChoice = choices.nextNode
    assume(topologyRel(currentNode, nextNodeChoice), "next node needs to be adjacent in the topology")
    assume(routingRel(currentNode)(srcNode, nextNodeChoice, dstNode), "hop needs to be allowed by the routing table")

    // TODO: check that a valid choice actually exists!

    currentNode := nextNodeChoice

    // we will be done if the next hop gets us to our destination
    when(nextNodeChoice === dstNode) {
      state := Received
    }
  }
}

class NocModelProperties(makeModel: => NocModel) extends Module {
  val model = Module(makeModel)
  val choices = IO(Input(chiselTypeOf(model.choices)))
  choices <> model.choices


  // check that the packet will be routed after at max 4 hops
  assert(model.status.hopCount <= 4.U)
}

class NocModelTests extends AnyFlatSpec with ChiselScalatestTester with Formal {
  behavior of "NocModel"

  it should "route packet after at max 4 hops with a bidirectional line and 5 nodes" in {
    verify(new NocModelProperties(new NocModel(5)), Seq(BoundedCheck(10)))
  }

  it should "find a packet that cannot be routed in 4 hops with a bidirectional line and 6 nodes" in {
    assertThrows[FailedBoundedCheckException] {
      verify(new NocModelProperties(new NocModel(6)), Seq(BoundedCheck(10)))
    }
    // checkout the VCD in:
    // test_run_dir/NocModel_should_find_a_packet_that_cannot_be_routed_in_4_hops_with_a_bidirectional_line_and_6_nodes/NocModelProperties.vcd
  }

}