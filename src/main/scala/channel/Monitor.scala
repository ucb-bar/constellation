package constellation.channel

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.util._

class NoCMonitor(val cParam: ChannelParams)(implicit val p: Parameters) extends Module with HasChannelParams {
  val io = IO(new Bundle {
    val in = Input(new Channel(cParam))
  })

  val in_flight = RegInit(0.U(nVirtualChannels.W))
  when (io.in.flit.valid) {
    when (io.in.flit.bits.head) {
      in_flight := in_flight | (1.U << io.in.flit.bits.virt_channel_id)
      assert (!in_flight(io.in.flit.bits.virt_channel_id), "Flit head/tail sequencing is broken")
    }
    when (io.in.flit.bits.tail) {
      in_flight := in_flight & ~(1.U << io.in.flit.bits.virt_channel_id)
    }
  }

  val possiblePackets = cParam.possiblePackets.map(p => Cat(p.egressId.U, p.vNet.U(vNetBits-1,0)))
  when (io.in.flit.valid && io.in.flit.bits.head) {
    assert (Cat(io.in.flit.bits.egress_id, io.in.flit.bits.vnet_id).isOneOf(possiblePackets.toSeq),
      "Illegal packet found")
  }

}
