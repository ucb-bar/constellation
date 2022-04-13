package constellation.channel

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.util._

import constellation.noc.{HasNoCParams}

class NoCMonitor(val cParam: ChannelParams)(implicit val p: Parameters) extends Module with HasChannelParams with HasNoCParams {
  val io = IO(new Bundle {
    val in = Input(new Channel(cParam))
  })

  val in_flight = RegInit(VecInit(Seq.fill(nVirtualChannels) { false.B }))
  for (i <- 0 until cParam.srcMultiplier) {
    val flit = io.in.flit(i)
    when (flit.valid) {
      when (flit.bits.head) {
        in_flight(flit.bits.virt_channel_id) := true.B
        assert (!in_flight(flit.bits.virt_channel_id), "Flit head/tail sequencing is broken")
      }
      when (flit.bits.tail) {
        in_flight(flit.bits.virt_channel_id) := false.B
      }
    }
    val possiblePackets = cParam.possiblePackets.map(p => Cat(p.egressId.U, p.vNet.U(vNetBits-1,0)))
    when (flit.valid && flit.bits.head) {
      assert (Cat(flit.bits.egress_id, flit.bits.vnet_id).isOneOf(possiblePackets.toSeq),
        "Illegal packet found")
    }
  }
}
