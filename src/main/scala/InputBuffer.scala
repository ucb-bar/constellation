package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}


class InputBuffer(inParam: ChannelParams)(implicit val p: Parameters) extends Module with HasAstroNoCParams {
  val maxBufferSize = inParam.virtualChannelParams.map(_.bufferSize).max
  val io = IO(new Bundle {
    val in = Flipped(Valid(new Flit))

    val head = Output(UInt(log2Ceil(maxBufferSize).W))

    val read_req = Input(Valid(new Bundle {
      val addr = UInt(log2Ceil(maxBufferSize).W)
      val channel = UInt(log2Ceil(inParam.virtualChannelParams.size).W)
    }))
    val read_resp = Output(new Flit)
  })
  val buffers = Reg(MixedVec(inParam.virtualChannelParams.map { u => Vec(u.bufferSize, new Flit) }))
  val heads = Reg(MixedVec(inParam.virtualChannelParams.map { u => UInt(log2Ceil(u.bufferSize).W) }))

  io.head := DontCare
  io.read_resp := DontCare
  for (i <- 0 until inParam.virtualChannelParams.size) {
    when (io.in.bits.virt_channel_id === i.U) {
      io.head := heads(i)
      when (io.in.fire()) {
        heads(i) := WrapInc(heads(i), inParam.virtualChannelParams(i).bufferSize)
        buffers(i)(heads(i)) := io.in.bits
      }
    }
    when (RegNext(io.read_req.bits.channel === i.U)) {
      io.read_resp := buffers(i)(RegNext(io.read_req.bits.addr))
    }
  }


  when (reset.asBool) { heads.foreach(_ := 0.U) }


}
