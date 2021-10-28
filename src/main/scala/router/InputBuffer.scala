package constellation.router

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}

import constellation._

class InputBuffer(val cParam: ChannelParams)(implicit val p: Parameters) extends Module with HasChannelParams {
  val io = IO(new Bundle {
    val in = Flipped(Valid(new Flit(cParam)))

    val head = Output(UInt(log2Up(maxBufferSize).W))

    val read_req = Input(Valid(new Bundle {
      val addr = UInt(log2Up(maxBufferSize).W)
      val channel = UInt(virtualChannelBits.W)
    }))
    val read_resp = Output(new Flit(cParam))
    val tail_read_req = MixedVec(virtualChannelParams.map(u => Input(UInt(log2Ceil(u.bufferSize).W))))
    val tail_read_resp = Vec(nVirtualChannels, Output(Bool()))
  })

  val bufferSz = virtualChannelParams.map(_.bufferSize).sum
  val buffer = virtualChannelParams.map { u => Reg(Vec(u.bufferSize, new Flit(cParam))) }
  val heads = Reg(Vec(nVirtualChannels, UInt(log2Up(maxBufferSize).W)))

  val in_virt_id = io.in.bits.virt_channel_id
  val in_head = heads(in_virt_id)

  when (io.in.valid) {
    (0 until nVirtualChannels).map { v =>
      when (v.U === in_virt_id) {
        buffer(v)(in_head) := io.in.bits
        heads(v) := WrapInc(heads(v), virtualChannelParams(v).bufferSize)
      }
    }
  }

  io.head := heads(in_virt_id)
  io.read_resp := Mux1H(UIntToOH(io.read_req.bits.channel),
    buffer.map(_(io.read_req.bits.addr)))
  (io.tail_read_resp zip io.tail_read_req).zipWithIndex.map { case ((resp,req),i) =>
    resp := buffer(i)(req).tail
    when (io.in.valid && (in_head === req) && (in_virt_id === i.U)) {
      resp := io.in.bits.tail
    }
  }

  when (reset.asBool) { heads.foreach(_ := 0.U) }
}
