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
  val (buffer, read, write) = if (cParam.useSyncReadBuffer) {
    val mem = SyncReadMem(bufferSz, new Flit(cParam))
    def read(x: UInt, en: Bool): Flit = mem.read(x, en)
    def write(x: UInt, d: Flit): Unit = mem.write(x, d)
    (mem, read(_,_), write(_,_))
  } else {
    val mem = Reg(Vec(bufferSz, new Flit(cParam)))
    def read(x: UInt, en: Bool): Flit = RegEnable(mem(x), en)
    def write(x: UInt, d: Flit): Unit = mem(x) := d
    (mem, read(_,_), write(_,_))
  }

  val tails = Reg(Vec(bufferSz, Bool()))

  val heads = Reg(Vec(nVirtualChannels, UInt(log2Up(maxBufferSize).W)))
  val bases = VecInit(virtualChannelParams.map(_.bufferSize).scanLeft(0)(_+_).dropRight(1).map(_.U))

  val in_virt_id = io.in.bits.virt_channel_id
  val in_base = bases(in_virt_id)
  val in_head = heads(in_virt_id)

  when (io.in.valid) {
    val waddr = in_base +& in_head
    write(waddr, io.in.bits)
    tails(waddr) := io.in.bits.tail
    heads(in_virt_id) := WrapInc(heads(in_virt_id),
      VecInit(virtualChannelParams.map(_.bufferSize.U))(in_virt_id))
  }

  def bypass(x: Flit, addr: UInt, channel: UInt): Flit = {
    val out = WireInit(x)
    when (RegNext(io.in.valid && (addr === in_head) && (channel === in_virt_id))) {
      out := RegNext(io.in.bits)
    }
    out
  }
  def bypass_tail(x: Bool, addr: UInt, channel: UInt): Bool = {
    val out = WireInit(x)
    when (io.in.valid && (addr === in_head) && (channel === in_virt_id)) {
      out := io.in.bits.tail
    }
    out
  }


  io.head := heads(in_virt_id)

  val raddr = bases(io.read_req.bits.channel) +& io.read_req.bits.addr
  io.read_resp := bypass(read(raddr, io.read_req.valid), io.read_req.bits.addr, io.read_req.bits.channel)
  (io.tail_read_resp zip io.tail_read_req).zipWithIndex.map { case ((resp,req),i) =>
    resp := bypass_tail(tails(bases(i.U) +& req), io.read_req.bits.addr, io.read_req.bits.channel)
  }

  when (reset.asBool) { heads.foreach(_ := 0.U) }
}
