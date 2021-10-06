package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}

class OutputUnitAlloc(val inParams: Seq[ChannelParams], val outParam: ChannelParams)(implicit val p: Parameters) extends Bundle with HasAstroNoCParams {
  val in_channel = UInt(log2Ceil(inParams.size).W)
  val in_virt_channel = UInt(log2Ceil(inParams.map(_.virtualChannelParams.size).max).W)
  val out_virt_channel = UInt(outParam.virtualChannelParams.size.W)
}


class OutputUnit(inParams: Seq[ChannelParams], outParam: ChannelParams)(implicit val p: Parameters) extends Module with HasAstroNoCParams {
  val nInputs = inParams.size
  val nVirtualChannels = outParam.virtualChannelParams.size
  val io = IO(new Bundle {
    val in = Flipped(Valid(new Flit))
    val alloc = Flipped(Valid(new OutputUnitAlloc(inParams, outParam)))

    val credit_alloc = Input(Valid(UInt(log2Ceil(nVirtualChannels).W)))
    val credit_free = Input(Valid(UInt(log2Ceil(nVirtualChannels).W)))
    val vc_free = Input(Valid(UInt(log2Ceil(nVirtualChannels).W)))

    val credit_available = Output(Vec(nVirtualChannels, Bool()))
    val out = Valid(new Flit)
  })

  val g_i :: g_a :: g_c :: Nil = Enum(3)

  // Technically the output unit doesn't do anything here with this state tracking
  // The only useful bit is tracking credits on each virtual channel
  class OutputState(val bufferSize: Int) extends Bundle {
    val g = UInt(2.W)
    val i_p = UInt(log2Ceil(nInputs).W)
    val i_c = UInt(log2Ceil(inParams.map(_.virtualChannelParams.size).max).W)
    val c = UInt(log2Ceil(bufferSize).W)
  }

  val states = Reg(MixedVec(outParam.virtualChannelParams.map { u => new OutputState(u.bufferSize) }))

  io.out := io.in

  when (io.alloc.fire()) {
    val id = io.alloc.bits.out_virt_channel
    states.zipWithIndex.map { case (s,i) =>
      when (id === i.U) {
        assert(s.g === g_i)

        s.g := g_a
        s.i_p := io.alloc.bits.in_channel
        s.i_c := io.alloc.bits.in_virt_channel
      }
    }
  }

  (io.credit_available zip states).map { case (c,s) =>
    c := s.c =/= 0.U
  }

  states.zipWithIndex.map { case (s,i) =>
    val free = (io.credit_free.valid && io.credit_free.bits === i.U)
    val alloc = (io.credit_alloc.valid && io.credit_alloc.bits === i.U)
    s.c := s.c +& free - alloc
  }


  when (io.vc_free.fire()) {
    states.zipWithIndex.map { case (s,i) =>
      when (io.vc_free.bits === i.U) {
        assert(s.g =/= g_i)
        s.g := g_i
      }
    }
  }

  when (reset.asBool) {
    states.foreach(_.g := g_i)
    states.foreach(s => s.c := s.bufferSize.U)
  }
}
