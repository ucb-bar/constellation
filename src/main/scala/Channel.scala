package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}

case class VirtualChannelParams(
  bufferSize: Int
)

case class ChannelParams(
  srcId: Int,
  destId: Int,
  virtualChannelParams: Seq[VirtualChannelParams],
  inputId: Int = -1,
  outputId: Int = -1,
  useSyncReadBuffer: Boolean = false,
  depth: Int = 0
) {
  val nVirtualChannels = virtualChannelParams.size
  val isInput = inputId >= 0
  val isOutput = outputId >= 0
  require(!(srcId == -1 ^ isInput))
  require(!(destId == -1 ^ isOutput))
  require(!(isInput && isOutput))
}

class Channel(val cParams: ChannelParams)(implicit val p: Parameters) extends Bundle with HasAstroNoCParams {
  val flit = Valid(new Flit)
  val credit_return = Input(Valid(UInt(log2Up(cParams.virtualChannelParams.size).W)))
  val vc_free = Input(Valid(UInt(log2Up(cParams.virtualChannelParams.size).W)))
}

class IOChannel(val cParams: ChannelParams)(implicit val p: Parameters) extends Bundle with HasAstroNoCParams {
  val flit = Decoupled(new Flit)
}

object ChannelBuffer {
  def apply(in: Channel, cParams: ChannelParams)(implicit p: Parameters): Channel = {
    val buffer = Module(new ChannelBuffer(cParams))
    buffer.io.in <> in
    buffer.io.out
  }
}

class ChannelBuffer(val cParams: ChannelParams)(implicit val p: Parameters) extends Module with HasAstroNoCParams {
  val io = IO(new Bundle {
    val in = Flipped(new Channel(cParams))
    val out = new Channel(cParams)
  })

  io.out.flit := Pipe(io.in.flit, cParams.depth)
  io.in.credit_return := Pipe(io.out.credit_return, cParams.depth)
  io.in.vc_free := Pipe(io.out.vc_free, cParams.depth)

}
