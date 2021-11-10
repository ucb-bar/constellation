package constellation

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}

case class VirtualChannelParams(
  bufferSize: Int = 1,
  possiblePackets: Set[(Int, Int)] = Set(),
) {
  val traversable = possiblePackets.size > 0
}

trait BaseChannelParams {
  def srcId: Int
  def destId: Int
  def virtualChannelParams: Seq[VirtualChannelParams]

  def nVirtualChannels = virtualChannelParams.size
  def traversable = virtualChannelParams.map(_.traversable).reduce(_||_)
  def possiblePackets = virtualChannelParams.map(_.possiblePackets).reduce(_++_)
}

case class ChannelParams(
  srcId: Int,
  destId: Int,
  virtualChannelParams: Seq[VirtualChannelParams] = Seq(VirtualChannelParams()),
  depth: Int = 0
) extends BaseChannelParams

case class IngressChannelParams(
  destId: Int,
  virtualChannelParams: Seq[VirtualChannelParams],
  ingressId: Int,
  vNetId: Int
) extends BaseChannelParams {
  def srcId = -1
  require(virtualChannelParams.size == 1)
}

case class EgressChannelParams(
  srcId: Int,
  virtualChannelParams: Seq[VirtualChannelParams],
  egressId: Int
) extends BaseChannelParams {
  def destId = -1
  require(virtualChannelParams.size == 1)
}



trait HasChannelParams extends HasNoCParams {
  val cParam: BaseChannelParams

  val virtualChannelParams = cParam.virtualChannelParams
  val nVirtualChannels = cParam.nVirtualChannels
  val virtualChannelBits = log2Up(nVirtualChannels)

  val maxBufferSize = virtualChannelParams.map(_.bufferSize).max
}

class Channel(val cParam: ChannelParams)(implicit val p: Parameters) extends Bundle with HasChannelParams {
  val flit = Valid(new Flit(cParam))
  val credit_return = Input(Valid(UInt(virtualChannelBits.W)))
  val vc_free = Input(Valid(UInt(virtualChannelBits.W)))
}

class TerminalChannel(val cParam: BaseChannelParams)(implicit val p: Parameters) extends Bundle with HasChannelParams {
  require(cParam match {
    case IngressChannelParams(_,_,_,_) => true
    case EgressChannelParams(_,_,_) => true
    case _ => false
  })

  val flit = Decoupled(new IOFlit(cParam))
}

object ChannelBuffer {
  def apply(in: Channel, cParam: ChannelParams)(implicit p: Parameters): Channel = {
    val buffer = Module(new ChannelBuffer(cParam))
    buffer.io.in <> in
    buffer.io.out
  }
}

class ChannelBuffer(val cParam: ChannelParams)(implicit val p: Parameters) extends Module with HasChannelParams {
  val io = IO(new Bundle {
    val in = Flipped(new Channel(cParam))
    val out = new Channel(cParam)
  })
  if (cParam.traversable) {
    io.out.flit := Pipe(io.in.flit, cParam.depth)
    io.in.credit_return := Pipe(io.out.credit_return, cParam.depth)
    io.in.vc_free := Pipe(io.out.vc_free, cParam.depth)
  } else {
    io.out.flit.valid := false.B
    io.out.flit.bits := DontCare
    io.in.credit_return.valid := false.B
    io.in.credit_return.bits := DontCare
    io.in.vc_free.valid := false.B
    io.in.vc_free.bits := DontCare
  }

}
