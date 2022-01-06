package constellation

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}

object ChannelBuffer {
  def apply(depth: Int)(implicit p: Parameters) = {
    val buffer = LazyModule(new ChannelBuffer(depth))
    buffer.node
  }
}

class ChannelBuffer(depth: Int)(implicit p: Parameters) extends LazyModule {
  val node = new ChannelIdentityNode
  lazy val module = new LazyModuleImp(this) {
    val in = node.in(0)._1
    val out = node.out(0)._1
    val cParam = node.in(0)._2.cp

    if (cParam.traversable) {
      out.flit := Pipe(in.flit, depth)
      in.credit_return := Pipe(out.credit_return, depth)
      in.vc_free := Pipe(out.vc_free, depth)
    } else {
      out.flit.valid := false.B
      out.flit.bits := DontCare
      in.credit_return.valid := false.B
      in.credit_return.bits := DontCare
      in.vc_free.valid := false.B
      in.vc_free.bits := DontCare
    }
  }
}
