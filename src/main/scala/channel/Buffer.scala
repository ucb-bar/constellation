package constellation.channel

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
      (out.flit zip in.flit).map(t => t._1 := Pipe(t._2))
      in.credit_return := ShiftRegister(out.credit_return, depth)
      in.vc_free := ShiftRegister(out.vc_free, depth)
    } else {
      out.flit.foreach(_.valid := false.B)
      out.flit.foreach(_.bits := DontCare)
      in.credit_return := 0.U
      in.vc_free := 0.U
    }
  }
}
