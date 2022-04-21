package constellation.channel

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config.{Parameters}

object WidthWidget {
  def split[T <: BaseFlit](in: IrrevocableIO[T], out: IrrevocableIO[T]) = {
    val inBits = in.bits.payload.getWidth
    val outBits = out.bits.payload.getWidth
    require(inBits > outBits && inBits % outBits == 0)
    val ratio = inBits / outBits
    val count = RegInit(0.U(log2Ceil(ratio).W))
    val first = count === 0.U
    val last = count === (ratio - 1).U
    val stored = Reg(UInt((inBits - outBits).W))

    out.valid := in.valid
    out.bits := in.bits
    out.bits.head := in.bits.head && first
    out.bits.tail := in.bits.tail && last
    out.bits.payload := Mux(first, in.bits.payload, stored)
    in.ready := last && out.ready
    when (out.fire()) {
      count := Mux(last, 0.U, count + 1.U)
      stored := Mux(first, in.bits.payload, stored) >> outBits
    }
  }

  def merge[T <: BaseFlit](in: IrrevocableIO[T], out: IrrevocableIO[T]) = {
    val inBits = in.bits.payload.getWidth
    val outBits = out.bits.payload.getWidth
    require(outBits > inBits && outBits % inBits == 0)
    val ratio = outBits / inBits
    val count = RegInit(0.U(log2Ceil(ratio).W))
    val first = count === 0.U
    val last = count === (ratio - 1).U
    val flit = Reg(out.bits.cloneType)
    val payload = Reg(Vec(ratio-1, UInt(inBits.W)))

    out.valid := in.valid && last
    out.bits := flit
    out.bits.tail := last && in.bits.tail
    out.bits.payload := Cat(in.bits.payload, payload.asUInt)
    in.ready := !last || out.ready
    when (in.fire()) {
      count := Mux(last, 0.U, count + 1.U)
      payload(count) := in.bits.payload
      when (first) {
        flit := in.bits
      }
    }
  }

  def apply[T <: BaseFlit](in: IrrevocableIO[T], out: IrrevocableIO[T]) = {
    val inBits = in.bits.payload.getWidth
    val outBits = out.bits.payload.getWidth

    if (inBits == outBits) {
      out <> in
    } else if (inBits < outBits) {
      merge(in, out)
    } else {
      split(in, out)
    }
  }
}

class IngressWidthWidget(srcBits: Int)(implicit p: Parameters) extends LazyModule {
  val node = new IngressChannelAdapterNode(
    slaveFn = { s => s.copy(payloadBits=srcBits) }
  )
  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      WidthWidget(in.flit, out.flit)
    }
  }
}

object IngressWidthWidget {
  def apply(destBits: Int, srcBits: Int)(implicit p: Parameters) = {
    if (destBits == srcBits) {
      val node = IngressChannelEphemeralNode()
      node
    } else {
      val ingress_width_widget = LazyModule(new IngressWidthWidget(srcBits))
      ingress_width_widget.node
    }
  }
}

class EgressWidthWidget(srcBits: Int)(implicit p: Parameters) extends LazyModule {
  val node = new EgressChannelAdapterNode(
    slaveFn = { s => s.copy(payloadBits=srcBits) }
  )
  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      WidthWidget(in.flit, out.flit)
    }
  }
}

object EgressWidthWidget {
  def apply(destBits: Int, srcBits: Int)(implicit p: Parameters) = {
    if (destBits == srcBits) {
      val node = EgressChannelEphemeralNode()
      node
    } else {
      val egress_width_widget = LazyModule(new EgressWidthWidget(srcBits))
      egress_width_widget.node
    }
  }
}

// class ChannelWidthWidget(destBits: Int, srcBits: Int)(implicit p: Parameters) extends LazyModule {
//   val node = new ChannelAdapterNode(
//     masterFn = { s =>
//       require(s.payloadBits == srcBits)
//       if (srcBits > destBits) {
//         val ratio = srcBits / destBits
//         s.copy(
//           payloadBits=destBits,
//           virtualChannelParams=s.virtualChannelParams.map(v => v.copy(bufferSize=v.bufferSize * ratio))
//         )
//       } else {
//         s.copy(payloadBits=destBits)
//       }
//     },
//     slaveFn = { s =>
//       require(s.payloadBits == destBits)
//       if (srcBits > destBits) {
//         s.copy(payloadBits=srcBits)
//       } else {
//         val ratio = srcBits / destBits
//         s.copy(
//           payloadBits=srcBits,
//           virtualChannelParams=s.virtualChannelParams.map(v => v.copy(bufferSize=v.bufferSize * ratio))
//         )
//       }
//     }
//   )
//   lazy val module = new LazyModuleImp(this) {
//     (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
//       in.vc_free := out.vc_free
//       if (srcBits == destBits) {
//         in.credit_return := out.credit_return
//         out.flit <> in.flit
//       } else if (srcBits > destBits) {
//         require(srcBits % destBits == 0)

//         (in.flit zip out.flit) foreach { case (iF, oF) =>
//           val in_q = Module(new Queue(new Flit(in.cParam),
//             in.cParam.virtualChannelParams.map(_.bufferSize).sum, pipe=true, flow=true))
//           in_q.io.enq.valid := iF.valid
//           in_q.io.enq.bits := iF.bits
//           assert(!(in_q.io.enq.valid && !in_q.io.enq.ready))

//           val in_flit = Wire(Irrevocable(new Flit(in.cParam)))
//           in_flit.valid := in_q.io.deq.valid
//           in_flit.bits := in_q.io.deq.bits
//           in_q.io.deq.ready := in_flit.ready

//           val out_flit = Wire(Irrevocable(new Flit(out.cParam)))
//           oF.valid := out_flit.valid
//           oF.bits := out_flit.bits
//           out_flit.ready := true.B
//           WidthWidget(in_flit, out_flit)
//         }

//         val ratio = srcBits / destBits
//         val counts = RegInit(VecInit(Seq.fill(in.nVirtualChannels) { 0.U(log2Ceil(ratio).W) }))
//         val in_credit_return = Wire(Vec(in.nVirtualChannels, Bool()))
//         in.credit_return := in_credit_return.asUInt
//         for (i <- 0 until in.nVirtualChannels) {
//           in_credit_return(i) := false.B
//           when (out.credit_return(i)) {
//             val last = counts(i) === (ratio-1).U
//             counts(i) := Mux(last, 0.U, counts(i) + 1.U)
//             when (last) {
//               in_credit_return(i) := true.B
//             }
//           }
//         }
//       } else {
//         require(destBits % srcBits == 0)

//         (in.flit zip out.flit) foreach { case (iF, oF) =>
//           val in_flit = Wire(Irrevocable(new Flit(in.cParam)))
//           in_flit.valid := iF.valid
//           in_flit.bits := iF.bits
//           assert(in_flit.ready)

//           val out_flit = Wire(Irrevocable(new Flit(out.cParam)))
//           oF.valid := out_flit.valid
//           oF.bits := out_flit.bits
//           out_flit.ready := true.B
//           WidthWidget(in_flit, out_flit)
//         }


//         val ratio = destBits / srcBits
//         val credits = RegInit(VecInit((0 until in.nVirtualChannels).map { i =>
//           0.U(log2Ceil(ratio*in.cParam.virtualChannelParams(i).bufferSize).W)
//         }))
//         val in_credit_return = Wire(Vec(in.nVirtualChannels, Bool()))
//         in.credit_return := in_credit_return.asUInt
//         for (i <- 0 until in.nVirtualChannels) {
//           when (out.credit_return(i)) {
//             in_credit_return(i) := true.B
//             credits(i) := credits(i) + (ratio - 1).U
//           } .otherwise {
//             val empty = credits(i) === 0.U
//             in_credit_return(i) := !empty
//             credits(i) := Mux(empty, 0.U, credits(i) - 1.U)
//           }
//         }
//       }
//     }
//   }
// }

// object ChannelWidthWidget {
//   def apply(destBits: Int, srcBits: Int)(implicit p: Parameters) = {
//     if (destBits == srcBits) {
//       val node = ChannelEphemeralNode()
//       node
//     } else {
//       val channel_width_widget = LazyModule(new ChannelWidthWidget(destBits, srcBits))
//       channel_width_widget.node
//     }
//   }
// }
