package constellation.rc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util._

import constellation.router.{HasRouterCtrlConsts, RouterCtrlBundle}

class TLNoCControl(baseAddress: BigInt, routerId: Int, nocName: String,
  routerRegionSize: Int = 4096)(implicit p: Parameters) extends LazyModule with HasRouterCtrlConsts {

  val device = new SimpleDevice(s"$nocName-router$routerId-ctrl", Nil)
  val address = baseAddress + routerId * routerRegionSize
  val node = TLRegisterNode(
    Seq(AddressSet(address, routerRegionSize-1)),
    device,
    "reg/control",
    beatBytes=4
  )

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val ctrl = Flipped(new RouterCtrlBundle)
    })
    val (bundleIn, edge) = node.in(0)
    val a = bundleIn.a
    val d = bundleIn.d

    a.ready := d.ready
    io.ctrl.enable := a.valid
    io.ctrl.write  := a.bits.opcode =/= TLMessages.Get
    io.ctrl.addr   := a.bits.address >> log2Ceil(CTRL_WORD_SZ)
    io.ctrl.wdata  := a.bits.data

    d.valid := a.valid
    d.bits := edge.AccessAck(toSource=a.bits.source, lgSize=a.bits.size)
    d.bits.opcode := Mux(io.ctrl.write, TLMessages.AccessAck, TLMessages.AccessAckData)
    d.bits.data := io.ctrl.rdata

    bundleIn.b.valid := false.B
    bundleIn.c.ready := true.B
    bundleIn.e.ready := true.B
  }
}
