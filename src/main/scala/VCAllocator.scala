package astronoc

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

class VCAllocReq(val cParam: ChannelParams, val nOutputs: Int, val nTerminalOutputs: Int)(implicit val p: Parameters) extends Bundle with HasChannelParams {
  val in_virt_channel = UInt(virtualChannelBits.W)
  val in_prio = UInt(prioBits.W)

  val out_channels = UInt((nOutputs+nTerminalOutputs).W)
  val dummy = UInt(1.W) //avoids firrtl bug
}

class VCAllocResp(val cParam: ChannelParams, val outParams: Seq[ChannelParams], val terminalOutParams: Seq[ChannelParams])(implicit val p: Parameters) extends Bundle with HasChannelParams with HasRouterOutputParams {
  val in_virt_channel = UInt(virtualChannelBits.W)
  val out_virt_channel = UInt(log2Up((Seq(1) ++ allOutParams.map(_.virtualChannelParams.size)).max).W)
  val out_channel = UInt(log2Up(nAllOutputs).W)
}

class VCAllocator(val rParams: RouterParams)(implicit val p: Parameters) extends Module with HasRouterParams {
  val io = IO(new Bundle {
    val req = MixedVec(allInParams.map { u => Flipped(Decoupled(new VCAllocReq(u, nOutputs, nTerminalOutputs))) })
    val resp = MixedVec(allInParams.map { u => Valid(new VCAllocResp(u, outParams, terminalOutParams)) })

    val channel_available = MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Input(Bool())) })
    val out_alloc = MixedVec(allOutParams.map { u => Valid(new OutputUnitAlloc(inParams, terminalInParams, u)) })
  })
  val nOutChannels = allOutParams.map(_.nVirtualChannels).sum

  class InternalAlloc extends Bundle {
    val dummy = Bool() // avoids firrtl bug
    val in_channel = UInt(log2Up(nAllInputs).W)
    val in_virt_channel = UInt(log2Up(allInParams.map(_.nVirtualChannels).max).W)
    val out_channel = UInt(log2Up(nAllOutputs).W)
    val out_virt_channel = UInt(log2Up(allOutParams.map(_.nVirtualChannels).max).W)
  }
  val req_matrix = Seq.fill(nOutChannels) { Seq.fill(nAllInputs) { Wire(Decoupled(new InternalAlloc)) } }

  io.resp.foreach(_.bits := DontCare)
  (io.resp zip io.req).map { case (o,i) => o.bits.in_virt_channel := i.bits.in_virt_channel }
  io.req.foreach { r => when (r.valid) { assert(PopCount(r.bits.out_channels) >= 1.U) } }

  var idx = 0
  for (j <- 0 until allOutParams.map(_.nVirtualChannels).max) {
    for (i <- 0 until nAllOutputs) {
      if (j < allOutParams(i).nVirtualChannels) {
        for (k <- 0 until nAllInputs) {
          val r = io.req(k)
          val legalPath = if (i < nOutputs) {
            val legalPathsMatrix = VecInit((0 until allInParams(k).nVirtualChannels).map { m =>
              VecInit((0 until nPrios).map { o =>
                rParams.vcAllocLegalPaths(allInParams(k).srcId, m, outParams(i).destId, j)(o).B
              })
            })
            legalPathsMatrix(r.bits.in_virt_channel)(r.bits.in_prio)
          } else {
            r.bits.in_prio === j.U
          }

          req_matrix(idx)(k).valid := (r.valid &&
            r.bits.out_channels(i) &&
            io.channel_available(i)(j) &&
            legalPath
          )
          req_matrix(idx)(k).bits.in_channel := k.U
          req_matrix(idx)(k).bits.in_virt_channel := r.bits.in_virt_channel
          req_matrix(idx)(k).bits.out_channel := i.U
          req_matrix(idx)(k).bits.out_virt_channel := j.U
          req_matrix(idx)(k).bits.dummy := r.bits.in_virt_channel
          when (req_matrix(idx)(k).fire()) {
            io.resp(k).bits.out_channel := i.U
            io.resp(k).bits.out_virt_channel := j.U
          }
        }
        idx += 1
      }
    }
  }

  val row_reqs = Seq.tabulate(nOutChannels) { i =>
    val rr_arb = Module(new RRArbiter(new InternalAlloc, nAllInputs))
    (rr_arb.io.in zip req_matrix(i)).map { case (l,r) => l <> r }
    rr_arb.io.out
  }

  idx = 0
  val out_arbs = allOutParams.map(u => Module(new Arbiter(new InternalAlloc, u.nVirtualChannels)))
  for (j <- 0 until allOutParams.map(_.nVirtualChannels).max) {
    for (i <- 0 until allOutParams.size) {
      if (j < allOutParams(i).nVirtualChannels) {
        // Reverse the arbiter to prefer higher channels first
        out_arbs(i).io.in(out_arbs(i).n-j-1) <> row_reqs(idx)
        idx += 1
      }
    }
  }
  require(idx == nOutChannels)

  out_arbs.foreach(_.io.out.ready := true.B)
  (out_arbs zip io.out_alloc).map { case (o,a) =>
    a.valid := o.io.out.fire()
    a.bits.in_channel := o.io.out.bits.in_channel
    a.bits.in_virt_channel := o.io.out.bits.in_virt_channel
    a.bits.out_virt_channel := o.io.out.bits.out_virt_channel
  }



  for (k <- 0 until allInParams.size) {
    val fires = req_matrix.map(_(k).fire())
    assert(PopCount(fires) <= 1.U)
    io.req(k).ready := fires.reduce(_||_)
    io.resp(k).valid := fires.reduce(_||_)
  }
}
