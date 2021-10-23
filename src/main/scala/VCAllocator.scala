package constellation

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._
import freechips.rocketchip.rocket.{DecodeLogic}

class VCAllocReq(val cParam: ChannelParams, val nOutputs: Int, val nTerminalOutputs: Int)(implicit val p: Parameters) extends Bundle with HasChannelParams {
  val in_virt_channel = UInt(virtualChannelBits.W)
  val in_user = UInt(userBits.W)
  val dest_id = UInt(nodeIdBits.W)
  val out_channels = UInt((nOutputs+nTerminalOutputs).W)
}

class VCAllocResp(val cParam: ChannelParams, val outParams: Seq[ChannelParams], val terminalOutParams: Seq[ChannelParams])(implicit val p: Parameters) extends Bundle with HasChannelParams with HasRouterOutputParams {
  val in_virt_channel = UInt(virtualChannelBits.W)
  val out_virt_channel = UInt(log2Up((Seq(1) ++ allOutParams.map(_.virtualChannelParams.size)).max).W)
  val out_channel = UInt(log2Up(nAllOutputs).W)
}

class Allocator(d0: Int, d1: Int, revD0: Boolean = false, revD1: Boolean = false)
    extends Module {

  class ValidReady extends Bundle {
    val valid = Input(Bool())
    val ready = Output(Bool())
    def fire() = valid && ready
  }

  val io = IO(new Bundle {
    val in = Vec(d0, Vec(d1, new ValidReady))
  })

  val in = Wire(Vec(d0, Vec(d1, new ValidReady)))
  (io.in zip (if (revD0) in.reverse else in)).map { case (io_row, in_row) =>
    (io_row zip (if (revD1) in_row.reverse else in_row)).map { case (io_e, in_e) =>
      io_e <> in_e
    }
  }

  val rank_1_arbs = Seq.fill(d0) { Module(new Arbiter(Bool(), d1)) }
  val rank_2_arbs = Seq.fill(d1) { Module(new Arbiter(Bool(), d0)) }

  Seq.tabulate(d0, d1) { case (y, x) =>
    rank_1_arbs(y).io.in(x).valid := in(y)(x).valid
    rank_1_arbs(y).io.in(x).bits := DontCare
    in(y)(x).ready := rank_1_arbs(y).io.in(x).ready

    rank_2_arbs(x).io.in(y).valid := (rank_1_arbs(y).io.out.valid &&
      rank_1_arbs(y).io.chosen === x.U)
    rank_2_arbs(x).io.in(y).bits := DontCare
  }
  rank_1_arbs.zipWithIndex.map { case (a,y) =>
    a.io.out.ready := rank_2_arbs.map(_.io.in(y).fire()).reduce(_||_)
  }
  rank_2_arbs.foreach(_.io.out.ready := true.B)

  assert((0 until d0).map { y => PopCount(
    (0 until d1).map { x => io.in(y)(x).fire() }) <= 1.U }.reduce(_&&_))
  assert((0 until d1).map { x => PopCount(
    (0 until d0).map { y => io.in(y)(x).fire() }) <= 1.U }.reduce(_&&_))
}

class VCAllocator(val rParams: RouterParams)(implicit val p: Parameters) extends Module
    with HasRouterParams {
  val io = IO(new Bundle {
    val req = MixedVec(allInParams.map { u =>
      Flipped(Decoupled(new VCAllocReq(u, nOutputs, nTerminalOutputs))) })
    val resp = MixedVec(allInParams.map { u =>
      Valid(new VCAllocResp(u, outParams, terminalOutParams)) })

    val channel_available = MixedVec(allOutParams.map { u =>
      Vec(u.nVirtualChannels, Input(Bool())) })
    val out_allocs = MixedVec(allOutParams.map { u =>
      Vec(u.nVirtualChannels, Output(Bool())) })
  })
  val nOutChannels = allOutParams.map(_.nVirtualChannels).sum

  io.resp.foreach(_.bits := DontCare)
  (io.resp zip io.req).map { case (o,i) => o.bits.in_virt_channel := i.bits.in_virt_channel }
  io.req.foreach { r => when (r.valid) { assert(PopCount(r.bits.out_channels) >= 1.U) } }

  val allocator = Module(new Allocator(nOutChannels, nAllInputs, revD0=true))
  allocator.io.in.foreach(_.foreach(_.valid := false.B))

  // if (nodeId == 0) {
  //   for (k <- 0 until nAllInputs) {
  //     for (m <- 0 until allInParams(k).nVirtualChannels) {
  //       for (i <- 0 until nOutputs) {
  //         for (j <- 0 until allOutParams(i).nVirtualChannels) {
  //           println((allInParams(k).srcId, m, outParams(i).destId, j,
  //             rParams.vcAllocLegalPaths(allInParams(k).srcId, m, outParams(i).destId, j)(0)))
  //         }
  //       }
  //     }
  //   }
  // }


  def getIdx(outChannel: Int, outVirtChannel: Int): Int = {
    require(outChannel < nAllOutputs &&
      outVirtChannel < allOutParams(outChannel).nVirtualChannels)
    if (outChannel < nOutputs) {
      (0 until outVirtChannel).map(v =>
        outParams.count(_.nVirtualChannels > v)
      ).sum + outParams.take(outChannel).count(_.nVirtualChannels > outVirtChannel)
    } else {
      require(outVirtChannel == 0)
      outParams.map(_.nVirtualChannels).sum + outChannel - nOutputs
    }
  }
  def getOutChannelInfo(idx: Int): (Int, Int) = {
    for (outId <- 0 until nAllOutputs)
      for (outVirtId <- 0 until allOutParams(outId).nVirtualChannels)
        if (getIdx(outId, outVirtId) == idx) return (outId, outVirtId)
    require(false)
    return (-1, -1)
  }
  def getOutChannelInfo(idx: UInt): (UInt, UInt) = {
    val outId = MuxLookup(idx, 0.U(1.W), (0 until nOutChannels).map(i =>
      i.U -> getOutChannelInfo(i)._1.U
    ))
    val outVirtId = MuxLookup(idx, 0.U(1.W), (0 until nOutChannels).map(i =>
      i.U -> getOutChannelInfo(i)._2.U
    ))
    (outId, outVirtId)
  }

  for (outId <- 0 until nAllOutputs) {
    for (outVirtId <- 0 until allOutParams(outId).nVirtualChannels) {
      val idx = getIdx(outId, outVirtId)
      for (inId <- 0 until nAllInputs) {
        val r = io.req(inId)
        val legalPath = if (outId < nOutputs) {
          val table = Seq.tabulate(allInParams(inId).nVirtualChannels, 1 << userBits, nNodes) {
            case (inVirtId, user, destId) =>
              ((((inVirtId << userBits) + user) << nodeIdBits) + destId,
                rParams.vcAllocLegalPaths(allInParams(inId).srcId, inVirtId,
                  outParams(outId).destId, outVirtId, destId, user)
              )
          }.flatten.flatten

          val addr = Cat(r.bits.in_virt_channel, r.bits.in_user, r.bits.dest_id)
          table.filter(_._2).map(_._1.U === addr).orR
        } else {
          true.B
        }
        allocator.io.in(idx)(inId).valid := (r.valid &&
          r.bits.out_channels(outId) &&
          io.channel_available(outId)(outVirtId) &&
          legalPath
        )
      }
      io.out_allocs(outId)(outVirtId) := allocator.io.in(idx).map(_.fire()).reduce(_||_)
    }
  }

  (io.req zip io.resp).zipWithIndex.map { case ((req,resp),i) =>
    val fires = allocator.io.in.map(_(i).fire())
    val fire_id = OHToUInt(fires)
    val (out_id, out_virt_id) = getOutChannelInfo(fire_id)
    assert(PopCount(fires) <= 1.U)
    req.ready := fires.reduce(_||_)
    resp.valid := fires.reduce(_||_)
    resp.bits.in_virt_channel := req.bits.in_virt_channel
    resp.bits.out_channel := out_id
    resp.bits.out_virt_channel := out_virt_id
  }
}
