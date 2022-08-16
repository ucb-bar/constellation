package constellation.router

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.util._

import constellation.channel._

// class FastForwardInputUnit(
//   val inParams: Seq[ChannelParams],
//   val outParams: Seq[ChannelParams],
//   val egressParams: Seq[EgressChannelParams]) extends Module with HasRouterOutputParams with HaSNoCParams {

//   val io = IO(new Bundle {
//     val enq = Flipped(Valid(new Flit(c
//   })
// }
