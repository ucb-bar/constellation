package constellation.util

import chisel3._
import chisel3.util._

object WrapInc {
  // "n" is the number of increments, so we wrap at n-1.
  def apply(value: UInt, n: Int): UInt = {
    if (n == 1) {
      0.U
    } else if (isPow2(n)) {
      (value + 1.U)(log2Ceil(n)-1,0)
    } else {
      val wrap = (value === (n-1).U)
      Mux(wrap, 0.U, value + 1.U)
    }
  }

  def apply(value: UInt, n: UInt): UInt = {
    val wrap = value === n - 1.U
    Mux(wrap, 0.U, value + 1.U)
  }
}

