// See LICENSE for license details.

/** LFSRs in all shapes and sizes.
  */

package chisel3.util

import chisel3._

// scalastyle:off magic.number
object LFSR16 {
  /** Generates a 16-bit linear feedback shift register, returning the register contents.
    * May be useful for generating a pseudorandom sequence.
    *
    * @param increment optional control to gate when the LFSR updates.
    */
  def apply(increment: Bool = Bool(true)): UInt = {
    val width = 16
    val lfsr = Reg(init=UInt(1, width))
    when (increment) { lfsr := Cat(lfsr(0)^lfsr(2)^lfsr(3)^lfsr(5), lfsr(width-1,1)) }
    lfsr
  }
}
// scalastyle:on magic.number

