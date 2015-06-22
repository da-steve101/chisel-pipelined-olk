package OLK.Util

import Chisel._
import scala.collection.mutable.ArrayBuffer

/** A basic, generic LUT implemented in chisel, with BRAMs or registers.
  * The output should appear on the same cycle as address is specified, though,
  * I'm not sure this can be done for BRAMs.
  *
  * Parameters:
  *     tableValues: ArrayBuffer[T], an array of values to define the LUT.
  * Inputs:
  *     addr: UInt, address to look up. We assume UInt < tableValues.length.
  * Outputs:
  *     out: T, The returned value of type T.
  */
class LookupTable[T <: Bits](val tableValues : ArrayBuffer[T]) extends Module {
    val io = new Bundle {
      val addr = UInt(INPUT, width=log2Up(tableValues.length))
      val out  = tableValues(0).clone.asOutput
    }
    val lut = Vec(tableValues)
    io.out := lut(io.addr)
}

class LookupTableTests[T <: Bits](c: LookupTable[T]) extends Tester(c) {

}

