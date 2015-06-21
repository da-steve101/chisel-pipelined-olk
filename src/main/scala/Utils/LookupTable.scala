package OLK.Util

import Chisel._
import scala.collection.mutable.ArrayBuffer

/** A basic, generic LUT implemented in chisel, with a.
  * Parameters:
  *     tableValues: ArrayBuffer[T], an array of values to define the LUT.
  * Inputs:
  *     addr: UInt, address to look up. We assume UInt < tableValues.length.
  * Outputs:
  *     out: T, The returned value of type T.
  */
class LookupTable[T <: Bits](val tableValues : ArrayBuffer[T]) extends Module {
  // table for generic type
  val io = new Bundle {
    val addr = UInt(INPUT, width=log2Up(tableValues.length))
    val out  = tableValues(0).clone.asOutput
  }
}

class LookupTableTests[T <: Bits](c: LookupTable[T]) extends Tester(c) {

}

