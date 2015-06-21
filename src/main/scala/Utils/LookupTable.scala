package OLK.Util

import Chisel._
import scala.collection.mutable.ArrayBuffer

class LookupTable[T <: Bits](val tableValues : ArrayBuffer[T]) extends Module {
  // table for generic type
  val io = new Bundle {
    val addr = UInt(INPUT, width=log2Up(tableValues.length))
    val out  = tableValues(0).clone.asOutput
  }
}

class LookupTableTests[T <: Bits](c: LookupTable[T]) extends Tester(c) {

}

