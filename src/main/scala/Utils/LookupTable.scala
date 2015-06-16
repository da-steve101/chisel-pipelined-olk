package OLK.Util

import Chisel._
import scala.collection.mutable.ArrayBuffer

class LookupTable(val tableValues : ArrayBuffer[Int]) {
  def log2Table: Int = { (scala.math.log10(tableValues.length)/scala.math.log10(2)).ceil.toInt }
  // table for generic type
  val io = new Bundle {
    val addr = UInt(INPUT, width=log2Table)
    val out  = dtype.clone.asOutput
  }

}

class LookupTableTests(c : LookupTable) extends Tester(c) {


}
