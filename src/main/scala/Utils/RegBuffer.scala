package OLK.Utils

import Chisel._
import scala.collection.mutable.ArrayBuffer
import cla.types._

/**
  This class delays a signal by 'stages' cycles
  */
class RegBuffer(val initVal : Fixed, val stages : Int) extends Module {
  val io = new Bundle {
    val in  = initVal.clone.asInput
    val out = initVal.clone.asOutput
  }

  if (stages < 1) {
    io.out := io.in
  } else {
    val buffer = Vec.fill(stages){Reg(init=initVal.clone)}
    buffer.head := io.in
    for (i <- 1 until buffer.length)
      buffer(i) := buffer(i - 1)
    io.out := buffer.last
  }
}

class RegBufferTests(c : RegBuffer) extends Tester(c) {
  val r = scala.util.Random
  val testVals = ArrayBuffer.fill(2*c.stages)(r.nextInt)

  for(i <- 0 until 2*c.stages){
    poke(c.io.in, BigInt(testVals(i)))
    if (i < c.stages)
      expect(c.io.out, c.initVal.litValue())
    else
      expect(c.io.out, BigInt(testVals(i - c.stages)))
    step(1)
  }
}
