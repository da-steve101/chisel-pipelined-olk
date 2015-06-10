package OLK.Sum

import Chisel._
import OLK._

/** SumL
  This block sums the stages in the pipeline
  parameter s : stages
  
  input z_i => SFix[s+1]
  input alpha => SFix
  input 1/(1+r) => Constant
  input addToDict => 1 Bit
  
  output z_p1 => SFix
  output zp   => SFix
  output sumL => SFix
  
  Registers:
  Stage_0[s+1], Stage_1[s], ... Stage_(s-1)[2]
  SumL_0, SumL_1, ... SumL_(s-1)

  Logic:
  Stage_0 = z_i[1:end-1] 
  SumL_0 = (addToDict) ? (z_i[end]*alpha) : 0
  for i = 0:(s-1){
      Stage_(i+1)[1:end] = Stage_(i)[1:end-1]
      SumL_(i+1) = (addToDict) ? (Stage_(i)[end]*alpha + (1/(1+r))*SumL_(i)) : (SumL_(i))
  }
  z_p1 = Stage_(s-1)[0]
  zp   = Stage_(s-1)[1]
  sumL = SumL_(s-1)
  
  */
class SumL(val bitWidth : Int, val intLength : Int, val stages : Int) extends Module {
  val io = new Bundle {
    val z      = Vec.fill(stages + 2){SFix(intLength, bitWidth).asInput}
    val alpha  = SFix(intLength, bitWidth).asInput
    val forget = SFix(intLength, bitWidth).asInput
    val addToDict = Bool(INPUT)

    val zp1    = SFix(intLength, bitWidth).asOutput
    val zp     = SFix(intLength, bitWidth).asOutput
    val sumL   = SFix(intLength, bitWidth).asOutput
  }
  assert(stages > 0)
  val zero        = new SFix(intLength, SInt(0, width=bitWidth))

  // Registers
  val stageAry = Array()
  for (s <- 0 until stages) {
    // Generate stage tree with one decreasing each stage
    stageAry :+= Vec.fill(stages + 1 - s){Reg(init=zero)}
  }
  val sumLStages = Vec.fill(stages){Reg(init=zero)}
  val sumLCalc   = Vec.fill(stages){zero}

  // Forward all unused Z vals to next stage
  for (s <- 0 until (stages + 1)) {
    stageAry(0)(s) := io.z(s) // Get from inputs
    for (a <- 1 until stages) {
      if (s < (stages + 1 - a))
        stageAry(a)(s) := stageAry(a - 1)(s)
    }
  }
  io.zp  := stageAry(stages - 1)(0)
  io.zp1 := stageAry(stages - 1)(1)

  // Calculate the sum if the example is added each cycle
  sumLCalc(0) := io.alpha*io.z(stages + 1)
  when (io.addToDict) {
    sumLStages(0) := sumLCalc(0)
  } .otherwise {
    sumLStages(0) := zero
  }
  for (a <- 1 until stages) {
    sumLCalc(a) := io.alpha*stageAry(a - 1)(stages + 1 - a) + io.forget*sumLStages(a - 1)
    when (io.addToDict) {
      sumLStages(a) := sumLCalc(a)
    } .otherwise {
      sumLStages(a) := sumLStages(a - 1)
    }
  }
  io.sumL := sumLStages(stages - 1)
}

class SumLTests(c : SumL) extends Tester(c) { 
  poke(c.io.forget.raw, BigInt(1 << (c.bitWidth - c.intLength)))
  poke(c.io.addToDict, Bool(false).litValue())

  val z = 1 << (c.bitWidth - c.intLength)
  val alpha = 3 << (c.bitWidth - c.intLength - 2) // 0.75
  val forget = 1 << (c.bitWidth - c.intLength - 1) // 0.5

  for (s <- 0 until (c.stages + 2))
    poke(c.io.z(s).raw, BigInt(z))

  // Check that z and sum propogates
  step(c.stages)
  expect(c.io.zp1.raw, BigInt(z))
  expect(c.io.zp.raw, BigInt(z))
  expect(c.io.sumL.raw, BigInt(0))

  // Check the sum is calculated properly
  var sumL = BigInt(0)
  for (s <- 0 until c.stages)
    sumL = (sumL >> 1) + BigInt(alpha)
  poke(c.io.addToDict, Bool(true).litValue())
  step(c.stages)
  expect(c.io.sumL.raw, sumL)
}
