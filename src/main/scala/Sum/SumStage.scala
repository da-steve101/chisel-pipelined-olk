package OLK.Sum

import Chisel._
import scala.collection.mutable.ArrayBuffer
import cla.types._

/** SumStage
  This file implements the summation stage
  using SumL and SumR

  parameter s : stages
  parameter d : dictionary size  
  parameter (1/(1+r))

  input zi     = SFix[s + 1]
  input vi     = SFix[d]
  input alpha  = SFix
  input addToDict => 1 Bit
  input alphai = SFix[d]

  output zp = SFix
  output wD = SFix
  output Sum = SFix

  constant Lookup table:
  (1/(1+r)), (1/(1+r))^2, (1/(1+r))^3 ... (1/(1+r))^s
  
  Registers:
  zpReg  = SFix
  wDReg  = SFix
  SumReg = SFix

  Logic:
  SumL_ = SumL[stages = s-1]()
  SumR_ = SumR[stages = s-1, dict = d]()

  sumA = SumL_zp*alpha + SumL_SumL*(1/(1+r)) + SumR_SumR*(1/(1+r))^SumR_q
  sumB = (SumR_SumR + SumR_wD1)*(1/(1+r))^SumR_q + SumL_SumL

  SumReg = (addToDict) ? (sumA) : (sumB)
  zpReg = SumL_z_p1
  wDReg = (addToDict) ? SumR_wD*(1/(1+r))^SumR_q : SumR_wD1*(1/(1+r))^SumR_q

  zp = zpReg
  wD = wDReg
  Sum = SumReg

  */
class SumStage(val bitWidth : Int, val fracWidth : Int, val stages : ArrayBuffer[Boolean],
  val dictSize : Int, val isNORMA : Boolean) extends Module {
  val io = new Bundle {
    val alphai = Vec.fill(dictSize){Fixed(INPUT, bitWidth, fracWidth)} // Weights in dictionary
    val zi     = Vec.fill(stages.count(_ == true) + 1){Fixed(INPUT, bitWidth, fracWidth)} // Pipelined inputs (newest is 0)
    val vi     = Vec.fill(dictSize){Fixed(INPUT, bitWidth, fracWidth)} // Dictionary (newest is 0)
    val alpha  = Fixed(INPUT, bitWidth, fracWidth) // Weight for pipelined example
    val forget = Fixed(INPUT, bitWidth, fracWidth)
    val addToDict = Bool(INPUT)

    val sum = Fixed(OUTPUT, bitWidth, fracWidth)
    val zp  = Fixed(OUTPUT, bitWidth, fracWidth)
    val wD  = Fixed(OUTPUT, bitWidth, fracWidth)
  }
  val zero = Fixed(0, bitWidth, fracWidth)
  val sumReg = Reg(init=zero)
  val zpReg  = Reg(init=zero)
  val wDReg  = Reg(init=zero)
  io.sum := sumReg
  io.zp  := zpReg
  io.wD  := wDReg

  val sumLModule = Module(new SumL(bitWidth, fracWidth, stages.count(_ == true) - 1, isNORMA))
  sumLModule.io.z := io.zi
  sumLModule.io.addToDict := io.addToDict
  sumLModule.io.forget := io.forget
  sumLModule.io.alpha := io.alpha
  val sumL = sumLModule.io.sumL
  val sumLzp1 = sumLModule.io.zp1
  val forgetPowQ  = sumLModule.io.forgetPowQ
  val forgetPowQ1 = sumLModule.io.forgetPowQ1
  zpReg := sumLModule.io.zp

  val sumrStages = stages.dropRight(1)
  val sumRModule = Module(new SumR(bitWidth, fracWidth, dictSize, sumrStages))
  sumRModule.io.vi := io.vi
  sumRModule.io.alphai := io.alphai
  sumRModule.io.addToDict := io.addToDict
  val sumR = sumRModule.io.sumR
  val sumRwD1 = sumRModule.io.wD1

  wDReg := Mux(io.addToDict, forgetPowQ1*sumRwD1, forgetPowQ*sumRModule.io.wD)
  val sumNotAdd = sumL + (forgetPowQ*(sumR + sumRwD1))
  val sumIsAdd  = (io.forget*sumL) + (io.alpha*sumLzp1) + (forgetPowQ1*sumR)
  sumReg := Mux(io.addToDict, sumIsAdd, sumNotAdd)
}

class SumStageTests(c : SumStage) extends Tester(c) {
  def toFixed(x : Double, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
  def toFixed(x : Float, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
  def toFixed(x : Int, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
  val r = scala.util.Random

  val cycles = 2*c.stages.length
  val expectSum = ArrayBuffer.fill(c.stages.length)(0)
  val expectzp  = ArrayBuffer.fill(c.stages.length)(0)
  val expectwD  = ArrayBuffer.fill(c.stages.length)(0)
  val addToDicts = ArrayBuffer.fill(cycles)(r.nextInt(2))
  val alpha  = ArrayBuffer.fill(cycles)(r.nextInt(1 << (c.bitWidth/2)))
  val forget = r.nextInt(1 << (c.bitWidth/2))
  poke(c.io.forget, BigInt(forget))

  for (cyc <- 0 until cycles) {
    val alphai = ArrayBuffer.fill(c.dictSize)(r.nextInt(1 << (c.bitWidth/2)))
    val zi     = ArrayBuffer.fill(c.stages.length + 1)(r.nextInt(1 << (c.bitWidth/2)))
    val vi     = ArrayBuffer.fill(c.dictSize)(r.nextInt(1 << (c.bitWidth/2)))
    val wi     = new ArrayBuffer[Int]()
    val ui     = new ArrayBuffer[Int]()
    for (d <- 0 until c.dictSize)
      wi += (vi(d)*alphai(d)) >> c.fracWidth
    if (cyc < cycles - c.stages.length) {
      for (s <- 0 until c.stages.length)
        ui += (zi(s + 1)*alpha(cyc + s)) >> c.fracWidth
      // calculate wD
      var sumDicts = 0
      for (i <- cyc until (cyc + c.stages.length)) {
        sumDicts = sumDicts + addToDicts(i)
      }
      var wD = wi(c.dictSize - 1 - sumDicts)
      for (i <- 0 until sumDicts)
        wD  = (wD*forget) >> c.fracWidth
      expectwD += wD

      // calculate what the sum will be in c.stages time
      var sum = 0
      for (i <- 0 until (c.dictSize -1 - sumDicts))
        sum = sum + wi(i)
      for (i <- 0 until c.stages.length) {
        if (addToDicts(cyc + i) == 1) 
          sum = ((forget*sum) >> c.fracWidth) + ui(c.stages.length - 1 - i)
        else {
          if (c.isNORMA)
            sum = ((forget*sum) >> c.fracWidth)
        }
      }
      expectSum += sum
    }
    expectzp += zi(0)

    poke(c.io.addToDict, Bool(addToDicts(cyc) == 1).litValue())
    poke(c.io.alpha, BigInt(alpha(cyc)))
    for (d <- 0 until c.dictSize) {
      poke(c.io.alphai(d), BigInt(alphai(d)))
      poke(c.io.vi(d), BigInt(vi(d)))
    }
    for (s <- 0 until (c.stages.length + 1))
      poke(c.io.zi(s), zi(s))

    step(1)
    expect(c.io.sum, BigInt(expectSum(cyc)))
    expect(c.io.zp, BigInt(expectzp(cyc)))
    expect(c.io.wD, BigInt(expectwD(cyc)))
  }
}
