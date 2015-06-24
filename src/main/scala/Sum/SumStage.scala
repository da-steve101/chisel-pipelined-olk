package OLK.Sum

import Chisel._
import scala.collection.mutable.ArrayBuffer

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
  def log2Dict : Int = { log2Up(dictSize) }
  Predef.assert(stages.last == true, "The last stage must have a register")

  val ZERO = Fixed(0, bitWidth, fracWidth)
  val ONE  = Fixed(1.0, bitWidth, fracWidth)

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

  val sumLStages = stages.dropRight(1).count(_ == true)
  var sumL = ZERO
  var sumLzp1 = io.zi(1)
  var forgetPowQ  = ONE
  var forgetPowQ1 = io.forget
  val zpReg  = Reg(init=ZERO)
  io.zp  := zpReg

  if (sumLStages > 0) {
    val sumLModule = Module(new SumL(bitWidth, fracWidth, sumLStages, isNORMA))
    sumLModule.io.z := io.zi
    sumLModule.io.addToDict := io.addToDict
    sumLModule.io.forget := io.forget
    sumLModule.io.alpha := io.alpha
    sumL = sumLModule.io.sumL
    sumLzp1 = sumLModule.io.zp1
    forgetPowQ  = sumLModule.io.forgetPowQ
    forgetPowQ1 = sumLModule.io.forgetPowQ1
    zpReg  := sumLModule.io.zp
  } else {
    zpReg := io.zi(0)
  }

  val sumRModule = Module(new SumR(bitWidth, fracWidth, dictSize, stages.dropRight(1)))
  sumRModule.io.vi := io.vi
  sumRModule.io.alphai := io.alphai
  sumRModule.io.addToDict := io.addToDict
  val sumR = sumRModule.io.sumR
  val sumRwD1 = sumRModule.io.wD1

  val sumNotAdd = sumL + (forgetPowQ*(sumR + sumRwD1))
  val sumIsAdd  = (io.forget*sumL) + (io.alpha*sumLzp1) + (forgetPowQ1*sumR)

  // Last stage registers
  val sumReg = Reg(init=ZERO)
  val wDReg  = Reg(init=ZERO)
  sumReg := Mux(io.addToDict, sumIsAdd, sumNotAdd)
  io.sum := sumReg
  wDReg  := Mux(io.addToDict, forgetPowQ1*sumRwD1, forgetPowQ*sumRModule.io.wD)
  io.wD  := wDReg
}

class SumStageTests(c : SumStage) extends Tester(c) {
  def toFixed(x : Double, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
  def toFixed(x : Float, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
  def toFixed(x : Int, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
  val r = scala.util.Random

  val activeStages = c.stages.count(_ == true)

  val cycles     = 2*c.stages.length
  val expectSum  = ArrayBuffer.fill(activeStages - 1)(0)
  val expectzp   = ArrayBuffer.fill(activeStages - 1)(0)
  val expectzp1  = ArrayBuffer.fill(activeStages - 1)(0)
  val expectwD   = ArrayBuffer.fill(activeStages - 1)(0)
  val expectwD1  = ArrayBuffer.fill(activeStages - 1)(0)
  val sumLAry   = ArrayBuffer.fill(activeStages - 1)(0)
  val sumRAry   = ArrayBuffer.fill(activeStages - 1)(0)
  val wd1Ary    = ArrayBuffer.fill(activeStages - 1)(0)
  val addToDicts = ArrayBuffer.fill(cycles + activeStages){ r.nextInt(2) == 1}
  val alpha      = ArrayBuffer.fill(cycles + activeStages){r.nextInt(1 << ((2*c.fracWidth)/3))}
  val forget     = r.nextInt(1 << c.fracWidth)
  poke(c.io.forget, BigInt(forget))

  for (cyc <- 0 until cycles) {
    val alphai = ArrayBuffer.fill(c.dictSize)(r.nextInt(1 << ((2*c.fracWidth)/3)))
    val zi     = ArrayBuffer.fill(activeStages + 1)(r.nextInt(1 << ((2*c.fracWidth)/3)))
    val vi     = ArrayBuffer.fill(c.dictSize)(r.nextInt(1 << ((2*c.fracWidth)/3)))

    val forgetPowQ = {
      var sum = 1 << c.fracWidth
      for (i <- 0 until (activeStages - 1)){
        if ( c.isNORMA ) {
          sum = (forget*sum) >> c.fracWidth
        } else {
          if ( addToDicts(cyc + i) )
            sum = (forget*sum) >> c.fracWidth
        }
      }
      sum
    }
    val forgetPowQ1 = (forget*forgetPowQ) >> c.fracWidth

    // multiply with alpha
    val ui = (zi zip alpha.drop(cyc).take(activeStages + 1).reverse).map(pair => { (pair._1 * pair._2) >> c.fracWidth } )
    val wi = (vi zip alphai).map(pair => { (pair._1 * pair._2) >> c.fracWidth } )

    val zp = zi(0)
    val zp1 = zi(1)
    val activeDicts = addToDicts.drop(cyc).take(activeStages - 1)
    val totalAdd = activeDicts.count(_ == true)
    val wd  = wi(wi.length - 1 - totalAdd)
    val wD1 = wi(wi.length - 2 - totalAdd)
    val sumR = wi.dropRight(totalAdd + 2).sum
    val sumLAry = ui.drop(2)

    var sumL = 0
    for ( i <- 0 until sumLAry.length ) {
      if ( addToDicts(cyc + i) )
        sumL =  ((forget*sumL) >> c.fracWidth) + sumLAry(sumLAry.length - 1 - i)
      else {
        if ( c.isNORMA )
          sumL = (forget*sumL) >> c.fracWidth
      }
    }

    expectzp += zp
    expectzp1 += zp1
    expectwD += {
      if ( addToDicts(cyc + activeStages - 1) )
        (wD1*forgetPowQ1) >> c.fracWidth
      else
        (wd*forgetPowQ) >> c.fracWidth
    }
    expectwD1 += wD1
    expectSum += {
      if ( addToDicts(cyc + activeStages - 1) ) {
        ((forget*sumL) >> c.fracWidth) + ((forgetPowQ1*sumR) >> c.fracWidth) +
          ((alpha(cyc + activeStages - 1)*expectzp1(cyc + activeStages - 1)) >> c.fracWidth)
      } else
        sumL + ((forgetPowQ*(sumR + expectwD1(cyc + activeStages - 1))) >> c.fracWidth)
    }

    poke(c.io.addToDict, Bool(addToDicts(cyc)).litValue())
    poke(c.io.alpha, BigInt(alpha(cyc)))
    for (d <- 0 until c.dictSize) {
      poke(c.io.alphai(d), BigInt(alphai(d)))
      poke(c.io.vi(d), BigInt(vi(d)))
    }
    for (s <- 0 until (activeStages + 1))
      poke(c.io.zi(s), zi(s))

    step(1)
    if ( cyc > 2 ) {
      expect(c.io.sum, BigInt(expectSum(cyc)))
      expect(c.io.zp, BigInt(expectzp(cyc)))
      expect(c.io.wD, BigInt(expectwD(cyc)))
    }
  }
}
