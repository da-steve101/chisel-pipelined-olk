/** SumStage.scala -> This file computes the sum of terms for computation of the decision function
Copyright (C) 2015 Stephen Tridgell

This file is part of a pipelined OLK application.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this code.  If not, see <http://www.gnu.org/licenses/>.
*/

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
    val forceNA = Bool(INPUT)

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
    sumLModule.io.forceNA := io.forceNA
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
  val sumRwD = sumRModule.io.wD

  val sumLForceNA = Mux(io.forceNA, sumL, io.forget*%sumL)
  val sumRForceNA = Mux(io.forceNA, forgetPowQ*%sumR, forgetPowQ1*%sumR)
  val sumRwD1ForceNA = Mux(io.forceNA, forgetPowQ*%sumRwD1, forgetPowQ1*%sumRwD1)
  val sumRwDForceNA = Mux(io.forceNA, forgetPowQ*%sumRwD, forgetPowQ1*%sumRwD)

  val sumNotAdd =  {
    if ( isNORMA )
      sumRForceNA + sumRwD1ForceNA + sumLForceNA
    else
      (forgetPowQ*%(sumR + sumRwD1)) + sumL
  }
  val sumIsAdd  = sumLForceNA + (io.alpha*%sumLzp1) + sumRForceNA

  // Last stage registers
  val sumReg = Reg(init=ZERO)
  val wDReg  = Reg(init=ZERO)
  sumReg := Mux(io.addToDict, sumIsAdd, sumNotAdd)
  io.sum := sumReg
  wDReg  := Mux(io.addToDict, sumRwD1ForceNA, {
    if ( isNORMA )
      sumRwDForceNA
    else
      forgetPowQ*%sumRModule.io.wD
  })
  io.wD  := wDReg
}
