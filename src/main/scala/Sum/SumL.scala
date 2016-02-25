/** SumL.scala -> This file computes the Sum of terms ahead in the pipeline
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


/** SumL
  This block sums the stages in the pipeline
  parameter s : stages
  
  input z_i => Fixed[s+1]
  input alpha => Fixed
  input 1/(1+r) => Constant
  input addToDict => 1 Bit
  
  output z_p1 => Fixed
  output zp   => Fixed
  output sumL => Fixed
  
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
class SumL(val bitWidth : Int, val fracWidth : Int, val stages : Int, val isNORMA : Boolean) extends Module {
  Predef.assert(stages > 0, "There must be atleast one stage")
  val io = new Bundle {
    val z      = Vec.fill(stages + 2){Fixed(INPUT, bitWidth, fracWidth)}
    val alpha  = Fixed(INPUT, bitWidth, fracWidth)
    val forget = Fixed(INPUT, bitWidth, fracWidth)
    val addToDict = Bool(INPUT)
    val forceNA = Bool(INPUT)

    val forgetPowQ  = Fixed(OUTPUT, bitWidth, fracWidth)
    val forgetPowQ1 = Fixed(OUTPUT, bitWidth, fracWidth)
    val zp1    = Fixed(OUTPUT, bitWidth, fracWidth)
    val zp     = Fixed(OUTPUT, bitWidth, fracWidth)
    val sumL   = Fixed(OUTPUT, bitWidth, fracWidth)
  }
  val ZERO = Fixed(0.0, bitWidth, fracWidth)
  val ONE = Fixed(1.0, bitWidth, fracWidth)

  // Registers
  val stageAry = new ArrayBuffer[Vec[Fixed]]()
  for (s <- 0 until stages) {
    // Generate stage tree with one decreasing each stage
    stageAry += Vec.fill(stages + 1 - s){Reg(init=ZERO)}
  }
  val sumLStages = Vec.fill(stages){Reg(init=ZERO)}
  val forgetPow  = {
    if (stages == 1) {
      Vec.fill(1)(ONE)
    } else {
      val res = Vec.fill(stages - 1){Reg(init=ONE)}
      if ( isNORMA )
        res(0) := Mux(io.forceNA, ONE, io.forget)
      else
        res(0) := Mux(io.addToDict, io.forget, ONE)
      res
    } }

  val forgetPowSq = Reg(init=ZERO)
  forgetPowSq    := io.forget*%io.forget

  val forgetPowQ  = Reg(init=ZERO) // forget^q
  val forgetPowQ1 = Reg(init=ZERO) // forget^(q+1)
  io.forgetPowQ  := forgetPowQ
  io.forgetPowQ1 := forgetPowQ1

  if (isNORMA) {
    forgetPowQ  := Mux(io.forceNA, forgetPow.last, io.forget*%forgetPow.last)
    forgetPowQ1 := Mux(io.forceNA, io.forget*%forgetPow.last, forgetPowSq*%forgetPow.last)
    for (s <- 1 until (stages - 1))
      forgetPow(s) := Mux(io.forceNA, forgetPow(s - 1), io.forget*%forgetPow(s - 1))
   } else {
    forgetPowQ  := Mux(io.addToDict, io.forget*%forgetPow.last, forgetPow.last)
    forgetPowQ1 := Mux(io.addToDict, forgetPowSq*%forgetPow.last, io.forget*%forgetPow.last)
    for (s <- 1 until (stages - 1))
      forgetPow(s) := Mux(io.addToDict, io.forget*%forgetPow(s - 1), forgetPow(s - 1))
  }

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
  sumLStages(0) := Mux(io.addToDict && !io.forceNA, io.alpha*%io.z(stages + 1), ZERO)
  for (a <- 1 until stages) {
    if ( isNORMA ) {
      val sumLForceNA = Mux(io.forceNA, sumLStages(a - 1), io.forget*%sumLStages(a - 1))
      sumLStages(a) := Mux(io.addToDict && !io.forceNA, io.alpha*%stageAry(a - 1)(stages + 1 - a) + io.forget*%sumLStages(a - 1), sumLForceNA)
    } else
      sumLStages(a) := Mux(io.addToDict, io.alpha*%stageAry(a - 1)(stages + 1 - a) + io.forget*%sumLStages(a - 1), sumLStages(a - 1))
  }
  io.sumL := sumLStages.last
}
