/** Pow2.scala -> This file computes the power of 2 in fixed point using a lookup table with linear interpolation
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

package OLK.Kernel

import Chisel._

import scala.collection.mutable.ArrayBuffer

/**
  This file computes y = 2^(-gamma*x) in fixed point
  It uses a a lookup table with a linear interpolation between the points
  Both x and gamma must be positive
  */
class Pow2(val bitWidth : Int, val fracWidth : Int, val stages : ArrayBuffer[Boolean],
  val lookupTableSize : Int) extends Module {
  Predef.assert(lookupTableSize > 0, "Table size must be greater than zero")
  def log2Table: Int = { if (lookupTableSize == 1) 0 else log2Up(lookupTableSize) }
  def limitShift: BigInt = { BigInt((1 << log2Up(fracWidth)) - 1) }
  Predef.assert((1 << log2Table) == lookupTableSize, "Table size must be a power of 2")
  Predef.assert(log2Table < fracWidth, "Table size must be smaller than the number of fractional bits")
  Predef.assert(stages.length == 5, "The size of stages must be exactly 5")
  val FRAC      = Fixed(BigInt((1 << fracWidth) - 1), bitWidth, fracWidth)

  val io = new Bundle {
    val x     = Fixed(INPUT, bitWidth, fracWidth)
    val gamma = Fixed(INPUT, bitWidth, fracWidth)
    val addToDict = Bool(INPUT)

    val xValAlt    = Fixed(INPUT, bitWidth, fracWidth)
    val xintAlt    = UInt(INPUT, width=(bitWidth - fracWidth))
    val xFracAlt   = Fixed(INPUT, bitWidth, fracWidth)
    val gradTabAlt = Fixed(INPUT, bitWidth, fracWidth)
    val offTabAlt  = Fixed(INPUT, bitWidth, fracWidth)
    val xint1Alt   = UInt(INPUT, width=(bitWidth - fracWidth))
    val limitAlt   = Bool(INPUT)
    val gradAlt    = Fixed(INPUT, bitWidth, fracWidth)
    val offAlt     = Fixed(INPUT, bitWidth, fracWidth)
    val xint2Alt   = UInt(INPUT, width=(log2Up(fracWidth) + 1))
    val yFracAlt   = Fixed(INPUT, bitWidth, fracWidth)
    val yAlt       = Fixed(INPUT, bitWidth, fracWidth)

    val xValOut    = Fixed(OUTPUT, bitWidth, fracWidth)
    val xintOut    = UInt(OUTPUT, width=(bitWidth - fracWidth))
    val xFracOut   = Fixed(OUTPUT, bitWidth, fracWidth)
    val gradTabOut = Fixed(OUTPUT, bitWidth, fracWidth)
    val offTabOut  = Fixed(OUTPUT, bitWidth, fracWidth)
    val xint1Out   = UInt(OUTPUT, width=(bitWidth - fracWidth))
    val limitOut   = Bool(OUTPUT)
    val gradOut    = Fixed(OUTPUT, bitWidth, fracWidth)
    val offOut     = Fixed(OUTPUT, bitWidth, fracWidth)
    val xint2Out   = UInt(OUTPUT, width=(log2Up(fracWidth) + 1))
    val yFracOut   = Fixed(OUTPUT, bitWidth, fracWidth)
    val yOut       = Fixed(OUTPUT, bitWidth, fracWidth)

    val y = Fixed(OUTPUT, bitWidth, fracWidth)
  }

  def optional[T <: Data](stage : Boolean, alt : T, calc : T) : T = {
    // optional Register and Mux
    if ( stage )
      RegNext(Mux(io.addToDict, alt, calc))
    else
      calc
  }

  // Generate Table for linear interpolation
  val gradients   = new ArrayBuffer[Int]()
  val offsets     = new ArrayBuffer[Int]()
  // Fixed point increment
  val increment   = 1.0 / (1 << log2Table)
  val tableEnd    = 1.0
  var x = 0.0
  // NOTE: x is positive, therefore gradient is negitive
  while (x < tableEnd) {
    // m = (y1 - y2)/(x1 - x2)
    val m = -(scala.math.pow(2, - x) - scala.math.pow(2,- x - increment))/increment
    // convert to Fixed
    gradients += (m  * (1 << fracWidth)).toInt 
    // b = y1 - m*x1
    val b = scala.math.pow(2, - x) - m*x
    // convert to Fixed
    offsets += (b * (1 << fracWidth)).toInt
    x += increment
  }

  // Create Lookup Tables gradTable(gradients) and offsetTable(offsets)
  val gradTable = Vec(gradients.map((i: Int) => Fixed(BigInt(i), bitWidth, fracWidth)))
  val offsetTable = Vec(offsets.map((i: Int) => Fixed(BigInt(i), bitWidth, fracWidth)))

  // multiply gamma*x
  val xValOut = io.gamma*%io.x
  io.xValOut := xValOut
  val xVal = optional[Fixed](stages(0), io.xValAlt, xValOut)

  // END OF STAGE 0

  // Split x into parts
  val x_int  = xVal(bitWidth - 1, fracWidth)
  val x_frac = xVal & FRAC
  val x_tabl = {
    if ( log2Table == 0 )
      UInt(0, width=1)
    else
      xVal(fracWidth - 1, fracWidth - log2Table)
  }

  // get values from lookup table
  val gradTabOut = gradTable(x_tabl)
  io.gradTabOut := gradTabOut
  val offTabOut = offsetTable(x_tabl)
  io.offTabOut := offTabOut
  val xFracOut = x_frac
  io.xFracOut := xFracOut
  io.xintOut   := x_int
  val gradVal  = optional[Fixed](stages(1), io.gradTabAlt, gradTabOut)
  val offVal   = optional[Fixed](stages(1), io.offTabAlt,  offTabOut)
  val xFracVal = optional[Fixed](stages(1), io.xFracAlt,   xFracOut)
  val x_int1   = optional[UInt](stages(1), io.xintAlt,  x_int)

  // END OF STAGE 1

  // calculate m*x
  val gradOut = gradVal*%xFracVal
  val offOut  = offVal
  val limitOut = {
    if ( bitWidth - fracWidth <= log2Up(fracWidth) )
      Bool(false)
    else
      (x_int1 >= UInt(limitShift, width=(bitWidth - fracWidth)))
  }
  io.gradOut  := gradOut
  io.offOut   := offOut
  io.xint1Out := x_int1
  io.limitOut := limitOut
  val limit     = optional[Bool](stages(2), io.limitAlt, limitOut)
  val mxVal     = optional[Fixed](stages(2), io.gradAlt, gradOut)
  val offValReg = optional[Fixed](stages(2), io.offAlt,  offOut)
  val x_int2    = optional[UInt](stages(2), io.xint1Alt, x_int1)

  // END OF STAGE 2

  // calculate y = mx + b
  val yFracOut = mxVal + offValReg
  // Need to have a zero in MSB (which should be optimized out) so that not interpreted as negitive shift
  val xint2Out = Mux(limit, UInt(limitShift, width=(log2Up(fracWidth) + 1)),
    x_int2(scala.math.min(log2Up(fracWidth), bitWidth - fracWidth - 1), 0) & UInt(limitShift, width=(log2Up(fracWidth) + 1)))
  io.yFracOut := yFracOut
  io.xint2Out := xint2Out
  val yFrac         = optional[Fixed](stages(3), io.yFracAlt, yFracOut)
  val x_int_delayed = optional[UInt](stages(3), io.xint2Alt, xint2Out)

  // END OF STAGE 3

  // calculate y >> x_int
  val yOut = yFrac >> x_int_delayed
  io.yOut := yOut
  val y    = optional[Fixed](stages(4), io.yAlt, yOut)

  // return y
  io.y := y
}
