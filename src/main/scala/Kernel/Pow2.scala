package OLK.Kernel

import Chisel._
import cla.types._
import scala.collection.mutable.ArrayBuffer

/**
  This file computes y = 2^(-gamma*x) in fixed point
  It uses a a lookup table with a linear interpolation between the points
  Both x and gamma must be positive
  */
class Pow2(val bitWidth : Int, val fracWidth : Int, val stages : ArrayBuffer[Boolean],
  val lookupTableSize : Int) extends Module {
  def log2Table: Int = { (scala.math.log10(lookupTableSize)/scala.math.log10(2)).ceil.toInt }
  Predef.assert((1 << log2Table) == lookupTableSize, "Table size must be a power of 2")
  Predef.assert(log2Table < fracWidth, "Table size must be smaller than the number of fractional bits")
  Predef.assert(log2Table > 0, "Table size must be greater than zero")
  Predef.assert(stages.length == 5, "The size of stages must be exactly 5")
  val ZERO      = Fixed(0, bitWidth, fracWidth)
  val ZERO_UINT = UInt(0, width=(bitWidth-fracWidth))
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
    val gradAlt    = Fixed(INPUT, bitWidth, fracWidth)
    val offAlt     = Fixed(INPUT, bitWidth, fracWidth)
    val xint2Alt   = UInt(INPUT, width=(bitWidth - fracWidth))
    val yFracAlt   = Fixed(INPUT, bitWidth, fracWidth)
    val yAlt       = Fixed(INPUT, bitWidth, fracWidth)

    val xValOut    = Fixed(OUTPUT, bitWidth, fracWidth)
    val xintOut    = UInt(OUTPUT, width=(bitWidth - fracWidth))
    val xFracOut   = Fixed(OUTPUT, bitWidth, fracWidth)
    val gradTabOut = Fixed(OUTPUT, bitWidth, fracWidth)
    val offTabOut  = Fixed(OUTPUT, bitWidth, fracWidth)
    val xint1Out   = UInt(OUTPUT, width=(bitWidth - fracWidth))
    val gradOut    = Fixed(OUTPUT, bitWidth, fracWidth)
    val offOut     = Fixed(OUTPUT, bitWidth, fracWidth)
    val xint2Out   = UInt(OUTPUT, width=(bitWidth - fracWidth))
    val yFracOut   = Fixed(OUTPUT, bitWidth, fracWidth)
    val yOut       = Fixed(OUTPUT, bitWidth, fracWidth)

    val y = Fixed(OUTPUT, bitWidth, fracWidth)
  }

  // For now just have two ... replace later
  def optional(stage : Boolean, alt : Fixed, calc : Fixed) : Fixed = {
    if ( stage ) {
      // optional Register and Mux
      val xMux = Mux(io.addToDict, alt, calc)
      val xReg = Reg(init=ZERO)
      xReg := xMux
      xReg
    } else calc
  }
  def optional_UInt(stage : Boolean, alt : UInt, calc : UInt) : UInt = {
    if ( stage ) {
      // optional Register and Mux
      val xMux = Mux(io.addToDict, alt, calc)
      val xReg = Reg(init=ZERO_UINT)
      xReg := xMux
      xReg
    } else calc
  }


  // Generate Table for linear interpolation
  val gradients   = new ArrayBuffer[Int]()
  val offsets     = new ArrayBuffer[Int]()
  // Fixed point increment
  val increment   = 1.0 / (1 << (fracWidth - log2Table))
  val tableEnd    = 1.0
  var x = 0.0
  while (x < tableEnd) {
    // m = (y1 - y2)/(x1 - x2)
    val m = (scala.math.pow(2, - x) - scala.math.pow(2,- x - increment))/increment
    // convert to Fixed
    gradients += (m  * (1 << fracWidth)).toInt 
    // b = y1 + m*(-x1)
    val b = scala.math.pow(2, - x) + m*x
    // convert to Fixed
    offsets += (b * (1 << fracWidth)).toInt
    x += increment
  }

  // TODO: create Lookup Tables gradTable(lookupTableSize) and offsetTable(lookupTableSize)

  // multiply gamma*x
  val xValOut = io.gamma*io.x
  io.xValOut := xValOut
  val xVal = optional(stages(0), io.xValAlt, xValOut)

  // END OF STAGE 0

  // Split x into parts
  val x_int  = xVal(bitWidth - 1, fracWidth)
  val x_frac = xVal & FRAC
  val x_tabl = xVal(fracWidth - 1, fracWidth - log2Table)

  // get values from lookup table
  val gradTabOut = x_frac // TODO: replace x_frac with gradTable(addr=x_tabl)
  io.gradTabOut := gradTabOut
  val offTabOut = ZERO // TODO: replace ZERO with offsetTable(addr=x_tabl)
  io.offTabOut := offTabOut
  val xFracOut = x_frac
  io.xFracOut := xFracOut
  io.xintOut   := x_int
  val gradVal  = optional(stages(1), io.gradTabAlt, gradTabOut)
  val offVal   = optional(stages(1), io.offTabAlt,  offTabOut)
  val xFracVal = optional(stages(1), io.xFracAlt,   xFracOut)
  val x_int1   = optional_UInt(stages(1), io.xintAlt,  x_int)

  // END OF STAGE 1

  // calculate m*x
  val gradOut = gradVal*xFracVal
  val offOut  = offVal
  io.gradOut  := gradOut
  io.offOut   := offOut
  io.xint1Out := x_int1
  val mxVal     = optional(stages(2), io.gradAlt, gradOut)
  val offValReg = optional(stages(2), io.offAlt,  offOut)
  val x_int2    = optional_UInt(stages(2),  io.xint1Alt,  x_int1)

  // END OF STAGE 2

  // calculate y = mx + b
  val yFracOut = mxVal + offValReg
  io.yFracOut := yFracOut
  io.xint2Out := x_int2
  val yFrac         = optional(stages(3), io.yFracAlt, yFracOut)
  val x_int_delayed = optional_UInt(stages(3), io.xint2Alt,  x_int2)

  // END OF STAGE 3

  // calculate y >> x_int
  val yOut = yFrac >> x_int_delayed
  io.yOut := yOut
  val y      = optional(stages(4), io.yAlt, yOut)

  // return y
  io.y := y
}

class Pow2Tests(c : Pow2) extends Tester(c) {
  val xValOutAry    = new ArrayBuffer[Int]()
  val xFracOutAry   = new ArrayBuffer[Int]()
  val xintOutAry    = new ArrayBuffer[Int]()
  val gradTabOutAry = new ArrayBuffer[Int]()
  val offTabOutAry  = new ArrayBuffer[Int]()
  val xint1OutAry   = new ArrayBuffer[Int]()
  val gradOutAry    = new ArrayBuffer[Int]()
  val offOutAry     = new ArrayBuffer[Int]()
  val xint2OutAry   = new ArrayBuffer[Int]()
  val yFracOutAry   = new ArrayBuffer[Int]()
  val yOutAry       = new ArrayBuffer[Int]()
  val yAry          = new ArrayBuffer[Int]()

  // Fill stages enabled with a 0 to account for Reg delay
  if ( c.stages(0) )
    xValOutAry += 0
  if ( c.stages(1) ) {
    gradTabOutAry += 0
    offTabOutAry  += 0
    xintOutAry    += 0
    xFracOutAry   += 0
  }
  if ( c.stages(2) ) {
    gradOutAry  += 0
    xint1OutAry += 0
    offOutAry   += 0
  }
  if ( c.stages(3) ) {
    yFracOutAry += 0
    xint2OutAry += 0
  }
  if ( c.stages(4) )
    yOutAry += 0

  val cycles = 20
  val r = scala.util.Random

  for ( cyc <- 0 until cycles ) {
    // generate random inputs
    val x          = r.nextInt(1 << (c.bitWidth - 3))
    val gamma      = r.nextInt(1 << (c.bitWidth - 3))
    val addToDict  = (r.nextInt(2) == 1)
    val xValAlt    = r.nextInt(1 << (c.bitWidth - 3))
    val xintAlt    = r.nextInt(1 << ((c.bitWidth - c.fracWidth)/2))
    val xFracAlt   = r.nextInt(1 << ((c.bitWidth - c.fracWidth)/2))
    val gradTabAlt = r.nextInt(1 << (c.bitWidth/2))
    val offTabAlt  = r.nextInt(1 << (c.bitWidth/2))
    val xint1Alt   = r.nextInt(1 << ((c.bitWidth - c.fracWidth)/2))
    val gradAlt    = r.nextInt(1 << (c.bitWidth/2))
    val offAlt     = r.nextInt(1 << (c.bitWidth/2))
    val xint2Alt   = r.nextInt(1 << ((c.bitWidth - c.fracWidth)/2))
    val yFracAlt   = r.nextInt(1 << (c.bitWidth/2))
    val yAlt       = r.nextInt(1 << (c.bitWidth/2))

    poke(c.io.x,          BigInt(x))
    poke(c.io.gamma,      BigInt(gamma))
    poke(c.io.addToDict,  Bool(addToDict).litValue())
    poke(c.io.xValAlt,    BigInt(xValAlt))
    poke(c.io.xintAlt,    BigInt(xintAlt))
    poke(c.io.gradTabAlt, BigInt(gradTabAlt))
    poke(c.io.offTabAlt,  BigInt(offTabAlt))
    poke(c.io.xint1Alt,   BigInt(xint1Alt))
    poke(c.io.gradAlt,    BigInt(gradAlt))
    poke(c.io.offAlt,     BigInt(offAlt))
    poke(c.io.xint2Alt,   BigInt(xint2Alt))
    poke(c.io.yFracAlt,   BigInt(yFracAlt))
    poke(c.io.yAlt,       BigInt(yAlt))


    xValOutAry += {
      if ( c.stages(0) && addToDict ){
        xValAlt
      } else {
        ((gamma*x) >> c.fracWidth)
      } }

    // calculate what the values should be
    val xIndex = ((xValOutAry(cyc).toDouble /(1 << c.fracWidth))/c.increment).floor
    val xDub   = xIndex*c.increment

    // m = (y1 - y2)/(x1 - x2)
    val m = (((scala.math.pow(2, - xDub) - scala.math.pow(2,- xDub - c.increment))/c.increment) * (1 << c.fracWidth)).toInt
    // b = y1 + m*(-x1)
    val b = (( scala.math.pow(2, - xDub) + m*xDub) * (1 << c.fracWidth)).toInt

    xintOutAry += {
      if ( c.stages(1) && addToDict) {
        xintAlt
      } else {
        (xValOutAry(cyc) & ((1 << c.bitWidth) - (1 << c.fracWidth))) >> c.fracWidth
      }
    }
    xFracOutAry += {
      if ( c.stages(1) && addToDict) {
        xFracAlt
      } else {
        xValOutAry(cyc) & ((1 << c.fracWidth) - 1)
      }
    }
    gradTabOutAry += {
      if ( c.stages(1) && addToDict) {
        gradTabAlt
      } else {
        m
      }
    }
    offTabOutAry += {
      if ( c.stages(1) && addToDict) {
        offTabAlt
      } else {
        b
      }
    }

    xint1OutAry += {
      if ( c.stages(2) && addToDict) {
        xint1Alt
      } else {
        xintOutAry(cyc)
      }
    }
    gradOutAry += {
      if ( c.stages(2) && addToDict) {
        gradAlt
      } else {
        gradTabOutAry(cyc)*xFracOutAry(cyc)
      }
    }
    offOutAry += {
      if ( c.stages(2) && addToDict) {
        offAlt
      } else {
        offTabOutAry(cyc)
      }
    }

    xint2OutAry += {
      if ( c.stages(3) && addToDict) {
        xint2Alt
      } else {
        xint1OutAry(cyc)
      }
    }
    yFracOutAry += {
      if ( c.stages(3) && addToDict) {
        yFracAlt
      } else {
        gradTabOutAry(cyc) + offTabOutAry(cyc)
      }
    }

    yOutAry += {
      if ( c.stages(4) && addToDict) {
        yAlt
      } else {
        yFracOutAry(cyc) >> xintOutAry(cyc)
      }
    }
    yAry += yOutAry(cyc)

    expect(c.io.xValOut, BigInt(xValOutAry(cyc)))
    expect(c.io.xintOut, BigInt(xintOutAry(cyc)))
    expect(c.io.gradTabOut, BigInt(gradTabOutAry(cyc)))
    expect(c.io.offTabOut, BigInt(offTabOutAry(cyc)))
    expect(c.io.xint1Out, BigInt(xint1OutAry(cyc)))
    expect(c.io.gradOut, BigInt(gradOutAry(cyc)))
    expect(c.io.offOut, BigInt(offOutAry(cyc)))
    expect(c.io.xint2Out, BigInt(xint2OutAry(cyc)))
    expect(c.io.yFracOut, BigInt(yFracOutAry(cyc)))
    expect(c.io.yOut, BigInt(yOutAry(cyc)))
    expect(c.io.y, BigInt(yAry(cyc)))
    step(1)
  }

}


