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
  def log2Table: Int = { log2Up(lookupTableSize) }
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
  val xValOut = io.gamma*io.x
  io.xValOut := xValOut
  val xVal = optional[Fixed](stages(0), io.xValAlt, xValOut)

  // END OF STAGE 0

  // Split x into parts
  val x_int  = xVal(bitWidth - 1, fracWidth)
  val x_frac = xVal & FRAC
  val x_tabl = xVal(fracWidth - 1, fracWidth - log2Table)

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
  val gradOut = gradVal*xFracVal
  val offOut  = offVal
  val limitOut = (x_int1 >= UInt(limitShift, width=(bitWidth - fracWidth)))
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
    x_int2(log2Up(fracWidth), 0) & UInt(limitShift, width=(log2Up(fracWidth) + 1)))
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

class Pow2Tests(c : Pow2) extends Tester(c) {
  val xValOutAry    = new ArrayBuffer[Int]()
  val xFracOutAry   = new ArrayBuffer[Int]()
  val xintOutAry    = new ArrayBuffer[Int]()
  val gradTabOutAry = new ArrayBuffer[Int]()
  val offTabOutAry  = new ArrayBuffer[Int]()
  val xint1OutAry   = new ArrayBuffer[Int]()
  val limitOutAry   = new ArrayBuffer[Boolean]()
  val gradOutAry    = new ArrayBuffer[Int]()
  val offOutAry     = new ArrayBuffer[Int]()
  val xint2OutAry   = new ArrayBuffer[Int]()
  val yFracOutAry   = new ArrayBuffer[Int]()
  val yOutAry       = new ArrayBuffer[Int]()
  val yAry          = new ArrayBuffer[Int]()

  // Fill stages enabled with a 0 to account for Reg delay
  if ( c.stages(0) ) {
    gradTabOutAry += 0
    offTabOutAry  += 0
    xintOutAry    += 0
    xFracOutAry   += 0
  }
  if ( c.stages(1) ) {
    gradOutAry  += 0
    xint1OutAry += 0
    offOutAry   += 0
    limitOutAry += false
  }
  if ( c.stages(2) ) {
    yFracOutAry += 0
    xint2OutAry += 0
  }
  if ( c.stages(3) )
    yOutAry += 0
  if ( c.stages(4) )
    yAry += 0

  val cycles = 20
  val r = scala.util.Random

  for ( cyc <- 0 until cycles ) {
    // generate random inputs
    val x          = r.nextInt(1 << ((2*c.bitWidth)/3))
    val gamma      = r.nextInt(1 << ((2*c.bitWidth)/3))
    val addToDict  = (r.nextInt(2) == 1)
    val xValAlt    = r.nextInt(1 << ((2*c.bitWidth)/3))
    val xintAlt    = r.nextInt(1 << ((c.bitWidth - c.fracWidth)/2))
    val xFracAlt   = r.nextInt(1 << ((c.bitWidth - c.fracWidth)/2))
    val gradTabAlt = r.nextInt(1 << ((2*c.bitWidth)/3))
    val offTabAlt  = r.nextInt(1 << ((2*c.bitWidth)/3))
    val xint1Alt   = r.nextInt(1 << ((c.bitWidth - c.fracWidth)/2))
    val limitAlt   = (r.nextInt(2) == 1)
    val gradAlt    = r.nextInt(1 << ((2*c.bitWidth)/3))
    val offAlt     = r.nextInt(1 << ((2*c.bitWidth)/3))
    val xint2Alt   = r.nextInt(1 << ((c.bitWidth - c.fracWidth)/2))
    val yFracAlt   = r.nextInt(1 << ((2*c.bitWidth)/3))
    val yAlt       = r.nextInt(1 << ((2*c.bitWidth)/3))

    poke(c.io.x,          BigInt(x))
    poke(c.io.gamma,      BigInt(gamma))
    poke(c.io.addToDict,  Bool(addToDict).litValue())
    poke(c.io.xValAlt,    BigInt(xValAlt))
    poke(c.io.xintAlt,    BigInt(xintAlt))
    poke(c.io.xFracAlt,   BigInt(xFracAlt))
    poke(c.io.gradTabAlt, BigInt(gradTabAlt))
    poke(c.io.offTabAlt,  BigInt(offTabAlt))
    poke(c.io.xint1Alt,   BigInt(xint1Alt))
    poke(c.io.limitAlt,   Bool(limitAlt).litValue())
    poke(c.io.gradAlt,    BigInt(gradAlt))
    poke(c.io.offAlt,     BigInt(offAlt))
    poke(c.io.xint2Alt,   BigInt(xint2Alt))
    poke(c.io.yFracAlt,   BigInt(yFracAlt))
    poke(c.io.yAlt,       BigInt(yAlt))

    xValOutAry += ((gamma*x) >> c.fracWidth)

    // calculate what the values should be
    val xIndex = ((xValOutAry(cyc).toDouble /(1 << c.fracWidth))/c.increment).floor
    val xDub   = xIndex*c.increment
    val xIndexAlt = ((xValAlt.toDouble /(1 << c.fracWidth))/c.increment).floor
    val xDubAlt   = xIndexAlt*c.increment

    // m = (y1 - y2)/(x1 - x2)
    val m    = -(scala.math.pow(2, - xDub) - scala.math.pow(2,- xDub - c.increment))/c.increment
    val mAlt = -(scala.math.pow(2, - xDubAlt) - scala.math.pow(2,- xDubAlt - c.increment))/c.increment
    // b = y1 - m*x1
    val b    = scala.math.pow(2, - xDub) - m*xDub
    val bAlt = scala.math.pow(2, - xDubAlt) - mAlt*xDubAlt

    xintOutAry += {
      if ( c.stages(0) && addToDict) {
        (xValAlt & ((1 << c.bitWidth) - (1 << c.fracWidth))) >> c.fracWidth
      } else {
        (xValOutAry(cyc) & ((1 << c.bitWidth) - (1 << c.fracWidth))) >> c.fracWidth
      }
    }
    xFracOutAry += {
      if ( c.stages(0) && addToDict) {
        xValAlt & ((1 << c.fracWidth) - 1)
      } else {
        xValOutAry(cyc) & ((1 << c.fracWidth) - 1)
      }
    }
    gradTabOutAry += {
      if ( c.stages(0) && addToDict) {
        (mAlt * (1 << c.fracWidth)).toInt
      } else {
        (m * (1 << c.fracWidth)).toInt
      }
    }
    offTabOutAry += {
      if ( c.stages(0) && addToDict) {
        (bAlt * (1 << c.fracWidth)).toInt
      } else {
        (b * (1 << c.fracWidth)).toInt
      }
    }

    xint1OutAry += {
      if ( c.stages(1) && addToDict) {
        xintAlt
      } else {
        xintOutAry(cyc)
      }
    }
    limitOutAry += {
      if ( c.stages(1) && addToDict )
        xintAlt >= c.limitShift.toInt
      else
        xintOutAry(cyc) >= c.limitShift.toInt
    }
    gradOutAry += {
      if ( c.stages(1) && addToDict) {
        (gradTabAlt*xFracAlt) >> c.fracWidth
      } else {
        (gradTabOutAry(cyc)*xFracOutAry(cyc)) >> c.fracWidth
      }
    }
    offOutAry += {
      if ( c.stages(1) && addToDict) {
        offTabAlt
      } else {
        offTabOutAry(cyc)
      }
    }

    xint2OutAry += {
      if ( c.stages(2) && addToDict) {
        if ( limitAlt )
          c.limitShift.toInt
        else
          xint1Alt & c.limitShift.toInt
      } else {
        if ( limitOutAry(cyc) )
          c.limitShift.toInt
        else
          xint1OutAry(cyc) & c.limitShift.toInt
      }
    }
    yFracOutAry += {
      if ( c.stages(2) && addToDict) {
        gradAlt + offAlt
      } else {
        gradOutAry(cyc) + offOutAry(cyc)
      }
    }

    yOutAry += {
      if ( c.stages(3) && addToDict) {
        yFracAlt >> xint2Alt
      } else {
        yFracOutAry(cyc) >> xint2OutAry(cyc)
      }
    }

    yAry += {
      if ( c.stages(4) && addToDict) {
        yAlt
      } else {
        yOutAry(cyc)
      }
    }

    if (cyc >= c.stages.count(_ == true)) {
      expect(c.io.xValOut,    BigInt(xValOutAry(cyc)))
      expect(c.io.xintOut,    BigInt(xintOutAry(cyc)))
      expect(c.io.xFracOut,   BigInt(xFracOutAry(cyc)))
      expect(c.io.gradTabOut, BigInt(gradTabOutAry(cyc)))
      expect(c.io.offTabOut,  BigInt(offTabOutAry(cyc)))
      expect(c.io.xint1Out,   BigInt(xint1OutAry(cyc)))
      expect(c.io.limitOut,   Bool(limitOutAry(cyc)).litValue())
      expect(c.io.gradOut,    BigInt(gradOutAry(cyc)))
      expect(c.io.offOut,     BigInt(offOutAry(cyc)))
      expect(c.io.xint2Out,   BigInt(xint2OutAry(cyc)))
      expect(c.io.yFracOut,   BigInt(yFracOutAry(cyc)))
      expect(c.io.yOut,       BigInt(yOutAry(cyc)))
      expect(c.io.y,          BigInt(yAry(cyc)))
    }
    step(1)
  }

}


