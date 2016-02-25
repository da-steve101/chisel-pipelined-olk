import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import Chisel._
import OLK.Kernel._

class Pow2Suite extends TestSuite {

  @Test def pow2Test {
    class Pow2Tests(c : Pow2) extends Tester(c) {
      val xValOutAry    = new ArrayBuffer[BigInt]()
      val xFracOutAry   = new ArrayBuffer[BigInt]()
      val xintOutAry    = new ArrayBuffer[BigInt]()
      val gradTabOutAry = new ArrayBuffer[BigInt]()
      val offTabOutAry  = new ArrayBuffer[BigInt]()
      val xint1OutAry   = new ArrayBuffer[BigInt]()
      val limitOutAry   = new ArrayBuffer[Boolean]()
      val gradOutAry    = new ArrayBuffer[BigInt]()
      val offOutAry     = new ArrayBuffer[BigInt]()
      val xint2OutAry   = new ArrayBuffer[BigInt]()
      val yFracOutAry   = new ArrayBuffer[BigInt]()
      val yOutAry       = new ArrayBuffer[BigInt]()
      val yAry          = new ArrayBuffer[BigInt]()

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

      val log2Table = { log2Up(c.lookupTableSize) }

      // Generate Table for linear interpolation
      val gradients   = new ArrayBuffer[BigInt]()
      val offsets     = new ArrayBuffer[BigInt]()
      // Fixed point increment
      val increment   = 1.0 / (1 << log2Table)
      val tableEnd    = 1.0
      var xtmp = 0.0
      // NOTE: x is positive, therefore gradient is negitive
      while (xtmp < tableEnd) {
        // m = (y1 - y2)/(x1 - x2)
        val m = -(scala.math.pow(2, - xtmp) - scala.math.pow(2,- xtmp - increment))/increment
        // convert to Fixed
        gradients += BigInt((m  * (1 << c.fracWidth)).toLong)
        // b = y1 - m*x1
        val b = scala.math.pow(2, - xtmp) - m*xtmp
        // convert to Fixed
        offsets += BigInt((b * (1 << c.fracWidth)).toLong)
        xtmp += increment
      }

      for ( cyc <- 0 until cycles ) {
        // generate random inputs
        val x          = BigInt(r.nextInt(1 << c.fracWidth))
        val gamma      = BigInt(r.nextInt(1 << c.fracWidth))
        val addToDict  = false //(r.nextInt(2) == 1)
        val xValAlt    = BigInt(r.nextInt(1 << c.fracWidth))
        val xintAlt    = BigInt(r.nextInt(1 << ((c.bitWidth - c.fracWidth)/2)))
        val xFracAlt   = BigInt(r.nextInt(1 << ((c.bitWidth - c.fracWidth)/2)))
        val gradTabAlt = BigInt(r.nextInt(1 << c.fracWidth))
        val offTabAlt  = BigInt(r.nextInt(1 << c.fracWidth))
        val xint1Alt   = BigInt(r.nextInt(1 << ((c.bitWidth - c.fracWidth)/2)))
        val limitAlt   = (r.nextInt(2) == 1)
        val gradAlt    = BigInt(r.nextInt(1 << c.fracWidth))
        val offAlt     = BigInt(r.nextInt(1 << c.fracWidth))
        val xint2Alt   = BigInt(r.nextInt(1 << (log2Up(c.fracWidth) - 1)))
        val yFracAlt   = BigInt(r.nextInt(1 << c.fracWidth))
        val yAlt       = BigInt(r.nextInt(1 << c.fracWidth))

        poke(c.io.x,          x)
        poke(c.io.gamma,      gamma)
        poke(c.io.addToDict,  Bool(addToDict).litValue())
        poke(c.io.xValAlt,    xValAlt)
        poke(c.io.xintAlt,    xintAlt)
        poke(c.io.xFracAlt,   xFracAlt)
        poke(c.io.gradTabAlt, gradTabAlt)
        poke(c.io.offTabAlt,  offTabAlt)
        poke(c.io.xint1Alt,   xint1Alt)
        poke(c.io.limitAlt,   Bool(limitAlt).litValue())
        poke(c.io.gradAlt,    gradAlt)
        poke(c.io.offAlt,     offAlt)
        poke(c.io.xint2Alt,   xint2Alt)
        poke(c.io.yFracAlt,   yFracAlt)
        poke(c.io.yAlt,       yAlt)

        xValOutAry += ((gamma*x) >> c.fracWidth)

        // calculate what the values should be
        val xIndex = ((xValOutAry(cyc) >> (c.fracWidth - log2Table)) & ( (1 << log2Table) - 1)).toInt
        val xIndexAlt = ((xValAlt >> (c.fracWidth - log2Table)) & ( (1 << log2Table) - 1)).toInt

        // m = (y1 - y2)/(x1 - x2)
        val m    = gradients(xIndex)
        val mAlt = gradients(xIndexAlt)
        // b = y1 - m*x1
        val b    = offsets(xIndex)
        val bAlt = offsets(xIndexAlt)

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
            mAlt
          } else {
            m
          }
        }
        offTabOutAry += {
          if ( c.stages(0) && addToDict) {
            bAlt
          } else {
            b
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
              BigInt(c.limitShift.toInt)
            else
              xint1Alt & c.limitShift.toInt
          } else {
            if ( limitOutAry(cyc) )
              BigInt(c.limitShift.toInt)
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
            yFracAlt >> xint2Alt.toInt
          } else {
            yFracOutAry(cyc) >> xint2OutAry(cyc).toInt
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
          expect(c.io.xValOut,    xValOutAry(cyc))
          expect(c.io.xintOut,    xintOutAry(cyc))
          expect(c.io.xFracOut,   xFracOutAry(cyc))
          expect(c.io.gradTabOut, gradTabOutAry(cyc))
          expect(c.io.offTabOut,  offTabOutAry(cyc))
          expect(c.io.xint1Out,   xint1OutAry(cyc))
          expect(c.io.limitOut,   Bool(limitOutAry(cyc)).litValue())
          expect(c.io.gradOut,    gradOutAry(cyc))
          expect(c.io.offOut,     offOutAry(cyc))
          expect(c.io.xint2Out,   xint2OutAry(cyc))
          expect(c.io.yFracOut,   yFracOutAry(cyc))
          expect(c.io.yOut,       yOutAry(cyc))
          expect(c.io.y,          yAry(cyc))
        }
        step(1)
      }

    }

    val myRand = new Random
    val fracWidth = myRand.nextInt(24) + 1
    val bitWidth = myRand.nextInt(24) + fracWidth + 4
    val lookupTableSize = 1 << (myRand.nextInt(8) + 1)
    val stages = ArrayBuffer.fill(5){ myRand.nextInt(2) == 1 }
    println("val fracWidth = " + fracWidth)
    println("val bitWidth = " + bitWidth)
    println("val stages = " + stages)
    println("val lookupTableSize = " + lookupTableSize)
    chiselMainTest(Array("--genHarness", "--compile", "--test", "--backend", "c"), () => {
      Module( new Pow2( bitWidth, fracWidth, stages, lookupTableSize ) )
    } ) { c => new Pow2Tests( c ) }
  }
}
