import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import Chisel._
import OLK.NORMAStage._

class NormaStageSuite extends TestSuite {

  @Test def normaCTest {

    class NORMAcTests(c: NORMAc) extends Tester(c) {
      def toFixed(x : Double, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
      def toFixed(x : Float, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
      def toFixed(x : Int, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
      val r = scala.util.Random

      for (i <- 0 until 10) {
        val ft     = BigInt(r.nextInt(1 << (c.bitWidth-2)))
        val rhoOld = BigInt(r.nextInt(1 << (c.bitWidth-2)))
        val bOld   = BigInt(r.nextInt(1 << (c.bitWidth-2)))
        val eta    = BigInt(r.nextInt(1 << (c.bitWidth/2)))
        val nu     = BigInt(r.nextInt(1 << (c.bitWidth/2)))
        val y      = BigInt(r.nextInt(2)*2 - 1)
        val etapos = eta
        val etaneg = -eta
        val etanu  = ((eta*nu) >> c.fracWidth)
        val etanu1 = -((eta*(toFixed(1, c.fracWidth) - nu)) >> c.fracWidth)

        var testCond = rhoOld - y*(ft + bOld)
        var bNew = bOld
        var rhoNew = rhoOld + etanu
        var addToDict = Bool(false)
        // dodgy hack incase overflowed into sign bit
        if (testCond > (1 << (c.bitWidth-1)))
          testCond = BigInt(-1)
        if (testCond > BigInt(0)) {
          bNew = bOld + y*eta
          rhoNew = rhoOld + etanu1
          addToDict = Bool(true)
        }
        poke(c.io.ft, ft)
        poke(c.io.rhoOld, rhoOld)
        poke(c.io.bOld, bOld)
        poke(c.io.y, Bool(y == 1).litValue())
        poke(c.io.etapos, etapos)
        poke(c.io.etaneg, etaneg)
        poke(c.io.etanu, etanu)
        poke(c.io.etanu1, etanu1)
        expect(c.io.addToDict, addToDict.litValue())
        expect(c.io.bNew, bNew)
        expect(c.io.rhoNew, rhoNew)
      }
    }

    val myRand = new Random
    val fracWidth = myRand.nextInt(24) + 1
    val bitWidth = myRand.nextInt(24) + fracWidth + 4
    println("val fracWidth = " + fracWidth)
    println("val bitWidth = " + bitWidth)
    chiselMainTest(Array("--genHarness", "--compile", "--test", "--backend", "c"), () => {
      Module( new NORMAc( bitWidth, fracWidth ) )
    } ) { c => new NORMAcTests( c ) }

  }

  @Test def normaN {
    class NORMAnTests(c: NORMAn) extends Tester(c) {
      def toFixed(x : Double, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
      def toFixed(x : Float, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
      def toFixed(x : Int, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
      val r = scala.util.Random

      for (i <- 0 until 10) {
        val ft     = BigInt(r.nextInt(1 << (c.bitWidth-2)))
        val rhoOld = BigInt(r.nextInt(1 << (c.bitWidth-2)))
        val eta    = BigInt(r.nextInt(1 << (c.bitWidth/2)))
        val nu     = BigInt(r.nextInt(1 << (c.bitWidth/2)))
        val etanu  = ((eta*nu) >> c.fracWidth)
        val etanu1 = -((eta*(toFixed(1, c.fracWidth) - nu)) >> c.fracWidth)

        var testCond = rhoOld - ft
        // dodgy hack incase overflowed into sign bit
        if (testCond > (1 << (c.bitWidth-1)))
          testCond = BigInt(-1)
        var rho = rhoOld + etanu
        var addToDict = false
        if (testCond > 0) {
          addToDict = true
          rho = rhoOld + etanu1
        }
        poke(c.io.ft, ft)
        poke(c.io.rhoOld, rhoOld)
        poke(c.io.etanu, etanu)
        poke(c.io.etanu1, etanu1)
        expect(c.io.addToDict, Bool(addToDict).litValue())
        expect(c.io.rhoNew, rho)
      }
    }
    val myRand = new Random
    val fracWidth = myRand.nextInt(24) + 1
    val bitWidth = myRand.nextInt(24) + fracWidth + 4
    println("val fracWidth = " + fracWidth)
    println("val bitWidth = " + bitWidth)
    chiselMainTest(Array("--genHarness", "--compile", "--test", "--backend", "c"), () => {
      Module( new NORMAn( bitWidth, fracWidth ) )
    } ) { c => new NORMAnTests( c ) }

  }

  @Test def normaR {

    class NORMArTests(c: NORMAr) extends Tester(c) {
      def toFixed(x : Double, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
      def toFixed(x : Float, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
      def toFixed(x : Int, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
      val r = scala.util.Random

      for (i <- 0 until 10) {
        val ft     = BigInt(r.nextInt(1 << (c.bitWidth-2)))
        val y      = BigInt(r.nextInt(1 << (c.bitWidth-2)))
        val rhoOld = BigInt(r.nextInt(1 << (c.bitWidth-2)))
        val eta    = BigInt(r.nextInt(1 << (c.bitWidth/2)))
        val nu     = BigInt(r.nextInt(1 << (c.bitWidth/2)))
        val etanu  = ((eta*nu) >> c.fracWidth)
        val etanu1 = -((eta*(toFixed(1, c.fracWidth) - nu)) >> c.fracWidth)

        val isPos = (y > ft)
        var testCond = ft - y - rhoOld
        if (isPos)
          testCond = y - ft - rhoOld
        // dodgy hack incase overflowed into sign bit
        if (testCond > (1 << (c.bitWidth-1)))
          testCond = BigInt(-1)
        var rho = rhoOld - etanu
        var addToDict = false
        if (testCond > 0) {
          addToDict = true
          rho = rhoOld - etanu1
        }
        poke(c.io.ft, ft)
        poke(c.io.y, y)
        poke(c.io.rhoOld, rhoOld)
        poke(c.io.etanu, etanu)
        poke(c.io.etanu1, etanu1)
        expect(c.io.addToDict, Bool(addToDict).litValue())
        expect(c.io.rhoNew, rho)
        expect(c.io.sign, Bool(isPos).litValue())
      }
    }

    val myRand = new Random
    val fracWidth = myRand.nextInt(24) + 1
    val bitWidth = myRand.nextInt(24) + fracWidth + 4
    println("val fracWidth = " + fracWidth)
    println("val bitWidth = " + bitWidth)
    chiselMainTest(Array("--genHarness", "--compile", "--test", "--backend", "c"), () => {
      Module( new NORMAr( bitWidth, fracWidth ) )
    } ) { c => new NORMArTests( c ) }

  }

  @Test def normaStageTests {

    class NORMAStageTests(c: NORMAStage) extends Tester(c) {
      def toFixed(x : Double, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
      def toFixed(x : Float, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
      def toFixed(x : Int, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
      val r = scala.util.Random

      // internal Registers
      var rho = BigInt(0)
      var b = BigInt(0)
      var rhoOld = BigInt(0)
      var bOld = BigInt(0)
      var alpha = BigInt(0)
      var addToDict = false

      for (i <- 0 until 30) {
        // Generate inputs
        val sum    = BigInt(r.nextInt(1 << (c.bitWidth/2)))
        val zp     = BigInt(r.nextInt(1 << (c.bitWidth/2)))
        val wD     = BigInt(r.nextInt(1 << (c.bitWidth/2)))
        val forget = BigInt(r.nextInt(1 << (c.fracWidth)))

        val yC     = (r.nextInt(2)*2) - 1
        val forceNA = (r.nextInt(5) == 1)
        val yReg   = BigInt(r.nextInt(1 << (c.bitWidth/2)))
        val eta    = BigInt(r.nextInt(1 << (c.bitWidth/2)))
        val nu     = BigInt(r.nextInt(1 << (c.bitWidth/2)))
        val etanu  = ((eta*nu) >> c.fracWidth)
        val etanu1 = -((eta*(toFixed(1, c.fracWidth) - nu)) >> c.fracWidth)
        val etapos = eta
        val etaneg = -eta

        poke(c.io.sum, sum)
        poke(c.io.zp, zp)
        poke(c.io.wD, wD)
        poke(c.io.forget, forget)
        poke(c.io.forceNA, Bool(forceNA).litValue())
        if (c.NORMAtype == 1) {
          val c_C = c.io.asInstanceOf[IOBundle_C]
          poke(c_C.yC, Bool(yC == 1).litValue())
        }
        if (c.NORMAtype == 3) {
          val c_R = c.io.asInstanceOf[IOBundle_R]
          poke(c_R.yReg, yReg)
        }
        poke(c.io.etanu, etanu)
        poke(c.io.etanu1, etanu1)
        poke(c.io.etapos, etapos)
        poke(c.io.etaneg, etaneg)

        var ft = ((forget*sum) >> c.fracWidth) + ((forget*wD) >> c.fracWidth)
        if (addToDict) {
          val tmpA = (alpha * zp) >> c.fracWidth
          val tmpB = (forget * sum) >> c.fracWidth
          ft = (tmpA + tmpB)
        }
        val ftNov = ft - rho
        // compute expected alpha and addToDict
        addToDict = false
        alpha = BigInt(0)
        if (c.NORMAtype == 1) {
          var testCond = rho - yC*(ft + b)
          // dodgy hack incase overflowed into sign bit
          if (testCond > (1 << (c.bitWidth-1)))
            testCond = BigInt(-1)
          alpha = yC*eta
          if (testCond > BigInt(0)) {
            b = b + yC*eta
            rho = rho + etanu1
            addToDict = true
          } else
            rho = rho + etanu
        } else if (c.NORMAtype == 2) {
          var testCond = rho - ft
          // dodgy hack incase overflowed into sign bit
          if (testCond > (1 << (c.bitWidth-1)))
            testCond = BigInt(-1)
          alpha = eta
          if (testCond > 0) {
            addToDict = true
            rho = rho + etanu1
          } else
            rho = rho + etanu
        } else {
          val isPos = (yReg > ft)
          var testCond = {
            if (isPos)
              yReg - ft - rho
            else
              ft - yReg - rho
          }
          if (isPos)
            alpha = etapos
          else
            alpha = etaneg
          if (testCond > (1 << (c.bitWidth-1)))
            testCond = BigInt(-1)
          if (testCond > 0) {
            addToDict = true
            rho = rho - etanu1
          } else
            rho = rho - etanu
        }
        // ft is different for novelty
        if (c.NORMAtype == 2)
          ft = ftNov
        if (forceNA){
          addToDict = false
          rho = rhoOld
          b = bOld
        }
        rhoOld = rho
        bOld = b

        // Clock and read outputs
        step(1)
        expect(c.io.addToDict, Bool(addToDict).litValue())
        expect(c.io.ft, ft)
        expect(c.io.alpha, alpha)
      }
    }

    val myRand = new Random
    val fracWidth = myRand.nextInt(24) + 1
    val bitWidth = myRand.nextInt(24) + fracWidth + 4
    val NORMAType = myRand.nextInt(3) + 1
    println("val fracWidth = " + fracWidth)
    println("val bitWidth = " + bitWidth)
    println("val NORMAType = " + NORMAType)
    chiselMainTest(Array("--genHarness", "--compile", "--test", "--backend", "c"), () => {
      Module( new NORMAStage( bitWidth, fracWidth, NORMAType ) )
    } ) { c => new NORMAStageTests( c ) }

  }
}
