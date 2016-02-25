import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import Chisel._
import OLK.NORMAStage._

class NormaStageSuite extends TestSuite {

  def normaCStep( rhoOld : BigInt, y : BigInt, ft : BigInt, bOld : BigInt, eta : BigInt,
    etanu1 : BigInt, etanu : BigInt, bitWidth : Int ) : (Boolean, BigInt, BigInt) = {
    val testCond = rhoOld - y*(ft + bOld)
    // dodgy hack incase overflowed into sign bit
    if (testCond >= BigInt(0) && !( testCond > (1 << (bitWidth-1)) ) )
      ( true, bOld + y*eta, rhoOld + etanu1 )
    else
      ( false, bOld, rhoOld + etanu)
  }

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

        val normaStep = normaCStep( rhoOld, y, ft, bOld, eta,
          etanu1, etanu, c.bitWidth )

        poke(c.io.ft, ft)
        poke(c.io.rhoOld, rhoOld)
        poke(c.io.bOld, bOld)
        poke(c.io.y, Bool(y == 1).litValue())
        poke(c.io.etapos, etapos)
        poke(c.io.etaneg, etaneg)
        poke(c.io.etanu, etanu)
        poke(c.io.etanu1, etanu1)
        expect(c.io.addToDict, normaStep._1)
        expect(c.io.bNew, normaStep._2)
        expect(c.io.rhoNew, normaStep._3)
      }
    }

    val myRand = new Random
    val fracWidth = myRand.nextInt(24) + 1
    val bitWidth = scala.math.min(myRand.nextInt(24) + fracWidth + 4, 31)
    println("val fracWidth = " + fracWidth)
    println("val bitWidth = " + bitWidth)
    chiselMainTest(Array("--genHarness", "--compile", "--test", "--backend", "c"), () => {
      Module( new NORMAc( bitWidth, fracWidth ) )
    } ) { c => new NORMAcTests( c ) }

  }

  def normaNStep( rhoOld : BigInt, ft : BigInt, etanu1 : BigInt, etanu : BigInt, bitWidth : Int ) : (Boolean, BigInt) = {
    val testCond = rhoOld - ft
    // dodgy hack incase overflowed into sign bit
    if (testCond >= 0 && !( testCond > (1 << (bitWidth-1)) ) )
      ( true, rhoOld + etanu1 )
    else
      ( false, rhoOld + etanu )
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

        val normaStep = normaNStep( rhoOld, ft, etanu1, etanu, c.bitWidth )

        poke(c.io.ft, ft)
        poke(c.io.rhoOld, rhoOld)
        poke(c.io.etanu, etanu)
        poke(c.io.etanu1, etanu1)
        expect(c.io.addToDict, normaStep._1 )
        expect(c.io.rhoNew, normaStep._2 )
      }
    }
    val myRand = new Random
    val fracWidth = myRand.nextInt(24) + 1
    val bitWidth = scala.math.min(myRand.nextInt(24) + fracWidth + 4, 31)
    println("val fracWidth = " + fracWidth)
    println("val bitWidth = " + bitWidth)
    chiselMainTest(Array("--genHarness", "--compile", "--test", "--backend", "c"), () => {
      Module( new NORMAn( bitWidth, fracWidth ) )
    } ) { c => new NORMAnTests( c ) }

  }

  def normaRStep( rhoOld : BigInt, y : BigInt, ft : BigInt, etanu1 : BigInt,
    etanu : BigInt, bitWidth : Int ) : (Boolean, BigInt, Boolean) = {
    val isPos = (y > ft)
    val testCond = { if ( isPos ) (y - ft - rhoOld) else (ft - y - rhoOld) }
    // dodgy hack incase overflowed into sign bit
    if (testCond >= 0 && !( testCond > (1 << (bitWidth-1)) ) )
      ( true, rhoOld - etanu1, isPos )
    else
      ( false, rhoOld - etanu, isPos )
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

        val normaStep = normaRStep( rhoOld, y, ft, etanu1, etanu, c.bitWidth )

        poke(c.io.ft, ft)
        poke(c.io.y, y)
        poke(c.io.rhoOld, rhoOld)
        poke(c.io.etanu, etanu)
        poke(c.io.etanu1, etanu1)
        expect(c.io.addToDict, normaStep._1)
        expect(c.io.rhoNew, normaStep._2)
        expect(c.io.sign, normaStep._3)
      }
    }

    val myRand = new Random
    val fracWidth = myRand.nextInt(24) + 1
    val bitWidth = scala.math.min(myRand.nextInt(24) + fracWidth + 4, 31)
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
        poke(c.io.forceNA, forceNA)
        if (c.NORMAtype == 1) {
          val c_C = c.io.asInstanceOf[IOBundle_C]
          poke(c_C.yC, yC == 1)
        }
        if (c.NORMAtype == 3) {
          val c_R = c.io.asInstanceOf[IOBundle_R]
          poke(c_R.yReg, yReg)
        }
        poke(c.io.etanu, etanu)
        poke(c.io.etanu1, etanu1)
        poke(c.io.etapos, etapos)
        poke(c.io.etaneg, etaneg)

        val sumForceNA = { if ( forceNA ) sum else ((forget*sum) >> c.fracWidth) }
        val wdForceNA = { if ( forceNA ) wD else ((forget*wD) >> c.fracWidth) }
        var ft = sumForceNA + wdForceNA
        if (addToDict) {
          val tmpA = (alpha * zp) >> c.fracWidth
          val tmpB = sumForceNA
          ft = (tmpA + tmpB)
        }
        // compute expected alpha and addToDict
        addToDict = false
        alpha = BigInt(0)
        if (c.NORMAtype == 1) {
          val normaStep = normaCStep( rhoOld, yC, ft, bOld, eta,
            etanu1, etanu, c.bitWidth )
          alpha = yC*eta
          addToDict = normaStep._1
          b = normaStep._2
          rho = normaStep._3
        } else if (c.NORMAtype == 2) {
          val normaStep = normaNStep( rhoOld, ft, etanu1, etanu, c.bitWidth )
          alpha = eta
          addToDict = normaStep._1
          rho = normaStep._2
        } else {
          val normaStep = normaRStep( rhoOld, yReg, ft, etanu1, etanu, c.bitWidth )
          addToDict = normaStep._1
          rho = normaStep._2
          if ( normaStep._3 )
            alpha = etapos
          else
            alpha = etaneg
        }
        // ft is different for novelty
        if (c.NORMAtype == 2)
          ft = ft - rhoOld
        if (forceNA){
          addToDict = false
          rho = rhoOld
          b = bOld
        }
        println("rhoOld = " + rhoOld + ", rho = " + rho)
        rhoOld = rho
        bOld = b

        // Clock and read outputs
        step(1)
        expect(c.io.addToDict, addToDict)
        expect(c.io.ft, ft)
        expect(c.io.alpha, alpha)
      }
    }

    val myRand = new Random
    val fracWidth = myRand.nextInt(24) + 1
    val bitWidth = scala.math.min(myRand.nextInt(24) + fracWidth + 4, 31)
    val NORMAType = myRand.nextInt(3) + 1
    println("val fracWidth = " + fracWidth)
    println("val bitWidth = " + bitWidth)
    println("val NORMAType = " + NORMAType)
    chiselMainTest(Array("--genHarness", "--compile", "--test", "--backend", "c"), () => {
      Module( new NORMAStage( bitWidth, fracWidth, NORMAType ) )
    } ) { c => new NORMAStageTests( c ) }

  }
}
