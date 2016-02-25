import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import Chisel._
import OLK._
import OLK.Sum._

class SumRSuite extends TestSuite {

  @Test def sumRTests {

    class SumRTests(c : SumR) extends Tester(c) {
      val lnOf2 = scala.math.log(2)
      def log2(x : Int) : Int = (scala.math.log(x.toDouble) / lnOf2).toInt
      def toFixed(x : Double, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
      def toFixed(x : Float, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
      def toFixed(x : Int, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
      val r = scala.util.Random

      val cycles = 3*(c.activeStages + 2)

      val addToDicts = ArrayBuffer.fill(cycles + c.activeStages + 1){ r.nextInt(2) == 1}
      val sumRAry = ArrayBuffer.fill(c.activeStages - 1){0}
      val wDAry   = ArrayBuffer.fill(c.activeStages - 1){0}
      val wD1Ary  = ArrayBuffer.fill(c.activeStages - 1){0}
      
      for (cyc <- 0 until cycles) {
        val inVI     = ArrayBuffer.fill(c.dictionarySize){r.nextInt(1 << c.fracWidth)}
        val inAlphaI = ArrayBuffer.fill(c.dictionarySize){r.nextInt(1 << c.fracWidth)}

        val wi = (inVI zip inAlphaI).map(pair => { (pair._1 * pair._2) >> c.fracWidth })
        val totalAdded = addToDicts.drop(cyc).take(c.activeStages).count(_ == true)

        sumRAry += wi.dropRight(totalAdded + 2).sum
        wDAry  += wi(wi.length - 1 - totalAdded)
        wD1Ary += wi(wi.length - 2 - totalAdded)

        poke(c.io.addToDict, Bool(addToDicts(cyc)).litValue())
        for (i <- 0 until c.dictionarySize) {
          poke(c.io.vi(i), BigInt(inVI(i)))
          poke(c.io.alphai(i), BigInt(inAlphaI(i)))
        }

        step(1)
        if (cyc >= c.activeStages - 1) {
          expect(c.io.sumR, BigInt(sumRAry(cyc)))
          expect(c.io.wD, BigInt(wDAry(cyc)))
          expect(c.io.wD1, BigInt(wD1Ary(cyc)))
        }
      }
    }

    val myRand = new Random
    val fracWidth = myRand.nextInt(24) + 1
    val bitWidth = myRand.nextInt(24) + fracWidth + 4
    val dictionarySize = myRand.nextInt(250) + 8
    val stages = Top.generateSumRStages(dictionarySize)
    println("val fracWidth = " + fracWidth)
    println("val bitWidth = " + bitWidth)
    println("val dictionarySize = " + dictionarySize)
    println("val stages = " + stages)
    chiselMainTest(Array("--genHarness", "--compile", "--test", "--backend", "c"), () => {
      Module( new SumR( bitWidth, fracWidth, dictionarySize, stages ) )
    } ) { c => new SumRTests( c ) }

  }

}
