import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import Chisel._
import OLK._
import OLK.Sum._

class SumStageSuite extends TestSuite {

  @Test def sumStageTests {

    class SumStageTests(c : SumStage) extends Tester(c) {
      def toFixed(x : Double, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
      def toFixed(x : Float, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
      def toFixed(x : Int, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
      val r = scala.util.Random

      val activeStages = c.stages.count(_ == true)

      val cycles     = 2*c.stages.length
      val expectSum  = ArrayBuffer.fill(activeStages - 1)( BigInt(0) )
      val expectzp   = ArrayBuffer.fill(activeStages - 1)( BigInt(0) )
      val expectzp1  = ArrayBuffer.fill(activeStages - 1)( BigInt(0) )
      val expectwD   = ArrayBuffer.fill(activeStages - 1)( BigInt(0) )
      val expectwD1  = ArrayBuffer.fill(activeStages - 1)( BigInt(0) )
      val sumLAry   = ArrayBuffer.fill(activeStages - 1)( BigInt(0) )
      val sumRAry   = ArrayBuffer.fill(activeStages - 1)( BigInt(0) )
      val wd1Ary    = ArrayBuffer.fill(activeStages - 1)( BigInt(0) )
      val addToDicts = ArrayBuffer.fill(cycles + activeStages){ r.nextInt(2) == 1}
      val alpha      = ArrayBuffer.fill(cycles + activeStages){ BigInt( r.nextInt(1 << ((2*c.fracWidth)/3)) ) }
      val forget     = BigInt( r.nextInt(1 << c.fracWidth) )
      poke(c.io.forget, forget)
      poke( c.io.forceNA, false )

      for (cyc <- 0 until cycles) {
        val alphai = ArrayBuffer.fill(c.dictSize)( BigInt( r.nextInt(1 << ((2*c.fracWidth)/3)) ) )
        val zi     = ArrayBuffer.fill(activeStages + 1)( BigInt( r.nextInt(1 << ((2*c.fracWidth)/3)) ) )
        val vi     = ArrayBuffer.fill(c.dictSize)( BigInt( r.nextInt(1 << ((2*c.fracWidth)/3)) ) )

        val forgetPowQ = {
          var sum = BigInt( 1 << c.fracWidth )
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

        var sumL = BigInt(0)
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
            (wd*forgetPowQ1) >> c.fracWidth
        }
        expectwD1 += wD1
        expectSum += {
          if ( addToDicts(cyc + activeStages - 1) ) {
            ((forget*sumL) >> c.fracWidth) + ((forgetPowQ1*sumR) >> c.fracWidth) +
            ((alpha(cyc + activeStages - 1)*expectzp1(cyc + activeStages - 1)) >> c.fracWidth)
          } else
            if ( c.isNORMA )
            ((sumL*forget) >> c.fracWidth) + ((forgetPowQ1*(sumR + expectwD1(cyc + activeStages - 1))) >> c.fracWidth)
          else
            sumL + ((forgetPowQ*(sumR + expectwD1(cyc + activeStages - 1))) >> c.fracWidth)
        }

        poke(c.io.addToDict, addToDicts(cyc) )
        poke(c.io.alpha, alpha(cyc) )
        poke(c.io.alphai, alphai.toArray )
        poke(c.io.vi, vi.toArray )
        poke(c.io.zi, zi.toArray)

        step(1)
        if ( cyc > 2 ) {
          expect(c.io.sum, expectSum(cyc) )
          expect(c.io.zp, expectzp(cyc) )
          expect(c.io.wD, expectwD(cyc) )
        }
      }
    }

    val myRand = new Random
    val fracWidth = myRand.nextInt(24) + 1
    val bitWidth = myRand.nextInt(24) + fracWidth + 4
    val dictionarySize = myRand.nextInt(250) + 8
    val stages = Top.generateSumRStages(dictionarySize)
    stages.append(true)
    println("val fracWidth = " + fracWidth)
    println("val bitWidth = " + bitWidth)
    println("val dictionarySize = " + dictionarySize)
    println("val stages = " + stages)
    chiselMainTest(Array("--genHarness", "--compile", "--test", "--backend", "c"), () => {
      Module( new SumStage( bitWidth, fracWidth, stages, dictionarySize, true ) )
    } ) { c => new SumStageTests( c ) }

  }

}
