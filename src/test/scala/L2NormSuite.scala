import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import Chisel._
import OLK.Kernel._

class L2NormSuite extends TestSuite {

  @Test def l2NormTest {
    class L2NormTests(c : L2Norm) extends Tester(c) {
      def toFixed(x : Double, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
      def toFixed(x : Float, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
      def toFixed(x : Int, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
      val r = scala.util.Random

      var noStages = 0
      for (i <- 0 until (c.stages.length))
        noStages = { if (c.stages(i)) { noStages + 1 } else { noStages } }

      val cycles = 10*noStages

      val expectResult = ArrayBuffer.fill(noStages)(0)
      val subRes = new ArrayBuffer[ArrayBuffer[Int]]()
      val sqrRes = new ArrayBuffer[ArrayBuffer[Int]]()
      if (c.stages(0)) {
        val sqrary = new ArrayBuffer[Int]()
        for (i <- 0 until c.features)
          sqrary += 0
        sqrRes += sqrary
      }
      // adderRes = stage[cycle[feature]]
      val adderRes = new ArrayBuffer[ArrayBuffer[ArrayBuffer[Int]]]()
      // add each stage
      for (i <- 0 until c.log2Features)
        adderRes += new ArrayBuffer[ArrayBuffer[Int]]()
      // compute cycles that are 0 in advance
      for (s <- 2 until c.stages.length) {
        if (c.stages(s - 1))
          adderRes(s - 2) += ArrayBuffer.fill(1 << (c.log2Features - 1 - (s - 2))){0}
      }

      for (cyc <- 0 until cycles) {
        val x1 = ArrayBuffer.fill(c.features)(r.nextInt(1 << (c.bitWidth/2)))
        val x2 = ArrayBuffer.fill(c.features)(r.nextInt(1 << (c.bitWidth/2)))
        val subalt = ArrayBuffer.fill(c.features)(r.nextInt(1 << (c.bitWidth/2)))
        val sqralt = ArrayBuffer.fill(c.features)(r.nextInt(1 << (c.bitWidth/2)))
        val adderalt  = ArrayBuffer.fill((1 << c.log2Features) - 1){r.nextInt(1 << (c.bitWidth/2))}
        val addToDict = (r.nextInt(2) == 1)
        poke(c.io.addToDict, Bool(addToDict).litValue())
        for (i <- 0 until adderalt.length)
          poke(c.io.adderalt(i), BigInt(adderalt(i)))
        for (i <- 0 until c.features) {
          poke(c.io.sqralt(i), BigInt(sqralt(i)))
          poke(c.io.subalt(i), BigInt(subalt(i)))
        }

        var sum = 0
        val subary = new ArrayBuffer[Int]()
        val sqrary = new ArrayBuffer[Int]()
        val sqrofalt = new ArrayBuffer[Int]()
        var sumalt = 0
        for (i <- 0 until c.features) {
          val tmp = (x1(i) - x2(i))
          subary += tmp
          val sqr = ((tmp*tmp) >> c.fracWidth)
          sqrary += sqr
          val sqr2 = (subalt(i)*subalt(i)) >> c.fracWidth
          sqrofalt += sqr2
          sum =  sum + sqr
          sumalt = sumalt + sqr2
          poke(c.io.x1(i), BigInt(x1(i)))
          poke(c.io.x2(i), BigInt(x2(i)))
        }
        subRes += subary
        if (addToDict && c.stages(0)) {
          sqrRes += sqrofalt
          expectResult += sumalt
        } else {
          sqrRes += sqrary
          expectResult += sum
        }
        if (addToDict) {
          // the last cycle has been added, correct the next ones for the sum
          // c.stages(end) forced true in implementation
          var tmpCyc = 0
          for (i <- 0 until (c.stages.length - 2)){
            if (c.stages(c.stages.length - 1 - i)) {
              var adderaltSum = 0
              for (j <- ((1 << i) - 1) until ((1 << (i+1)) - 1))
                adderaltSum += adderalt(adderalt.length - 1 - j)
              expectResult(cyc + 1 + tmpCyc) = adderaltSum
              tmpCyc += 1
            }
          }
          if (c.stages(1))
            expectResult(cyc + 1 + tmpCyc) = sqralt.sum
        }

        var adderStageCumSum = 0
        for (stage <- 0 until c.log2Features) {
          val adderFeatures = new ArrayBuffer[Int]()
          val adderStageSize = 1 << (c.log2Features - 1 - stage)
          for (f <- 0 until adderStageSize) {
            if (stage == 0) {
              if (addToDict && c.stages(1)) {
                // change adderRes based on new alt inputs
                adderFeatures += { if ( sqralt.length > 2*f + 1 ) {sqralt(2*f) + sqralt(2*f + 1)} else { sqralt(2*f) } }
              } else
                adderFeatures += { if ( sqrRes(cyc).length > 2*f + 1 ) { sqrRes(cyc)(2*f) + sqrRes(cyc)(2*f + 1) } else sqrRes(cyc)(2*f) }
            }
            else {
              if (addToDict && c.stages(stage + 1)) {
                // change adderRes based on new alt inputs
                adderFeatures += (adderalt(2*f + adderStageCumSum) + adderalt(2*f + adderStageCumSum + 1))
              } else
                adderFeatures += (adderRes(stage - 1)(cyc)(2*f) + adderRes(stage - 1)(cyc)(2*f + 1))
            }
          }
          if (stage != 0)
            adderStageCumSum += 2*adderStageSize
          // add the result to the next cycle
          adderRes(stage) += adderFeatures
        }

        for (i <- 0 until c.features) {
          expect(c.io.subout(i), BigInt(subRes(cyc)(i)))
          expect(c.io.sqrout(i), BigInt(sqrRes(cyc)(i)))
        }
        var adderPos = 0
        for (i <- 0 until c.log2Features) {
          for (j <- 0 until (1 << (c.log2Features - 1 - i))) {
            expect(c.io.adderout(adderPos), BigInt(adderRes(i)(cyc)(j)))
            adderPos = adderPos + 1
          }
        }

        expect(c.io.result, BigInt(expectResult(cyc)))
        step(1)
      }
    }
    val myRand = new Random
    val fracWidth = myRand.nextInt(24) + 1
    val bitWidth = myRand.nextInt(24) + fracWidth
    val features = 1 << (myRand.nextInt(5) + 1)
    val stages = ArrayBuffer.fill((scala.math.log10(features)/scala.math.log10(2)).ceil.toInt + 2){ myRand.nextInt(2) == 1 }
    println("val fracWidth = " + fracWidth)
    println("val bitWidth = " + bitWidth)
    println("val features = " + features)
    println("val stages = " + stages)

    chiselMainTest(Array("--genHarness", "--compile", "--test", "--backend", "c"), () => {
      Module( new L2Norm( bitWidth, fracWidth, stages, features ) )
    } ) { c => new L2NormTests( c ) }

  }
}
