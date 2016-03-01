import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore
import scala.util.Random
import Chisel._
import OLK.Dict._

class DictSuite extends TestSuite {

  @Test def dictTests {

    class DictTests(c : Dict) extends Tester(c) {
      val one = BigInt(1 << c.fracWidth)
      val zero = BigInt(0)
      poke(c.io.alpha, zero)
      poke(c.io.forget, one)
      poke(c.io.addToDict, false)
      for (p <- 0 until c.pipelineStages){
        for (f <- 0  until c.features)
          poke(c.io.example(f), one + BigInt(p))
        step(1)
        for (p2 <- 0 until c.pipelineStages) {
          var x = zero
          if (p2 <= p)
            x = one  + BigInt(p - p2)
          for (f <- 0 until c.features)
            expect(c.io.currentPipeline(p2)(f), x)
        }
      }
      // Pipeline now full, test dict
      poke(c.io.alpha, one)      // 1
      poke(c.io.forget, one/2) // 0.5
      poke(c.io.addToDict, true)
      for (p <- 0 until c.pipelineStages) {
        var pVal = p
        if (p > (c.pipelineStages - 4)) {
          // in the last case test if the example is not added
          poke(c.io.addToDict, false)
          pVal = (c.pipelineStages - 4)
        }
        step(1)
        for (d <- 0 until (pVal+1)) {
          // Check weights
          var alphai = (one >> d)
          if (p > pVal && c.isNORMA)
            alphai = (alphai >> (p - pVal))
          expect(c.io.currentAlpha(d), alphai)
          // Check dictionary
          val x = (one + BigInt(pVal - d))
          for (f <- 0 until c.features) {
            expect(c.io.currentDict(d)(f), x)
          }
        }
      }
    }
    /*
    val myRand = new Random
    val fracWidth = myRand.nextInt(24) + 1
    val bitWidth = myRand.nextInt(24) + fracWidth
    val dictSize = myRand.nextInt(300) + 1
    val features = myRand.nextInt(32) + 1
    val stages = myRand.nextInt(20) + 2
     */
    val fracWidth = 9
    val bitWidth = 17
    val dictSize = 273
    val features = 18
    val stages = 10
    println("val fracWidth = " + fracWidth)
    println("val bitWidth = " + bitWidth)
    println("val dictSize = " + dictSize)
    println("val features = " + features)
    println("val stages = " + stages)
    chiselMainTest(Array("--genHarness", "--compile", "--test", "--backend", "c"), () => {
      Module( new Dict( bitWidth, fracWidth, dictSize, features, stages, true ) )
    } ) { c => new DictTests( c ) }
  }

}

