import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import Chisel._
import OLK.Sum._

class SumLSuite extends TestSuite {

  @Test def sumLTest {

    class SumLTests(c : SumL) extends Tester(c) {

      val r = scala.util.Random

      val cycles = 3*(c.stages + 1)
      val alpha = ArrayBuffer.fill(cycles + c.stages){ BigInt(r.nextInt(1 << ((2*c.bitWidth)/3))) }
      val addToDicts = ArrayBuffer.fill(cycles + c.stages){ r.nextInt(2) == 1 }
      val forceNAs = ArrayBuffer.fill(cycles + c.stages){ r.nextInt(5) == 1 }
      val forget = BigInt(r.nextInt(1 << c.fracWidth))

      val expectedZp1  = ArrayBuffer.fill(c.stages - 1){ BigInt(0) }
      val expectedZp   = ArrayBuffer.fill(c.stages - 1){ BigInt(0) }
      val expectedSumL = ArrayBuffer.fill(c.stages - 1){ BigInt(0) }
      val expectedQ    = ArrayBuffer.fill(c.stages - 1){ BigInt(0) }
      val expectedQ1   = ArrayBuffer.fill(c.stages - 1){ BigInt(0) }

      poke(c.io.forget, forget)

      for ( cyc <- 0 until cycles ) {
        var sum = BigInt(1 << c.fracWidth)
        for ( i <- 0 until (c.stages - 1) ) {
          if ( !forceNAs(cyc + i) ) {
            sum = (sum*forget) >> c.fracWidth
          }
        }
        var sumQ1 = sum
        if ( !forceNAs(cyc + c.stages - 1) ) {
          sum = (sum*forget) >> c.fracWidth
          sumQ1 = ((forget*forget) >> c.fracWidth)*sumQ1 >> c.fracWidth
        } else
          sumQ1 = (sum*forget) >> c.fracWidth

        expectedQ += sum
        expectedQ1 += sumQ1

        val z = ArrayBuffer.fill(c.stages + 2){ BigInt(r.nextInt(1 << ((2*c.bitWidth)/3))) }

        expectedZp1 += z(1)
        expectedZp  += z(0)

        // multiply all z values with alphas
        val mult = (z.drop(2).reverse zip alpha.drop(cyc).take(c.stages)).map( pair => { (pair._1 * pair._2) >> c.fracWidth } )
        var sumL = BigInt(0)
        for ( i <- 0 until mult.length ) {
          var tmp = mult(i)
          if ( !forceNAs(cyc + i) ) {
            sumL = (forget*sumL) >> c.fracWidth
            if ( addToDicts(cyc + i) )
              sumL = sumL + tmp
          }
        }
        expectedSumL += sumL

        poke(c.io.alpha, alpha(cyc))
        poke(c.io.addToDict, addToDicts(cyc))
        poke(c.io.forceNA, forceNAs(cyc))

        for (s <- 0 until (c.stages + 2))
          poke(c.io.z(s), z(s))

        step(1)

        expect(c.io.zp1, expectedZp1(cyc))
        expect(c.io.zp, expectedZp(cyc))
        expect(c.io.sumL, expectedSumL(cyc))
        if ( cyc >= c.stages - 1) {
          expect(c.io.forgetPowQ, expectedQ(cyc))
          expect(c.io.forgetPowQ1, expectedQ1(cyc))
        }
      }
    }

    val myRand = new Random
    val fracWidth = myRand.nextInt(24) + 1
    val bitWidth = myRand.nextInt(24) + fracWidth + 4
    val stages = myRand.nextInt(50) + 2
    val isNORMA = true
    println("val fracWidth = " + fracWidth)
    println("val bitWidth = " + bitWidth)
    println("val stages = " + stages)
    println("val isNORMA = " + isNORMA)
    chiselMainTest(Array("--genHarness", "--compile", "--test", "--backend", "c"), () => {
      Module( new SumL( bitWidth, fracWidth, stages, isNORMA ) )
    } ) { c => new SumLTests( c ) }

  }

}
