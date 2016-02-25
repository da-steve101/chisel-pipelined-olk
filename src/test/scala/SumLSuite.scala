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
      val alpha = ArrayBuffer.fill(cycles + c.stages){ r.nextInt(1 << ((2*c.bitWidth)/3)) }
      val addToDicts = ArrayBuffer.fill(cycles + c.stages){ r.nextInt(2) == 1 }
      val forget = r.nextInt(1 << ((2*c.bitWidth)/3))

      poke(c.io.forget, BigInt(forget))

      val expectedZp1  = ArrayBuffer.fill(c.stages - 1){ 0 }
      val expectedZp   = ArrayBuffer.fill(c.stages - 1){ 0 }
      val expectedSumL = ArrayBuffer.fill(c.stages - 1){ 0 }
      val expectedQ    = ArrayBuffer.fill(c.stages - 1){ 0 }
      val expectedQ1   = ArrayBuffer.fill(c.stages - 1){ 0 }

      for ( cyc <- 0 until cycles ) {
        expectedQ += {
          var sum = 1 << c.fracWidth
          if ( c.isNORMA ) {
            for ( i <- 0 until c.stages )
              sum = (sum*forget) >> c.fracWidth
          } else {
            for ( i <- 0 until c.stages ) {
              if ( addToDicts(cyc + i) )
                sum = (sum*forget) >> c.fracWidth
            }
          }
          sum
        }
        expectedQ1 += (forget*expectedQ.last) >> c.fracWidth

        val z = ArrayBuffer.fill(c.stages + 2){ r.nextInt(1 << ((2*c.bitWidth)/3)) }

        expectedZp1 += z(1)
        expectedZp  += z(0)

        val mult = (z.drop(2).reverse zip alpha.drop(cyc).take(c.stages)).map( pair => { (pair._1 * pair._2) >> c.fracWidth } )
        var sumL = 0
        for ( i <- 0 until mult.length ) {
          var tmp = mult(i)
          if ( addToDicts(cyc + i) ) {
            sumL = ((forget*sumL) >> c.fracWidth) + tmp
          } else if ( c.isNORMA )
            sumL = (forget*sumL) >> c.fracWidth
        }
        expectedSumL += sumL

        poke(c.io.alpha, BigInt(alpha(cyc)))
        poke(c.io.addToDict, Bool(addToDicts(cyc)).litValue())

        for (s <- 0 until (c.stages + 2))
          poke(c.io.z(s), BigInt(z(s)))

        step(1)

        expect(c.io.zp1, BigInt(expectedZp1(cyc)))
        expect(c.io.zp, BigInt(expectedZp(cyc)))
        expect(c.io.sumL, BigInt(expectedSumL(cyc)))
        if ( cyc > c.stages + 1) {
          expect(c.io.forgetPowQ, BigInt(expectedQ(cyc)))
          expect(c.io.forgetPowQ1, BigInt(expectedQ1(cyc)))
        }
      }
    }

    val myRand = new Random
    val fracWidth = myRand.nextInt(24) + 1
    val bitWidth = myRand.nextInt(24) + fracWidth + 4
    val stages = myRand.nextInt(50)
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
