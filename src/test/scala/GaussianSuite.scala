import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import Chisel._
import OLK.Kernel._

class GaussianSuite extends TestSuite {

  @Test def gaussTest {
    class GaussianTests(c : Gaussian) extends Tester(c) {
      val r = scala.util.Random

      val cycles = 3*(c.kCycles + 1)

      val pipeline   = ArrayBuffer.fill(c.pCycles){ArrayBuffer.fill(c.features){BigInt(r.nextInt(1 << c.fracWidth))}}
      val dictionary = ArrayBuffer.fill(c.dictSize){ArrayBuffer.fill(c.features){BigInt(r.nextInt(1 << c.fracWidth))}}
      val addToDict = ArrayBuffer.fill(cycles + c.kCycles){ r.nextInt(2) == 1 }
      val gamma = BigInt(r.nextInt(1 << c.fracWidth))
      for (j <- 0 until c.features) {
        for (i <- 0 until c.pCycles)
          poke(c.io.pipeline(i)(j), pipeline(i)(j))
        for (i <- 0 until c.dictSize)
          poke(c.io.dictionary(i)(j), dictionary(i)(j))
      }
      poke(c.io.gamma, gamma)

      // Pad inital cycles with zeros
      // expectedPipeline(cycle)(index)
      val expectedPipeline = ArrayBuffer.fill(c.kCycles){ArrayBuffer.fill(c.pCycles - c.kCycles)(BigInt(0))}
      // expectedDict(cycle)(index)
      val expectedDict = ArrayBuffer.fill(c.kCycles){ArrayBuffer.fill(c.dictSize)(BigInt(0))}

      val log2Table = { (scala.math.log10(c.tableSize)/scala.math.log10(2)).ceil.toInt }

      // Generate Table for linear interpolation
      val gradients   = new ArrayBuffer[BigInt]()
      val offsets     = new ArrayBuffer[BigInt]()
      // Fixed point increment
      val increment   = 1.0 / (1 << log2Table)
      val tableEnd    = 1.0
      var x = 0.0
      // NOTE: x is positive, therefore gradient is negitive
      while (x < tableEnd) {
        // m = (y1 - y2)/(x1 - x2)
        val m = -(scala.math.pow(2, - x) - scala.math.pow(2,- x - increment))/increment
        // convert to Fixed
        gradients += BigInt((m  * (1 << c.fracWidth)).toLong)
        // b = y1 - m*x1
        val b = scala.math.pow(2, - x) - m*x
        // convert to Fixed
        offsets += BigInt((b * (1 << c.fracWidth)).toLong)
        x += increment
      }

      for ( cyc <- 0 until cycles ) {
        val example = ArrayBuffer.fill(c.features){BigInt(r.nextInt(1 << c.fracWidth))}
        // calculate the outputs for this example
        val subPipe = pipeline.map(x => {
          val subAry = new ArrayBuffer[BigInt]()
          for (i <- 0 until c.features)
            subAry += x(i) - example(i)
          subAry
        })
        val subDict = dictionary.map(x => {
          val subAry = new ArrayBuffer[BigInt]()
          for (i <- 0 until c.features)
            subAry += x(i) - example(i)
          subAry
        })
        val l2Pipe = subPipe.map(x => { x.map(y => { ((y*y) >> c.fracWidth) }).sum }).map( x => { (gamma*x) >> c.fracWidth })
        val l2Dict = subDict.map(x => { x.map(y => { ((y*y) >> c.fracWidth) }).sum }).map( x => { (gamma*x) >> c.fracWidth })
        val xIntPipe = l2Pipe.map(x => { x >> c.fracWidth }).map(x => x.toInt)
        val xIntDict = l2Dict.map(x => { x >> c.fracWidth }).map(x => x.toInt)
        val xTabPipe = l2Pipe.map(x => { (x >> (c.fracWidth - log2Table)) & ((1 << log2Table) - 1) }).map(x => x.toInt)
        val xTabDict = l2Dict.map(x => { (x >> (c.fracWidth - log2Table)) & ((1 << log2Table) - 1) }).map(x => x.toInt)
        val xFracPipe = l2Pipe.map(x => { x & ((1 << c.fracWidth) - 1) })
        val xFracDict = l2Dict.map(x => { x & ((1 << c.fracWidth) - 1) })
        val yPipe    = (xFracPipe zip xTabPipe).map(pair => { ((gradients(pair._2)*pair._1) >> c.fracWidth) + offsets(pair._2) })
        val yDict    = (xFracDict zip xTabDict).map(pair => { ((gradients(pair._2)*pair._1) >> c.fracWidth) + offsets(pair._2) })
        val yOutPipe = (yPipe zip xIntPipe).map(pair => { pair._1 >> pair._2 })

        val yPipeAdded = new ArrayBuffer[BigInt]()
        for (i <- 0 until c.kCycles) {
          if (addToDict(c.kCycles - 1 - i + cyc))
            yPipeAdded += yOutPipe(yOutPipe.length - c.kCycles + i)
        }

        val yOutDictTmp = (yDict zip xIntDict).map(pair => { pair._1 >> pair._2 })
        val yOutDict = yOutDictTmp.take(c.dictSize - yPipeAdded.length)

        // Add the values for k cycles in advance
        expectedPipeline += yOutPipe.take(c.pCycles - c.kCycles)
        yPipeAdded.appendAll(yOutDict)
        expectedDict     += yPipeAdded

        poke(c.io.addToDict, Bool(addToDict(cyc)).litValue())
        for (i <- 0 until c.features)
          poke(c.io.example(i), example(i))

        if (cyc >= c.kCycles) {
          for (i <- 0 until (c.pCycles - c.kCycles))
            expect(c.io.pipelineOut(i), expectedPipeline(cyc)(i))
          for (i <- 0 until (c.dictSize))
            expect(c.io.dictOut(i), expectedDict(cyc)(i))
        }

        step(1)
      }
    }

    val myRand = new Random
    val fracWidth = myRand.nextInt(24) + 1
    val bitWidth = myRand.nextInt(24) + fracWidth + 4
    val dictSize = myRand.nextInt(250) + 1
    val log2Features = myRand.nextInt(5) + 1
    val features = 1 << log2Features
    val pCycles = myRand.nextInt(20) + 8
    val stages = ArrayBuffer.fill(7 + log2Features){ myRand.nextInt(2) == 1 }
    val tableSize = 1 << (myRand.nextInt(8) + 1)
    println("val fracWidth = " + fracWidth)
    println("val bitWidth = " + bitWidth)
    println("val dictSize = " + dictSize)
    println("val features = " + features)
    println("val pCycles = " + pCycles)
    println("val stages = " + stages)
    println("val tableSize = " + tableSize)
    chiselMainTest(Array("--genHarness", "--compile", "--test", "--backend", "c"), () => {
      Module( new Gaussian( bitWidth, fracWidth, dictSize, features, pCycles, stages, tableSize) )
    } ) { c => new GaussianTests( c ) }

  }

}
