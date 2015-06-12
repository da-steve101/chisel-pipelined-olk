package OLK.Sum

import Chisel._
import OLK._
import cla.types._

/** SumR
  This function sums the results of the kernel function
  that are currently in the dictionary
  parameter d : dictionary size
  parameter s : stages

  input vi     = SFix[d]  // Outputs from kernel function
  input alphai = SFix[d] // Current weights of dictionary
  input addToDict = 1 Bit
  
  output sumR  = SFix
  output wD    = SFix
  output WD1   = SFix

  Registers:
  Stage_0, Stage_1, ... Stage_(s-1)[1]
  Spare_0[s+1], ... Spare_(s-1)[2]

  Logic:
  wi = vi.*alphai

  Stage_0[0:k] = sum(w1 : w(d-s-2)) + (addTODict) ? w(d-s-1) : 0
  Stage_1[0:l] = sum(Stage_0) + (addToDict) ? Spare_0[0] : 0
  Stage_2[0:m] = sum(Stage_1) + (addToDict) ? Spare_1[0] : 0
  ... etc where m < l < k

  Spare_0 = (addToDict) ? [w(d-s-2):w(d)] : [w(d-s-1):w(d-1)]
  Spare_1 = (addToDict) ? Spare_0[1:end]  : Spare_0[0:end-1]
  Spare_2 = (addToDict) ? Spare_1[1:end]  : Spare_1[0:end-1]
  ... etc

  sumR = Stage_(s-1)
  wD1  = Spare_(s-1)[0]
  wD   = Spare_(s-1)[1]
  */

class SumR(val bitWidth : Int, val fracWidth : Int, val dictionarySize : Int, val numStages : Int) extends Module {
  def group[A](list : List[A], size : Int) : List[List[A]] = list.foldLeft( (List[List[A]](), 0) ) { (r, c) => 
    r match {
      case (head :: tail, num) =>
        if (num < size) ( (c :: head) :: tail, num + 1)
        else            ( List(c) :: head :: tail, 1)
    case (Nil, num) => (List(List(c)),1)
    }
  }._1.foldLeft(List[List[A]]())( (r,c) => c.reverse :: r)

    val lnOf2 = scala.math.log(2)
    def log2(x : Int) : Int = (scala.math.log(x.toDouble) / lnOf2).toInt
    def log2(x : Double) : Int = (scala.math.log(x) / lnOf2).toInt

    val io = new Bundle {
        val vi  = Vec.fill(dictionarySize){Fixed(INPUT, bitWidth, fracWidth)}
        val alphai = Vec.fill(dictionarySize){Fixed(INPUT, bitWidth, fracWidth)}
        val addToDict = Bool(INPUT)

        val sumR = Fixed(OUTPUT, bitWidth, fracWidth)
        val wD = Fixed(OUTPUT, bitWidth, fracWidth)
        val wD1 = Fixed(OUTPUT, bitWidth, fracWidth)
    }

    /**
     * Initial Implementation
     */
    // Setup inputs as list to make it easier to work with
    val viList = io.vi.toList
    val alphaiList = io.alphai.toList

    // wi = vi.*alphai
    val wiList = (viList zip alphaiList).map(pair => pair._1 * pair._2)

    // Pipeline Stage Correction
    // Spares
    val spares = scala.collection.mutable.MutableList(Mux(io.addToDict, Vec(wiList.slice(dictionarySize - numStages, dictionarySize)), Vec(wiList.slice(dictionarySize - numStages - 1, dictionarySize - 1))))
    val pipeStage = scala.collection.mutable.MutableList(Mux(io.addToDict, wiList(dictionarySize - numStages - 1), 0))
    for (i <- 0 until numStages) {

    }

    // Adder Tree
    // Tree Roots
    val adderTree = scala.collection.mutable.MutableList(group(wiList,2))
    val adderAdditionTree = scala.collection.mutable.MutableList(adderTree(0).map(pair => pair(0) + pair(1)))
    
    // GROW TREE GROW!!!!
    for (i <- 0 until log2(dictionarySize/2)) {
      adderTree += group(adderAdditionTree(i), 2)
      adderAdditionTree += adderTree(i+1).map(pair => pair(0) + pair(1))
    }

    // Output
    io.sumR := adderAdditionTree(log2(dictionarySize/2+1))(0)
}

class SumRTests(c : SumR) extends Tester(c) {
    val lnOf2 = scala.math.log(2)
    def log2(x : Int) : Int = (scala.math.log(x.toDouble) / lnOf2).toInt
    def toFixed(x : Double, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
    def toFixed(x : Float, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
    def toFixed(x : Int, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
    val r = scala.util.Random

    for (i <- 0 to 10) {
        val inVI = Array.fill(c.dictionarySize){r.nextInt(scala.math.pow(2, (c.bitWidth - c.fracWidth)/2).toInt) * r.nextFloat()}
        val inAlphaI = Array.fill(c.dictionarySize){r.nextInt(scala.math.pow(2, (c.bitWidth - c.fracWidth)/2).toInt) * r.nextFloat()}
        for (i <- 0 until c.dictionarySize) {
            var fixedVI = toFixed(inVI(i), c.fracWidth)
            var fixedAlphaI = toFixed(inAlphaI(i), c.fracWidth)
            poke(c.io.vi(i), fixedVI)
            poke(c.io.alphai(i), fixedAlphaI)
        }
        expect(c.io.sumR, toFixed((inVI zip inAlphaI).map(pair => pair._1 * pair._2).reduceLeft(_+_), c.fracWidth))
    }
}
