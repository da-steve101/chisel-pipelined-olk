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
  output q     = UInt(width=log2(s))

  Registers:
  Stage_0, Stage_1, ... Stage_(s-1)[1]
  Spare_0[s+1], ... Spare_(s-1)[2]
  QReg_0, QReg_1, QReg_2, ... QReg_(s-1) // Count the number of missed factors

  Logic:
  wi = vi.*alphai

  QReg_0 = (addToDict) ? 1 : 0
  QReg_1 = (addToDict) ? QReg_0 + 1 : QReg_0
  QReg_2 = (addToDict) ? QReg_1 + 1 : QReg_1
  QReg_3 = (addToDict) ? QReg_2 + 1 : QReg_2
  ... etc

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
  q    = QReg_(s-1)
  */

class SumR(val bitWidth : Int, val fracWidth : Int, val dictionarySize : Int, val numStages : Int) extends Module {
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
        val q = UInt(OUTPUT, width=log2(numStages))
    }

    /**
     * Initial Implementation
    */
    
    io.sumR := io.vi.reduceLeft(_ + _)
}

class SumRTests(c : SumR) extends Tester(c) {
    def toFixed(x : Double, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
    def toFixed(x : Float, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
    def toFixed(x : Int, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
    val r = scala.util.Random

    for (i <- 0 to 10) {
        val inVI = Array.fill(c.dictionarySize){r.nextInt(scala.math.pow(2, (c.bitWidth - c.fracWidth)/2).toInt) * r.nextFloat()}
        for (i <- 0 until c.dictionarySize) {
            var fixedVI = toFixed(inVI(i), c.fracWidth)
            poke(c.io.vi(i), fixedVI)
        }
        expect(c.io.sumR, toFixed(inVI.reduceLeft(_+_), c.fracWidth))
    }
}
