package OLK.NORMAStage

import Chisel._
import cla.types._

/** NORMAc
  This file computes the following 
  input ft => SFix
  input y => Bool (1 = +ve, 0 = -ve)
  input rhoOld => SFix
  input bOld => SFix
  input etapos => Constant
  input etaneg => Constant
  input etanu  => Constant
  input etanu1 => Constant

  output addToDict => Bool
  output bNew => SFix
  output rhoNew => SFix

  rhoAdded = rhoOld - etanu1
  rhoNotAdded = rhoOld - etanu

  bAdded = (y) ? bOld + etapos : bOld + etaneg

  testCond = (y) ? rhoOld - (ft + bOld) : rhoOld + (ft + bOld)

  addToDict = (testCond > 0)
  bNew = (addToDict) ? bAdded : bOld
  rhoNew = (addToDict) ? rhoAdded : rhoNotAdded
  
  */
class NORMAc(val bitWidth : Int, val fracWidth : Int) extends Module {
  val io = new Bundle {
    val ft     = Fixed(INPUT, bitWidth, fracWidth)
    val y      = Bool(INPUT)  // = (1 = +ve, 0 = -ve)
    val rhoOld = Fixed(INPUT, bitWidth, fracWidth)
    val bOld   = Fixed(INPUT, bitWidth, fracWidth)
    val etapos = Fixed(INPUT, bitWidth, fracWidth) // = eta
    val etaneg = Fixed(INPUT, bitWidth, fracWidth) // = -eta
    val etanu  = Fixed(INPUT, bitWidth, fracWidth) // = eta*nu
    val etanu1 = Fixed(INPUT, bitWidth, fracWidth) // = -eta*(1-nu)

    val addToDict = Bool(OUTPUT)
    val sign      = Bool(OUTPUT)
    val bNew      = Fixed(OUTPUT, bitWidth, fracWidth)
    val rhoNew    = Fixed(OUTPUT, bitWidth, fracWidth)
  }

  val rhoAdded = io.rhoOld + io.etanu1
  val rhoNotAdded = io.rhoOld + io.etanu
  // Mux(y>0, y = 1, y = -1)
  val bAdded = Mux(io.y, io.bOld + io.etapos, io.bOld + io.etaneg)
  val gt = io.ft + io.bOld
  val testCond = Mux(io.y, io.rhoOld - gt, io.rhoOld + gt)

  when(testCond > Fixed(0, bitWidth, fracWidth)) {
    io.bNew := bAdded
    io.addToDict := Bool(true)
    io.rhoNew := rhoAdded
  } .otherwise {
    io.bNew := io.bOld
    io.addToDict := Bool(false)
    io.rhoNew := rhoNotAdded
  }
  io.sign := io.y
}

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

    var testCond = rhoOld - y*(ft + bOld)
    var bNew = bOld
    var rhoNew = rhoOld + etanu
    var addToDict = Bool(false)
    // dodgy hack incase overflowed into sign bit
    if (testCond > (1 << (c.bitWidth-1)))
      testCond = BigInt(-1)
    if (testCond > BigInt(0)) {
      bNew = bOld + y*eta
      rhoNew = rhoOld + etanu1
      addToDict = Bool(true)
    }
    poke(c.io.ft, ft)
    poke(c.io.rhoOld, rhoOld)
    poke(c.io.bOld, bOld)
    poke(c.io.y, Bool(y == 1).litValue())
    poke(c.io.etapos, etapos)
    poke(c.io.etaneg, etaneg)
    poke(c.io.etanu, etanu)
    poke(c.io.etanu1, etanu1)
    expect(c.io.addToDict, addToDict.litValue())
    expect(c.io.bNew, bNew)
    expect(c.io.rhoNew, rhoNew)
  }
}
