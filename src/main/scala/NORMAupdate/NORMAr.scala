package OLK.NORMAStage

import Chisel._


/** NORMAr
  Note: using e-insensitive loss but renamed epsilon to rho
  This file computes the following
  inputs ft, y, rho
  outputs addToDict, sign, rho

  sign = y > ft
  if ( |y - ft| - rho > 0)
     addToDict = true
     rho = rho + eta*(1 - nu)
  else
     addToDict = false
     rho = rho - eta*nu
  */
class NORMAr(val bitWidth : Int, val fracWidth : Int) extends Module {
  val io = new Bundle {
    val ft     = Fixed(INPUT, bitWidth, fracWidth)
    val y      = Fixed(INPUT, bitWidth, fracWidth)
    val rhoOld = Fixed(INPUT, bitWidth, fracWidth)
    val etanu  = Fixed(INPUT, bitWidth, fracWidth) // = eta*nu
    val etanu1 = Fixed(INPUT, bitWidth, fracWidth) // = -eta*(1-nu)

    val addToDict = Bool(OUTPUT)
    val sign      = Bool(OUTPUT)
    val rhoNew    = Fixed(OUTPUT, bitWidth, fracWidth)
  }
  val isPos = (io.y > io.ft)
  val testCond = Mux(isPos, io.y - io.ft - io.rhoOld, io.ft - io.y - io.rhoOld)
  when (testCond > Fixed(0, bitWidth, fracWidth)) {
    io.addToDict := Bool(true)
    io.rhoNew := io.rhoOld - io.etanu1
  } .otherwise {
    io.addToDict := Bool(false)
    io.rhoNew := io.rhoOld - io.etanu
  }
  io.sign := isPos
}

class NORMArTests(c: NORMAr) extends Tester(c) {
  def toFixed(x : Double, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
  def toFixed(x : Float, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
  def toFixed(x : Int, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
  val r = scala.util.Random

  for (i <- 0 until 10) {
    val ft     = BigInt(r.nextInt(1 << (c.bitWidth-2)))
    val y      = BigInt(r.nextInt(1 << (c.bitWidth-2)))
    val rhoOld = BigInt(r.nextInt(1 << (c.bitWidth-2)))
    val eta    = BigInt(r.nextInt(1 << (c.bitWidth/2)))
    val nu     = BigInt(r.nextInt(1 << (c.bitWidth/2)))
    val etanu  = ((eta*nu) >> c.fracWidth)
    val etanu1 = -((eta*(toFixed(1, c.fracWidth) - nu)) >> c.fracWidth)

    val isPos = (y > ft)
    var testCond = ft - y - rhoOld
    if (isPos)
      testCond = y - ft - rhoOld
    // dodgy hack incase overflowed into sign bit
    if (testCond > (1 << (c.bitWidth-1)))
      testCond = BigInt(-1)
    var rho = rhoOld - etanu
    var addToDict = false
    if (testCond > 0) {
      addToDict = true
      rho = rhoOld - etanu1
    }
    poke(c.io.ft, ft)
    poke(c.io.y, y)
    poke(c.io.rhoOld, rhoOld)
    poke(c.io.etanu, etanu)
    poke(c.io.etanu1, etanu1)
    expect(c.io.addToDict, Bool(addToDict).litValue())
    expect(c.io.rhoNew, rho)
    expect(c.io.sign, Bool(isPos).litValue())
  }
}
