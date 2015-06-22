package OLK.NORMAStage

import Chisel._
import cla.types._

/** NORMAn
  rho = (rho - ft > 0) ? (rho - eta*(1-nu)) : (rho + eta*nu)
  */
class NORMAn(val bitWidth : Int, val fracWidth : Int) extends Module {
  val io = new Bundle {
    val ft     = Fixed(INPUT, bitWidth, fracWidth)
    val rhoOld = Fixed(INPUT, bitWidth, fracWidth)
    val etanu  = Fixed(INPUT, bitWidth, fracWidth) // = eta*nu
    val etanu1 = Fixed(INPUT, bitWidth, fracWidth) // = -eta*(1-nu)

    val addToDict = Bool(OUTPUT)
    val rhoNew    = Fixed(OUTPUT, bitWidth, fracWidth)
  }
  val testCond = io.rhoOld - io.ft
  when (testCond > Fixed(0, bitWidth, fracWidth)) {
    io.rhoNew := io.rhoOld + io.etanu1
    io.addToDict := Bool(true)
  } .otherwise {
    io.rhoNew := io.rhoOld + io.etanu
    io.addToDict := Bool(false)
  }
}

class NORMAnTests(c: NORMAn) extends Tester(c) {
  def toFixed(x : Double, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
  def toFixed(x : Float, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
  def toFixed(x : Int, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
  val r = scala.util.Random

  for (i <- 0 until 10) {
    val ft     = BigInt(r.nextInt(1 << (c.bitWidth-2)))
    val rhoOld = BigInt(r.nextInt(1 << (c.bitWidth-2)))
    val eta    = BigInt(r.nextInt(1 << (c.bitWidth/2)))
    val nu     = BigInt(r.nextInt(1 << (c.bitWidth/2)))
    val etanu  = ((eta*nu) >> c.fracWidth)
    val etanu1 = -((eta*(toFixed(1, c.fracWidth) - nu)) >> c.fracWidth)

    var testCond = rhoOld - ft
    // dodgy hack incase overflowed into sign bit
    if (testCond > (1 << (c.bitWidth-1)))
      testCond = BigInt(-1)
    var rho = rhoOld + etanu
    var addToDict = false
    if (testCond > 0) {
      addToDict = true
      rho = rhoOld + etanu1
    }
    poke(c.io.ft, ft)
    poke(c.io.rhoOld, rhoOld)
    poke(c.io.etanu, etanu)
    poke(c.io.etanu1, etanu1)
    expect(c.io.addToDict, Bool(addToDict).litValue())
    expect(c.io.rhoNew, rho)
  }
}
