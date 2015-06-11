package OLK.NORMAStage

import Chisel._
import cla.types._

/** NORMAStage
  This file computes the update for NORMA
  */
class NORMAStage(val bitWidth : Int, val fracWidth : Int, val NORMAtype : Int) extends Module {
  Predef.assert(NORMAtype == 1 || NORMAtype == 2 || NORMAtype == 3,
    "Norma type must be Classification = 1, Novelty = 2, Regression = 3")
  val io = new Bundle {
    val sum    = Fixed(INPUT, bitWidth, fracWidth)
    val zp     = Fixed(INPUT, bitWidth, fracWidth)
    val wD     = Fixed(INPUT, bitWidth, fracWidth)
    val forget = Fixed(INPUT, bitWidth, fracWidth)

    val yC     = Bool(INPUT) // Only used for NORMAc
    val yReg   = Fixed(INPUT, bitWidth, fracWidth) // Only used for NORMAr
    val etapos = Fixed(INPUT, bitWidth, fracWidth) // = eta
    val etaneg = Fixed(INPUT, bitWidth, fracWidth) // = -eta
    val etanu  = Fixed(INPUT, bitWidth, fracWidth) // = eta*nu
    val etanu1 = Fixed(INPUT, bitWidth, fracWidth) // = -eta*(1-nu)

    val addToDict = Bool(OUTPUT)
    val ft        = Fixed(OUTPUT, bitWidth, fracWidth)
    val alpha     = Fixed(OUTPUT, bitWidth, fracWidth)
  }
  val zero = Fixed(0, bitWidth, fracWidth)
  val rhoReg = Reg(init=zero)
  val alphaReg = Reg(init=zero)
  val ftReg = Reg(init=zero)
  val addToDictReg = Reg(init=Bool(false))
  io.alpha := alphaReg
  io.ft := ftReg
  io.addToDict := addToDictReg

  val ft = Mux(addToDictReg, (alphaReg*io.zp) + (io.forget*io.sum), io.sum + io.wD)
  if (NORMAtype == 2)
    ftReg := ft - rhoReg
  else
    ftReg := ft

  val NORMA = { if (NORMAtype == 1) {
    val res = Module(new NORMAc(bitWidth, fracWidth))
    val bReg = Reg(init=zero)
    res.io.bOld := bReg
    bReg := res.io.bNew
    res.io.y := io.yC
    res.io.etapos := io.etapos
    res.io.etaneg := io.etaneg
    alphaReg := Mux(res.io.sign, io.etapos, io.etaneg)
    res
  }
  else if (NORMAtype == 2) {
    val res = Module(new NORMAn(bitWidth, fracWidth))
    alphaReg := io.etapos
    res
  } else {
    val res = Module(new NORMAr(bitWidth, fracWidth))
    res.io.y := io.yReg
    alphaReg := Mux(res.io.sign, io.etapos, io.etaneg)
    res
  } }

  // Common Section
  NORMA.io.ft := ft
  NORMA.io.rhoOld := rhoReg
  NORMA.io.etanu  := io.etanu
  NORMA.io.etanu1 := io.etanu1
  addToDictReg := NORMA.io.addToDict
  rhoReg := NORMA.io.rhoNew
}

class NORMAStageTests(c: NORMAStage) extends Tester(c) {
  def toFixed(x : Double, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
  def toFixed(x : Float, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
  def toFixed(x : Int, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
  val r = scala.util.Random

  // internal Registers
  var rho = BigInt(0)
  var b = BigInt(0)
  var alpha = BigInt(0)
  var addToDict = false

  for (i <- 0 until 10) {
    // Generate inputs
    val sum    = BigInt(r.nextInt(1 << (c.bitWidth-2)))
    val zp     = BigInt(r.nextInt(1 << (c.bitWidth-2)))
    val wD     = BigInt(r.nextInt(1 << (c.bitWidth-2)))
    val forget = BigInt(r.nextInt(1 << (c.fracWidth)))

    val yC     = (r.nextInt(2)*2) - 1
    val yReg   = BigInt(r.nextInt(1 << (c.bitWidth-2)))
    val eta    = BigInt(r.nextInt(1 << (c.bitWidth/2)))
    val nu     = BigInt(r.nextInt(1 << (c.bitWidth/2)))
    val etanu  = ((eta*nu) >> c.fracWidth)
    val etanu1 = -((eta*(toFixed(1, c.fracWidth) - nu)) >> c.fracWidth)
    val etapos = eta
    val etaneg = -eta

    poke(c.io.sum, sum)
    poke(c.io.zp, zp)
    poke(c.io.wD, wD)
    poke(c.io.forget, forget)
    poke(c.io.yC, Bool(yC == 1).litValue())
    poke(c.io.yReg, yReg)
    poke(c.io.etanu, etanu)
    poke(c.io.etanu1, etanu1)
    poke(c.io.etapos, etapos)
    poke(c.io.etaneg, etaneg)

    var ft = sum + wD
    if (addToDict) {
      val tmpA = (alpha * zp) >> c.fracWidth
      val tmpB = (forget * sum) >> c.fracWidth
      ft = (tmpA + tmpB)
    }
    val ftNov = ft - rho
    // compute expected alpha and addToDict
    addToDict = false
    alpha = BigInt(0)
    if (c.NORMAtype == 1) {
      var testCond = rho - yC*(ft + b)
      // dodgy hack incase overflowed into sign bit
      if (testCond > (1 << (c.bitWidth-1)))
        testCond = BigInt(-1)
      alpha = yC*eta
      if (testCond > BigInt(0)) {
        b = b + yC*eta
        rho = rho + etanu1
        addToDict = true
      } else
        rho = rho + etanu
    } else if (c.NORMAtype == 2) {
      var testCond = rho - ft
      // dodgy hack incase overflowed into sign bit
      if (testCond > (1 << (c.bitWidth-1)))
        testCond = BigInt(-1)
      alpha = eta
      if (testCond > 0) {
        addToDict = true
        rho = rho + etanu1
      } else
        rho = rho + etanu
    } else {
      val isPos = (yReg > ft)
      var testCond = ft - yReg - rho
      if (isPos) {
        testCond = yReg - ft - rho
        alpha = etapos
      } else
        alpha = etaneg
      if (testCond > (1 << (c.bitWidth-1)))
        testCond = BigInt(-1)
      if (testCond > 0) {
        addToDict = true
        rho = rho - etanu1
      } else
        rho = rho - etanu
    }
    // Clock and read outputs
    step(1)
    if (c.NORMAtype == 2)
      ft = ftNov
    expect(c.io.addToDict, Bool(addToDict).litValue())
    expect(c.io.ft, ft)
    expect(c.io.alpha, alpha)
  }
}
