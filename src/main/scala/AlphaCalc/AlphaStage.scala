package OLK.AlphaCalc

import Chisel._
import FixedPoint._

/** AlphaStage
  This file computes the value of alpha for OLKn/c/r
  input Zp => SFix
  input sum => SFix
  input Wd => SFix
  input 1/(1+r) => Constant
  input C/(1+r) => Constant
  input forceNotAdd => 1 Bit
  // Optional Inputs
  input y        => (OLKn: 0, OLKc: 1 Bit, OLKr: 0)
  input (y - e)  => (OLKn: 0, OLKc: 0,     OLKr: SFix)
  input (-y - e) => (OLKn: 0, OLKc: 0,     OLKr: SFix)
  // Outputs
  output alpha => SFix  // Used for decision to add or not
  output ft    => SFix  // Used for query outputs
  output addToDict => 1 bit

  Registers:
  alphaReg      => SFix
  uAlphaReg     => SFix  (OLKc Only)
  addToDictReg  => 1 Bit (OLKr Only)
  forceNoAddReg => 1 Bit
  ftReg         => SFix  (For Query results)
  
  Logic:
  forceNoAddReg = forceNoAdd
  ft = alphaReg*Zp + sum*(1/(1+r))
  ft_alt = sum + Wd
  ftReg = (addToDict) ? ft : ft_alt
  -----------------
  alpha = alphaReg
  
  OLKc:
      OLKc_ = AlphaFunctOLKc()     // Block for ft
      OLKc_alt_ = AlphaFunctOLKc() // Block for ft_alt
      alphaReg = (addToDict) ? (OLKc_out) : (OLKc_alt_out)
      uAlphaReg = (addToDict) ? (OLKc_alpha) : (OLKc_alt_alpha)
      --------------------------------------------------------
      addToDict = !forceNotAdd && (uAlphaReg > 0)
  OLKn:
      OLKn_ = AlphaFunctOLKn()     // Block for ft
      OLKn_alt_ = AlphaFunctOLKn() // Block for ft_alt
      alphaReg = (addToDict) ? (OLKn_out) : (OLKn_alt_out)
      --------------------------------------------------------
      addToDict = !forceNotAdd && (alphaReg > 0)
  OLKr:
      OLKr_ = AlphaFunctOLKr()     // Block for ft
      OLKr_alt_ = AlphaFunctOLKr() // Block for ft_alt
      addToDictReg = (addToDict) ? (OLKr_addToDict) : (OLKr_alt_addToDict)
      alphaReg = (addToDict) ? (OLKr_out) : (OLKr_alt_out)
      --------------------------------------------------------
      addToDict = !forceNotAdd && (addToDictReg)
  */
class AlphaStage(val bitWidth : Int, val intLength : Int, val olkType : Int) extends Module {
  /**
    olkType => 0 = OLKc, 1 = OLKn, 2 = OLKr
    */
  val io = new Bundle {
    val zp     = SFix(intLength, bitWidth).asInput
    val sum    = SFix(intLength, bitWidth).asInput
    val wD     = SFix(intLength, bitWidth).asInput
    val forceNA = Bool(INPUT)
    val fracr  = SFix(intLength, bitWidth).asInput
    val fracC  = SFix(intLength, bitWidth).asInput
    val y    = Bool(INPUT) // OLKc only
    val yepos = SFix(intLength, bitWidth).asInput // OLKr only
    val yeneg = SFix(intLength, bitWidth).asInput // OLKr only
    val alpha  = SFix(intLength, bitWidth).asOutput
    val ft     = SFix(intLength, bitWidth).asOutput
    val addToDict = Bool(OUTPUT)
  }
  // REGISTERS
  val zero          = new SFix(intLength, SInt(0, width=bitWidth))
  val alphaReg      = Reg(init=zero)
  val uAlphaReg     = Reg(init=zero)  // OLKc Only
  val addToDictReg  = Reg(init=Bool(false))        // OLKr Only
  val forceNoAddReg = Reg(init=Bool(false))
  val ftReg         = Reg(init=zero)  // For Query results

  // LOGIC
  forceNoAddReg := io.forceNA
  val ft     = alphaReg*io.zp + io.sum*io.fracr
  val ft_alt = io.sum + io.wD
  when (io.addToDict) {
    ftReg := ft
  } .otherwise {
    ftReg := ft_alt
  }
  io.alpha := alphaReg

  if (olkType == 0) { // OLKc
    val olkc = Module(new AlphaFunctOLKc(bitWidth, intLength))
    when (io.addToDict) {
      olkc.io.ft := ft
    } .otherwise {
      olkc.io.ft := ft_alt
    }
    olkc.io.y := io.y
    olkc.io.fracr := io.fracr
    olkc.io.fracC := io.fracC
    uAlphaReg := olkc.io.Ualpha
    alphaReg := olkc.io.alpha
    io.addToDict  := (~forceNoAddReg) && (uAlphaReg > zero)
  }
  if (olkType == 1) { // OLKn
    val olkn = Module(new AlphaFunctOLKn(bitWidth, intLength))
    when (io.addToDict) {
      olkn.io.ft := ft
    } .otherwise {
      olkn.io.ft := ft_alt
    }
    olkn.io.fracr := io.fracr
    olkn.io.fracC := io.fracC
    alphaReg := olkn.io.alpha
    io.addToDict  := (~forceNoAddReg) && (alphaReg > zero)
  }
  if (olkType == 2) { // OLKr
    val olkr = Module(new AlphaFunctOLKr(bitWidth, intLength))
    when (io.addToDict) {
      olkr.io.ft := ft
    } .otherwise {
      olkr.io.ft := ft_alt
    }
    olkr.io.yepos := io.yepos
    olkr.io.yeneg := io.yeneg
    olkr.io.fracr := io.fracr
    olkr.io.fracC := io.fracC
    alphaReg := olkr.io.alpha
    addToDictReg  := olkr.io.addToDict
    io.addToDict  := (~forceNoAddReg) && addToDictReg
  }
}

class AlphaStageTester(c: AlphaStage) extends Tester(c) {

}
