package OLK.AlphaCalc

import Chisel._
import FixedPoint._

/** AlphaFunctOLKr
  This file computes the following 
  input ft => SFix
  input   y - e => SFix
  input - y - e => SFix
  input 1/(1+r) => Constant
  input C/(1+r) => Constant
  output out => SFix
  output addToDict => 1 Bit
  alpha_1(ft) = (  y - e) - ft*(1/(1+r))
  alpha_2(ft) = (- y - e) + ft*(1/(1+r))
  if alpha_12(ft) > C/(1+r)
     alpha_12(ft) = C/(1+r)

  if alpha_12(ft) < 0
     alpha_12(ft) = 0
  if alpha_1(ft) == 0 && alpha_2(ft) == 0
     addToDict = 0
  else
     addToDict = 1

  out = alpha_2(ft) - alpha_1(ft)
  */
class AlphaFunctOLKr(val bitWidth : Int, val intLength : Int) extends Module {
  val io = new Bundle {
    val ft     = SFix(intLength, bitWidth).asInput
    val yepos  = SFix(intLength, bitWidth).asInput
    val yeneg  = SFix(intLength, bitWidth).asInput
    val fracr  = SFix(intLength, bitWidth).asInput
    val fracC  = SFix(intLength, bitWidth).asInput
    val alpha  = SFix(intLength, bitWidth).asOutput
    val addToDict = Bool(OUTPUT)
  }
  val alpha1_A = io.yepos - io.ft*io.fracr
  val alpha2_A = io.yeneg + io.ft*io.fracr
  val alpha1_B = alpha1_A
  val alpha2_B = alpha2_A
  val zero     = new SFix(intLength, SInt(0, width=bitWidth))
  when (alpha1_A > io.fracC) {
    alpha1_B := io.fracC
  } .elsewhen (zero > alpha1_A) {
    alpha1_B := zero
  }
  when (alpha2_A > io.fracC) {
    alpha2_B := io.fracC
  } .elsewhen (zero > alpha2_A ) {
    alpha2_B := zero
  }
  io.addToDict := (alpha2_A > zero || alpha1_A > zero)

  io.alpha := alpha2_B - alpha1_B
}

class AlphaFunctOLKrTests(c: AlphaFunctOLKr) extends Tester(c) {

}
