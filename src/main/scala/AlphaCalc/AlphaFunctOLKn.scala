package OLK.AlphaCalc

import Chisel._
import FixedPoint._

/** AlphaFunctOLKn
  This file computes the following:
  input ft => SFix
  input 1/(1+r) => Constant
  input C/(1+r) => Constant
  output out => SFix
  alpha(ft) = 1 - ft*(1/(1+r))
  if ft < (1 + r) - C
     alpha(ft) = C/(1+r)
  out = alpha(ft)
  */
class AlphaFunctOLKn(val bitWidth : Int, val intLength : Int) extends Module {
  val io = new Bundle {
    val ft     = SFix(intLength, bitWidth).asInput
    val fracr  = SFix(intLength, bitWidth).asInput
    val fracC  = SFix(intLength, bitWidth).asInput
    val alpha  = SFix(intLength, bitWidth).asOutput
  }
  val one = new SFix(intLength, SInt(1 << (bitWidth - intLength),width=bitWidth))
  val newAlpha = SFix(intLength, bitWidth)
  val tmpMult =  SFix(intLength, bitWidth)
  tmpMult := io.ft*io.fracr
  newAlpha := one - tmpMult
  when (newAlpha > io.fracC) {
    io.alpha := io.fracC
  } .otherwise {
    io.alpha := newAlpha
  }
}

class AlphaFunctOLKnTests(c: AlphaFunctOLKn) extends Tester(c) {
  var ftAry = Array(BigInt(12), BigInt(256), BigInt(731))
  var fracrAry = Array(BigInt(289), BigInt(173), BigInt(800))
  var fracCAry = Array(BigInt(100), BigInt(512), BigInt(1024))
  for ( ft <- ftAry ) {
    for ( fracr <- fracrAry ) {
      for ( fracC <- fracCAry ) {
        poke(c.io.fracr.raw, fracr)
        poke(c.io.fracC.raw, fracC)
        poke(c.io.ft.raw, ft)
        val alpha = (1 << (bitWidth - intLength)) - ((ft*fracr) >> (bitWidth - intLength))
        if (alpha > fracC)
          expect(c.io.alpha.raw, fracC)
        else
          expect(c.io.alpha.raw, alpha)
      }
    }
  }
}
