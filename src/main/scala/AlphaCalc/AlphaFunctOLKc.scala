/** AlphaFunctOLKc -> module for computing the update for OLK classification
Copyright (C) 2015 Stephen Tridgell

This file is part of a pipelined OLK application.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this code.  If not, see <http://www.gnu.org/licenses/>.
*/

package OLK.AlphaCalc

import Chisel._
import FixedPoint._

/** AlphaFunctOLKc
  This file computes the following 
  input ft => SFix
  input y => +/-1
  input 1/(1+r) => Constant
  input C/(1+r) => Constant
  output out => SFix
  output alpha => SFix  // Used for decision to add or not
  alpha(ft) = 1 - ft*(y/(1+r))
  if alpha(ft) > C/(1+r)
     alpha(ft) = C/(1+r)
  alpha = alpha(ft)
  out = (y == 1) ? alpha(ft) : -alpha(ft)
  */
class AlphaFunctOLKc(val bitWidth : Int, val intLength : Int) extends Module {
  val io = new Bundle {
    val ft     = SFix(intLength, bitWidth).asInput
    val y      = Bool(INPUT) // 1 = +1, 0 = -1
    val fracr  = SFix(intLength, bitWidth).asInput
    val fracC  = SFix(intLength, bitWidth).asInput
    val Ualpha  = SFix(intLength, bitWidth).asOutput
    val alpha  = SFix(intLength, bitWidth).asOutput
  }
  val one = new SFix(intLength, SInt(1 << (bitWidth - intLength),width=bitWidth))
  val zero = new SFix(intLength, SInt(0,width=bitWidth))
  val newAlpha_A = SFix(intLength, bitWidth)
  val newAlpha_B = SFix(intLength, bitWidth)
  val tmpMult =  SFix(intLength, bitWidth)
  tmpMult := io.ft*io.fracr
  newAlpha_A := one - tmpMult
  newAlpha_B := one + tmpMult
  val Ualpha_res = SFix(intLength, bitWidth)
  Ualpha_res := zero
  when (io.y) {
    when (newAlpha_A > io.fracC) {
      Ualpha_res :=  newAlpha_A
    } .otherwise {
      Ualpha_res :=  io.fracC
    }
  } .otherwise {
    when (newAlpha_B > io.fracC) {
      Ualpha_res :=  newAlpha_B
    } .otherwise {
      Ualpha_res :=  io.fracC
    }
  }
  io.Ualpha := Ualpha_res
  when (io.y) {
    io.alpha := Ualpha_res
  } .otherwise {
    io.alpha := Ualpha_res
  }
}

class AlphaFunctOLKcTests(c: AlphaFunctOLKc) extends Tester(c) {
  var ftAry = Array(BigInt(12), BigInt(256), BigInt(731))
  var fracrAry = Array(BigInt(289), BigInt(173), BigInt(800))
  var fracCAry = Array(BigInt(100), BigInt(512), BigInt(1024))
  var yAry = Array(true, false)
  for ( ft <- ftAry ) {
    for ( fracr <- fracrAry ) {
      for ( fracC <- fracCAry ) {
        for ( y <- yAry ) {
          poke(c.io.fracr.raw, fracr)
          poke(c.io.y, Bool(y).litValue())
          poke(c.io.fracC.raw, fracC)
          poke(c.io.ft.raw, ft)
          var alpha = BigInt(0)
          var Ualpha = BigInt(0)
          if (y) {
            Ualpha = BigInt(1 << (c.bitWidth - c.intLength)) - ((ft*fracr) >> (c.bitWidth - c.intLength))
            if (Ualpha > fracC)
              Ualpha = fracC
            alpha = Ualpha
          }
          else{
            Ualpha = BigInt(1 << (c.bitWidth - c.intLength)) + ((ft*fracr) >> (c.bitWidth - c.intLength))
            if (Ualpha > fracC)
              Ualpha = fracC
            alpha = -Ualpha
          }
          expect(c.io.alpha.raw, alpha)
          expect(c.io.Ualpha.raw, Ualpha)
        }
      }
    }
  }
}
