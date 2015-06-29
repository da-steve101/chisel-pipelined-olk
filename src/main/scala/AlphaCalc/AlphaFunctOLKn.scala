/** AlphaFunctOLKn.scala -> This file computes the update for OLK with novelty detection
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


/** AlphaFunctOLKn
  This file computes the following:
  input ft => Fixed
  input 1/(1+r) => Constant
  input C/(1+r) => Constant
  output out => Fixed
  alpha(ft) = 1 - ft*(1/(1+r))
  if ft < (1 + r) - C
     alpha(ft) = C/(1+r)
  out = alpha(ft)
  */
class AlphaFunctOLKn(val bitWidth : Int, val fracWidth : Int) extends Module {
  val io = new Bundle {
    val ft     = Fixed(INPUT, bitWidth, fracWidth)
    val fracr  = Fixed(INPUT, bitWidth, fracWidth)
    val fracC  = Fixed(INPUT, bitWidth, fracWidth)
    val alpha  = Fixed(OUTPUT, bitWidth, fracWidth)
  }
  val one = Fixed(1.0, bitWidth, fracWidth)
  val tmpMult  = io.ft*io.fracr
  val newAlpha = one - tmpMult
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
  val one = BigInt(1 << c.fracWidth)
  for ( ft <- ftAry ) {
    for ( fracr <- fracrAry ) {
      for ( fracC <- fracCAry ) {
        poke(c.io.fracr, fracr)
        poke(c.io.fracC, fracC)
        poke(c.io.ft, ft)
        val alpha = one - ((ft*fracr) >> c.fracWidth)
        if (alpha > fracC)
          expect(c.io.alpha, fracC)
        else
          expect(c.io.alpha, alpha)
      }
    }
  }
}
