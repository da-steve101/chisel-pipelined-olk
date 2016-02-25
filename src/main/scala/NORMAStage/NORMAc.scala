/** NORMAc.scala -> This file computes the update for classification with NORMA
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

package OLK.NORMAStage

import Chisel._


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

  when(testCond >= Fixed(0, bitWidth, fracWidth)) {
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
