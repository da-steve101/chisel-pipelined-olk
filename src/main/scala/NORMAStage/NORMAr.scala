/** NORMAr.scala -> This file computes the update for regression with NORMA
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
  when (testCond >= Fixed(0, bitWidth, fracWidth)) {
    io.addToDict := Bool(true)
    io.rhoNew := io.rhoOld - io.etanu1
  } .otherwise {
    io.addToDict := Bool(false)
    io.rhoNew := io.rhoOld - io.etanu
  }
  io.sign := isPos
}
