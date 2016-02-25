/** NORMAn -> This file computes the update for novelty detection with NORMA
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
  when (testCond >= Fixed(0, bitWidth, fracWidth)) {
    io.rhoNew := io.rhoOld + io.etanu1
    io.addToDict := Bool(true)
  } .otherwise {
    io.rhoNew := io.rhoOld + io.etanu
    io.addToDict := Bool(false)
  }
}
