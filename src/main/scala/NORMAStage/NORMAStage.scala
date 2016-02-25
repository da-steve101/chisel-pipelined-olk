/** NORMAStage.scala -> This file computes the update for all applications of NORMA
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


class IOBundle(val bitWidth : Int, val fracWidth : Int) extends Bundle {
  val forceNA = Bool(INPUT)
  val sum    = Fixed(INPUT, bitWidth, fracWidth)
  val zp     = Fixed(INPUT, bitWidth, fracWidth)
  val wD     = Fixed(INPUT, bitWidth, fracWidth)
  val forget = Fixed(INPUT, bitWidth, fracWidth)

  val etapos = Fixed(INPUT, bitWidth, fracWidth) // = eta
  val etaneg = Fixed(INPUT, bitWidth, fracWidth) // = -eta
  val etanu  = Fixed(INPUT, bitWidth, fracWidth) // = eta*nu
  val etanu1 = Fixed(INPUT, bitWidth, fracWidth) // = -eta*(1-nu)

  val forceNAout = Bool(OUTPUT)
  val addToDict = Bool(OUTPUT)
  val ft        = Fixed(OUTPUT, bitWidth, fracWidth)
  val alpha     = Fixed(OUTPUT, bitWidth, fracWidth)
}

// Only used for NORMAc
class IOBundle_C(val bitWidth_c : Int, val fracWidth_c : Int) extends IOBundle(bitWidth_c, fracWidth_c) {
  val yC = Bool(INPUT)
}

// Only used for NORMAr
class IOBundle_R(val bitWidth_r : Int, val fracWidth_r : Int) extends IOBundle(bitWidth_r, fracWidth_r) {
  val yReg = Fixed(INPUT, bitWidth_r, fracWidth_r)
}

/** NORMAStage
  This file computes the update for NORMA
  */
class NORMAStage(val bitWidth : Int, val fracWidth : Int, val NORMAtype : Int) extends Module {
  Predef.assert(NORMAtype == 1 || NORMAtype == 2 || NORMAtype == 3,
    "Norma type must be Classification = 1, Novelty = 2, Regression = 3")

  val ZERO = Fixed(0, bitWidth, fracWidth)
  var yC   = Bool(true)
  var yReg = ZERO
  val rhoReg = Reg(init=ZERO)
  val bReg = Reg(init=ZERO)
  val alphaReg = Reg(init=ZERO)
  val ftReg = Reg(init=ZERO)
  val addToDictReg = Reg(init=Bool(false))
  val io = {
    if (NORMAtype == 1) {
      val res = new IOBundle_C(bitWidth, fracWidth); yC = res.yC; res
    } else if (NORMAtype == 3) {
      val res = new IOBundle_R(bitWidth, fracWidth); yReg = res.yReg; res
    } else {
      new IOBundle(bitWidth, fracWidth) }}

  val NORMA = { if (NORMAtype == 1) {
    val res = Module(new NORMAc(bitWidth, fracWidth))
    res.io.bOld := bReg
    bReg := Mux(io.forceNA, bReg, res.io.bNew)
    res.io.y := yC
    res.io.etapos := io.etapos
    res.io.etaneg := io.etaneg
    alphaReg := Mux(res.io.sign, io.etapos, io.etaneg)
    res
  } else if (NORMAtype == 2) {
    val res = Module(new NORMAn(bitWidth, fracWidth))
    alphaReg := io.etapos
    res
  } else {
    val res = Module(new NORMAr(bitWidth, fracWidth))
    res.io.y := yReg
    alphaReg := Mux(res.io.sign, io.etapos, io.etaneg)
    res
  } }

  val sumForceNA = Mux(io.forceNA, io.sum, io.forget*%io.sum)
  val wDForceNA  = Mux(io.forceNA, io.wD, io.forget*%io.wD)

  // Common Section
  val ft = Mux(addToDictReg, (alphaReg*%io.zp) + sumForceNA, sumForceNA + wDForceNA)
  if (NORMAtype == 2)
    ftReg := ft - rhoReg
  else
    ftReg := ft
  NORMA.io.ft := ft
  NORMA.io.rhoOld := rhoReg
  NORMA.io.etanu  := io.etanu
  NORMA.io.etanu1 := io.etanu1
  val newRho = Mux(io.forceNA, rhoReg, NORMA.io.rhoNew)
  rhoReg := newRho
  printf("rhoReg = %x\n", rhoReg)

  val forceNAReg = Reg(init=Bool(true), next=io.forceNA)
  io.forceNAout := forceNAReg
  io.alpha := alphaReg
  io.ft := ftReg
  addToDictReg := Mux(io.forceNA, Bool(false), NORMA.io.addToDict)
  io.addToDict := addToDictReg

}
