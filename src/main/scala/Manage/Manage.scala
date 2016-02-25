/** Manage.scala -> This file delays inputs until they are needed for the compuation
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

package OLK.Manage

import Chisel._
import scala.collection.mutable.ArrayBuffer

/** Manage
  This file pipelines input so that it will match up for the alpha stage
  parameter p : pipeline stages

  input forceNotAdd = 1 Bit // This is used for queries
  // Optional Inputs
  input y        = (OLKn: 0, OLKc: 1 Bit, OLKr: SFix)
  input e  = Constant (OLKr Only)

  output forceNotAddOut = 1Bit
  output yOut    = (OLKn: 0, OLKc: 1 Bit, OLKr: 0)
  output yPosOut = (OLKn: 0, OLKc: 0, OLKr: SFix)
  output yNegOut = (OLKn: 0, OLKc: 0, OLKr: SFix)

  Registers:
  forceNAReg_0, forceNAReg_1, ... forceNAReg_(p-1)
  yReg_0, yReg_1, ... yReg_(p-2) (OLKr and OLKc Only)
  yPos, YNeg (OLKr Only)

  forceNAReg_0 = forceNotAdd
  forceNAReg_1 = forceNAReg_0
  forceNAReg_2 = forceNAReg_1
  ...
  forceNotAddOut = forceNAReg_(p-1)
  
  OLKc:
      yReg_0 = y
      yReg_1 = yReg_0
      yReg_2 = yReg_1
      ...  
      yReg_(p-2) = yReg(p-3)
      yOut = yReg_(p-2)
  OLKr:
      yReg_0 = y
      yReg_1 = yReg_0
      yReg_2 = yReg_1
      ...  
      yReg_(p-3) = yReg(p-4)
      yPos =   yReg_(p-3) - e
      yNeg = - yReg_(p-3) - e
      yPosOut = yPos
      yNegOut = yNeg
  */
class IOBundle(bitWidth: Int, fracWidth : Int) extends Bundle {
  val forceNAin  = Bool(INPUT)
  val forceNAout = Bool(OUTPUT)

  val forgetin  = Fixed(INPUT, bitWidth, fracWidth)
  val forgetout = Fixed(OUTPUT, bitWidth, fracWidth)
}

// For NORMA
class NORMAIOBundle(bitWidth : Int, fracWidth : Int) extends IOBundle(bitWidth, fracWidth) {
  val eta    = Fixed(INPUT, bitWidth, fracWidth)
  val nu     = Fixed(INPUT, bitWidth, fracWidth)
  val etapos = Fixed(OUTPUT, bitWidth, fracWidth) // = eta
  val etaneg = Fixed(OUTPUT, bitWidth, fracWidth) // = -eta
  val etanu  = Fixed(OUTPUT, bitWidth, fracWidth) // = eta*nu
  val etanu1 = Fixed(OUTPUT, bitWidth, fracWidth) // = -eta*(1-nu)
}

// Only used for Regression
class NORMArIOBundle(bitWidth : Int, fracWidth : Int) extends NORMAIOBundle(bitWidth, fracWidth) {
  val yRegin  = Fixed(INPUT, bitWidth, fracWidth)
  val yRegout = Fixed(OUTPUT, bitWidth, fracWidth)
}

// Only used for Classification
class NORMAcIOBundle(bitWidth : Int, fracWidth : Int) extends NORMAIOBundle(bitWidth, fracWidth) {
  val yCin  = Bool(INPUT)
  val yCout = Bool(OUTPUT)
}

// For OLK
class OLKIOBundle(bitWidth : Int, fracWidth : Int) extends IOBundle(bitWidth, fracWidth) {
  val fracCin  = Fixed(INPUT, bitWidth, fracWidth)
  val fracCout = Fixed(OUTPUT, bitWidth, fracWidth)
}

// Only used for Regression
class OLKrIOBundle(bitWidth : Int, fracWidth : Int) extends OLKIOBundle(bitWidth, fracWidth) {
  val epsilon  = Fixed(INPUT, bitWidth, fracWidth)
  val yRegin   = Fixed(INPUT, bitWidth, fracWidth)
  val yepos    = Fixed(OUTPUT, bitWidth, fracWidth) // OLKr only = (y - epsilon)
  val yeneg    = Fixed(OUTPUT, bitWidth, fracWidth) // OLKr only = -(y + epsilon)
}

// Only used for Classification
class OLKcIOBundle(bitWidth : Int, fracWidth : Int) extends OLKIOBundle(bitWidth, fracWidth) {
  val yCin  = Bool(INPUT)
  val yCout = Bool(OUTPUT)
}

class Manage(val bitWidth : Int, val fracWidth : Int, val stages : Int,
  val isNORMA : Boolean, val appType : Int) extends Module {
  Predef.assert(stages > 1, "There must be atleast two stages in the Manage class")
  Predef.assert(appType == 1 || appType == 2 || appType == 3,
    "appType must be 1 (classification), 2 (Novelty) or 3 (Regression)")

  val ZERO = Fixed(0, bitWidth, fracWidth)

  val io = {
    if ( isNORMA ) {
      val etaposReg = Reg(init=ZERO)
      val etanegReg = Reg(init=ZERO)
      val etanuReg  = Reg(init=ZERO)
      val etanu1Reg = Reg(init=ZERO)

      val normaIO = {
        if ( appType == 1 ) {
          val res   = new NORMAcIOBundle(bitWidth, fracWidth)
          val yCReg = ShiftRegister(res.yCin, stages, Bool(true))
          res.yCout  := yCReg
          res
        } else if ( appType == 2 ) {
          new NORMAIOBundle(bitWidth, fracWidth)
        } else {
          val res  = new NORMArIOBundle(bitWidth, fracWidth)
          val yReg = ShiftRegister(res.yRegin, ZERO, stages, Bool(true))
          res.yRegout := yReg
          res
        }
      }

      etaposReg      := normaIO.eta
      normaIO.etapos := etaposReg
      etanegReg      := -normaIO.eta
      normaIO.etaneg := etanegReg
      etanuReg       := normaIO.eta*%normaIO.nu
      normaIO.etanu  := etanuReg
      etanu1Reg      := etanuReg - normaIO.eta // - eta*(1 - nu)
      normaIO.etanu1 := etanu1Reg

      normaIO
    } else {
      val fracCReg = Reg(init=ZERO)

      val olkIO = {
        if ( appType == 1 ) {
          val res   = new OLKcIOBundle(bitWidth, fracWidth)
          val yCReg = ShiftRegister(res.yCin, stages, Bool(true))
          res.yCout := yCReg
          res
        } else if ( appType == 2 ) {
          new OLKIOBundle(bitWidth, fracWidth)
        } else {
          val res  = new OLKrIOBundle(bitWidth, fracWidth)
          val yReg = ShiftRegister(res.yRegin, stages - 1, Bool(true))
          val yeposReg = Reg(init=ZERO)
          val yenegReg = Reg(init=ZERO)
          yeposReg  := yReg - res.epsilon
          yenegReg  := - res.epsilon - yReg
          res.yepos := yeposReg
          res.yeneg := yenegReg
          res
        }
      }

      fracCReg       := olkIO.fracCin
      olkIO.fracCout := fracCReg

      olkIO
    }
  }

  // Common
  val forceReg = ShiftRegister(io.forceNAin, Bool(true), stages)
  val forgetReg = Reg(init=ZERO)

  io.forceNAout := forceReg

  forgetReg    := io.forgetin
  io.forgetout := forgetReg
}
