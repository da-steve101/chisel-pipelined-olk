package OLK.Manage

import Chisel._
import OLK._

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
