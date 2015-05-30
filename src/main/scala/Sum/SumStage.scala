package OLK.Sum

import Chisel._
import OLK._

/** SumStage
  This file implements the summation stage
  using SumL and SumR

  parameter s : stages
  parameter d : dictionary size  
  parameter (1/(1+r))

  input zi     = SFix[s + 1]
  input vi     = SFix[d]
  input alpha  = SFix
  input addToDict => 1 Bit
  input alphai = SFix[d]

  output zp = SFix
  output wD = SFix
  output Sum = SFix

  constant Lookup table:
  (1/(1+r)), (1/(1+r))^2, (1/(1+r))^3 ... (1/(1+r))^s
  
  Registers:
  zpReg  = SFix
  wDReg  = SFix
  SumReg = SFix

  Logic:
  SumL_ = SumL[stages = s-1]()
  SumR_ = SumR[stages = s-1, dict = d]()

  sumA = SumL_zp*alpha + SumL_SumL*(1/(1+r)) + SumR_SumR*(1/(1+r))^SumR_q
  sumB = (SumR_SumR + SumR_wD1)*(1/(1+r))^SumR_q + SumL_SumL

  SumReg = (addToDict) ? (sumA) : (sumB)
  zpReg = SumL_z_p1
  wDReg = (addToDict) ? SumR_wD*(1/(1+r))^SumR_q : SumR_wD1*(1/(1+r))^SumR_q

  zp = zpReg
  wD = wDReg
  Sum = SumReg

  */
