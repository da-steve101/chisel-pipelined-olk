package OLK.AlphaCalc

import Chisel._
import FixedPoint._

/** AlphaStage
  This file computes the value of alpha for OLKn/c/r
  input Zp => SFix
  input sum => SFix
  input Wd => SFix
  input 1/(1+r) => Constant
  input C/(1+r) => Constant
  input forceNotAdd => 1 Bit
  // Optional Inputs
  input y        => (OLKn: 0, OLKc: 1 Bit, OLKr: 0)
  input (y - e)  => (OLKn: 0, OLKc: 0,     OLKr: SFix)
  input (-y - e) => (OLKn: 0, OLKc: 0,     OLKr: SFix)
  // Outputs
  output alpha => SFix  // Used for decision to add or not
  output ft    => SFix  // Used for query outputs
  output addToDict => 1 bit

  Registers:
  alphaReg      => SFix
  uAlphaReg     => SFix  (OLKc Only)
  addToDictReg  => 1 Bit (OLKr Only)
  forceNoAddReg => 1 Bit
  ftReg         => SFix  (For Query results)
  
  Logic:
  forceNoAddReg = forceNoAdd
  ft = alphaReg*Zp + sum*(1/(1+r))
  ft_alt = sum + Wd
  ftReg = (addToDict) ? ft : ft_alt
  -----------------
  alpha = alphaReg
  
  OLKc:
      OLKc_ = AlphaFunctOLKc()     // Block for ft
      OLKc_alt_ = AlphaFunctOLKc() // Block for ft_alt
      alphaReg = (addToDict) ? (OLKc_out) : (OLKc_alt_out)
      uAlphaReg = (addToDict) ? (OLKc_alpha) : (OLKc_alt_alpha)
      --------------------------------------------------------
      addToDict = !forceNotAdd && (uAlphaReg > 0)
  OLKn:
      OLKn_ = AlphaFunctOLKn()     // Block for ft
      OLKn_alt_ = AlphaFunctOLKn() // Block for ft_alt
      alphaReg = (addToDict) ? (OLKn_out) : (OLKn_alt_out)
      --------------------------------------------------------
      addToDict = !forceNotAdd && (alphaReg > 0)
  OLKr:
      OLKr_ = AlphaFunctOLKr()     // Block for ft
      OLKr_alt_ = AlphaFunctOLKr() // Block for ft_alt
      addToDictReg = (addToDict) ? (OLKr_addToDict) : (OLKr_alt_addToDict)
      alphaReg = (addToDict) ? (OLKr_out) : (OLKr_alt_out)
      --------------------------------------------------------
      addToDict = !forceNotAdd && (addToDictReg)
  */
class AlphaStage extends Module {

}

class AlphaStage extends Tester(c) {

}
