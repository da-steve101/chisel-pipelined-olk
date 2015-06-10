package OLK.NORMAStage

import Chisel._
import FixedPoint._

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
//class NORMAc(val bitWidth : Int, val intLength : Int) extends Module {

//}

//class NORMAcTests(c: NORMAc) extends Tester(c) {

//}
