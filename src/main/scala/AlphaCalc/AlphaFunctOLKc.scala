package OLK.AlphaCalc

import Chisel._
import FixedPoint._

/** AlphaFunctOLKc
  This file computes the following 
  input ft => SFix
  input y => +/-1
  input 1/(1+r) => Constant
  input C/(1+r) => Constant
  output out => SFix
  output alpha => SFix  // Used for decision to add or not
  alpha(ft) = 1 - ft*(y/(1+r))
  if alpha(ft) > C/(1+r)
     alpha(ft) = C/(1+r)
  alpha = alpha(ft)
  out = (y == 1) ? alpha(ft) : -alpha(ft)
  */
class AlphaFunctOLKc extends Module {

}

class AlphaFunctOLKcTests extends Tester(c) {

}
