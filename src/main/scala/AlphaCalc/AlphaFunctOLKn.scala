package OLK

import Chisel._
import FixedPoint._

/** AlphaFunctOLKn
  This file computes the following:
  input ft => SFix
  input 1/(1+r) => Constant
  input C/(1+r) => Constant
  output out => SFix
  alpha(ft) = 1 - ft*(1/(1+r))
  if alpha(ft) > C/(1+r)
     alpha(ft) = C/(1+r)
  out = alpha(ft)
  */
class AlphaFunctOLKn extends Module {

}

class AlphaFunctOLKnTests extends Tester(c) {

}
