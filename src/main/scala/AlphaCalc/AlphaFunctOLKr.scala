package OLK

import Chisel._
import FixedPoint._

/** AlphaFunctOLKr
  This file computes the following 
  input ft => SFix
  input   y - e => SFix
  input - y - e => SFix
  input 1/(1+r) => Constant
  input C/(1+r) => Constant
  output out => SFix
  output addToDict => 1 Bit
  alpha_1(ft) = (  y - e) - ft*(1/(1+r))
  alpha_2(ft) = (- y - e) + ft*(1/(1+r))
  if alpha_12(ft) > C/(1+r)
     alpha_12(ft) = C/(1+r)

  if alpha_12(ft) < 0
     alpha_12(ft) = 0
  if alpha_1(ft) == 0 && alpha_2(ft) == 0
     addToDict = 0
  else
     addToDict = 1

  out = alpha_2(ft) - alpha_1(ft)
  */
class AlphaFunctOLKr extends Module {

}

class AlphaFunctOLKrTests extends Tester(c) {

}
