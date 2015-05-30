package OLK

import Chisel._
import OLK._

/** l2norm
  This file computes the l2norm
  parameter f : no features
  parameter s : no stages

  input x1 = SFix[f]
  input x2 = SFix[f]

  output z = SFix

  Logic:
  z = ||x1-x2||^2 over s stages

  NB: may need to start at intermediate stages
      or have an abort process etc

  */
