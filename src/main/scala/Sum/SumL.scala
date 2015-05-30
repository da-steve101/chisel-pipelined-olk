package OLK.Sum

import Chisel._
import OLK._

/** SumL
  This block sums the stages in the pipeline
  parameter s : stages
  
  input z_i => SFix[s+1]
  input alpha => SFix
  input 1/(1+r) => Constant
  input addToDict => 1 Bit
  
  output z_p1 => SFix
  output zp   => SFix
  output sumL => SFix
  
  Registers:
  Stage_0[s+1], Stage_1[s], ... Stage_(s-1)[2]
  SumL_0, SumL_1, ... SumL_(s-1)

  Logic:
  Stage_0 = z_i[1:end-1] 
  SumL_0 = (addToDict) ? (z_i[end]*alpha) : 0
  for i = 0:(s-1){
      Stage_(i+1)[1:end] = Stage_(i)[1:end-1]
      SumL_(i+1) = (addToDict) ? (Stage_(i)[end]*alpha + (1/(1+r))*SumL_(i)) : (SumL_(i))
  }
  z_p1 = Stage_(s-1)[0]
  zp   = Stage_(s-1)[1]
  sumL = SumL_(s-1)
  
  */
