package OLK

import Chisel._
import OLK._

/** OLK
  This file links the pipeline stages together
  to form the OLK pipeline
  parameter k : kernel stages
  parameter s : sum stages
  parameter f : no features
  parameter d : no dictionary
  parameter type : OLKc, OLKr, OLKn

  parameter p = k + s + 1

  input example = SFix[f]
  input y       = (0 : OLKn, 1 Bit : OLKc, SFix : OLKr)
  input forceNoAdd = 1 Bit

  output queryVal = SFix

  Blocks:
  Dict_ = Dict(example)
  Manage_ = Manage(y, forceNoAdd)
  Kernel_ = Kernel()
  SumStage_ = SumStage()
  AlphaStage_ = AlphaStage()

  ft = AlphaStage_ft
  OLKn:
      queryVal = ft - 1
  OLKc/OLKr:
      queryVal = ft

  */
