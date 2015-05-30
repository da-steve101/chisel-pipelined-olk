package OLK.Dict

import Chisel._
import OLK._

/** Dict
  This file manages the Dictionary
  parameter d : dictionary size
  parameter f : no features
  parameter p : pipeline stages

  input alpha = SFix
  input 1/(1+r) = Constant
  input example = SFix[F]
  input addToDict = 1 Bit
  
  output currentDict = SFix[f][d]
  output currentAlpha = SFix[d]
  output currentPipeline = SFix[f][p]

  Registers:
  pipelineEx = SFix[f][p]
  dict  = SFix[f][d]
  weights = SFix[d]

  Logic:
  pipelineEx = [example pipelineEx[0:end-1]]
  dict = (addToDict) ? [pipelineEx[end] dict[0:end-1]] : dict
  weights = (addToDict) ? [alpha (1/(1+r))*weights[0:end-1]] : weights

  currentPipeline = pipelineEx
  currentAlpha = weights
  currentDict = dict
  */
