package OLK.Kernel

import Chisel._
import OLK._

/** Kernel
  This file evaluates a kernel function in parallel and pipelined
  parameter p : pipeline cycles
  parameter d : dictionary size
  parameter f : feature size
  parameter k : kernel pipelined cycles

  input example  = SFix
  input currentPipeline = SFix[f][p]
  input currentDictionary = SFix[f][d]
  input addToDict =  1 Bit
  
  output zi = SFix[p-k]
  output vi = SFix[d]

  Registers:
  ziReg, viReg

  Logic:
  in k cycles compute a kernel function with new example
  
  ziReg = K(example, currentPipeline[0:end-k])
  
  combinedEx = [currentPipeline[end-k+1:end] currentDictionary]
  each cycle drop (addToDict) ? currentDictionary[end] : currentPipeline[end]
  viReg = K(example, combinedEx)

  zi = ziReg
  vi = viReg

  */
