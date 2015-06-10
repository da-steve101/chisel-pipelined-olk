package OLK.Dict

import Chisel._
import OLK._
import FixedPoint._

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
class Dict(val bitWidth : Int, val intLength : Int, val dictSize : Int,
           val features : Int, val pipelineStages : Int) extends Module {
  val io = new Bundle {
    val alpha     = SFix(intLength, bitWidth).asInput
    val forget    = SFix(intLength, bitWidth).asInput
    val example   = Vec.fill(features){SFix(intLength, bitWidth).asInput}
    val addToDict = Bool(INPUT)

    val currentDict  = Vec.fill(dictSize){Vec.fill(features){SFix(intLength, bitWidth).asInput}}
    val currentAlpha = Vec.fill(dictSize){SFix(intLength, bitWidth).asInput}
    val currentPipeline = Vec.fill(pipelineStages){Vec.fill(features){SFix(intLength, bitWidth).asInput}}
  }

  // Registers
  val zero        = new SFix(intLength, SInt(0, width=bitWidth))
  val pipelinedEx = Vec.fill(pipelineStages){Vec.fill(features){Reg(init=zero)}}
  val dict        = Vec.fill(dictSize){Vec.fill(features){Reg(init=zero)}}
  val weights     = Vec.fill(dictSize){Reg(init=zero)}
  val forgetWeights = Vec.fill(dictSize - 1){zero}

  for (f <- 0 until features) {
    for (p <- 0 until pipelineStages)
      io.currentPipeline(p)(f) := pipelinedEx(p)(f)
    for (d <- 0 until dictSize)
      io.currentDict(d)(f) := dict(d)(f)
  }
  for (d <- 0 until dictSize)
    io.currentAlpha(d) := weights(d)

  for (f <- 0 until features) {
    // Pipeline
    pipelinedEx(0)(f) := io.example(f)
    for (p <- 0 until (pipelineStages - 1))
      pipelinedEx(p+1)(f) := pipelinedEx(p)(f)

    // Dictionary
    when (io.addToDict) { 
      dict(0)(f) := pipelinedEx(pipelineStages - 1)(f)
      for (d <- 0 until (dictSize - 1))
        dict(d+1)(f) := dict(d)(f)
    } .otherwise {
      for (d <- 0 until (dictSize))
        dict(d)(f) := dict(d)(f)
    }
  }

  when (io.addToDict) {
    weights(0) := io.alpha
  } .otherwise {
    weights(0) := weights(0)
  }

  for (d <- 0 until (dictSize - 1)) {
    forgetWeights(d) := io.forget*weights(d)
    when (io.addToDict) {
      weights(d+1) := forgetWeights(d)
    } .otherwise {
      weights(d+1) := weights(d+1)
    }
  }
}

class DictTests(c : Dict) extends Tester(c) {
  poke(c.io.alpha.raw, BigInt(0))
  poke(c.io.forget.raw, BigInt(1 << (c.bitWidth - c.intLength)))
  poke(c.io.addToDict, Bool(false).litValue())
  for (p <- 0 until c.pipelineStages){
    for (f <- 0  until c.features)
      poke(c.io.example(f).raw, BigInt((1 << (c.bitWidth - c.intLength)) + p))
    step(1)
    for (p2 <- 0 until c.pipelineStages) {
      var x = 0
      if (p2 < p)
        x = p - p2 + (1 << (c.bitWidth - c.intLength))
      for (f <- 0 until c.features)
        expect(c.io.currentPipeline(p2)(f).raw, BigInt(x))
    }
  }
  // Pipeline now full, test dict
  poke(c.io.alpha.raw, BigInt(1 << (c.bitWidth - c.intLength)))      // 1
  poke(c.io.forget.raw, BigInt(1 << (c.bitWidth - c.intLength - 1))) // 0.5
  poke(c.io.addToDict, Bool(true).litValue())
  step(1)
  for (p <- 0 until c.pipelineStages) {
    if (p == (c.pipelineStages - 1)) {
      // in the last case test if the example is not added
      poke(c.io.addToDict, Bool(false).litValue())
      p <- (c.pipelineStages - 2)
    }
    for (d <- 0 until (p+1)) {
      // Check weights
      val alphai = BigInt(((1 << (c.bitWidth - c.intLength))) >> d)
      expect(c.io.currentAlpha(d).raw, alphai)
      // Check dictionary
      for (f <- 0 until c.features) {
        val x = BigInt(((1 << (c.bitWidth - c.intLength)) + p + 1 - d) >> d)
        expect(c.io.currentDict(d)(f).raw, x)
      }
    }
    step(1)
  }
}
