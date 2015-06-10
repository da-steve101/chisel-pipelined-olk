package OLK.Dict

import Chisel._
import OLK._
import cla.types._

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
class Dict(val bitWidth : Int, val fracWidth : Int, val dictSize : Int,
           val features : Int, val pipelineStages : Int) extends Module {
  val io = new Bundle {
    val alpha     = Fixed(INPUT, bitWidth, fracWidth)
    val forget    = Fixed(INPUT, bitWidth, fracWidth)
    val example   = Vec.fill(features){Fixed(INPUT, bitWidth, fracWidth)}
    val addToDict = Bool(INPUT)

    val currentDict  = Vec.fill(dictSize){Vec.fill(features){Fixed(INPUT, bitWidth, fracWidth)}}
    val currentAlpha = Vec.fill(dictSize){Fixed(INPUT, bitWidth, fracWidth)}
    val currentPipeline = Vec.fill(pipelineStages){Vec.fill(features){Fixed(INPUT, bitWidth, fracWidth)}}
  }

  // Registers
  val zero        = Fixed(0, bitWidth, fracWidth)
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
  poke(c.io.alpha, BigInt(0))
  poke(c.io.forget, BigInt(1 << (c.bitWidth - c.fracWidth)))
  poke(c.io.addToDict, Bool(false).litValue())
  for (p <- 0 until c.pipelineStages){
    for (f <- 0  until c.features)
      poke(c.io.example(f), BigInt((1 << (c.bitWidth - c.fracWidth)) + p))
    step(1)
    for (p2 <- 0 until c.pipelineStages) {
      var x = 0
      if (p2 < p)
        x = p - p2 + (1 << (c.bitWidth - c.fracWidth))
      for (f <- 0 until c.features)
        expect(c.io.currentPipeline(p2)(f), BigInt(x))
    }
  }
  // Pipeline now full, test dict
  poke(c.io.alpha, BigInt(1 << (c.bitWidth - c.fracWidth)))      // 1
  poke(c.io.forget, BigInt(1 << (c.bitWidth - c.fracWidth - 1))) // 0.5
  poke(c.io.addToDict, Bool(true).litValue())
  step(1)
  for (p <- 0 until c.pipelineStages) {
    if (p == (c.pipelineStages - 1)) {
      // in the last case test if the example is not added
      poke(c.io.addToDict, Bool(false).litValue())
      //p <- (c.pipelineStages - 2)
    }
    for (d <- 0 until (p+1)) {
      // Check weights
      val alphai = BigInt(((1 << (c.bitWidth - c.fracWidth))) >> d)
      expect(c.io.currentAlpha(d), alphai)
      // Check dictionary
      for (f <- 0 until c.features) {
        val x = BigInt(((1 << (c.bitWidth - c.fracWidth)) + p + 1 - d) >> d)
        expect(c.io.currentDict(d)(f), x)
      }
    }
    step(1)
  }
}
