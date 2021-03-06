/** Dict.scala -> This file manages the dictionary for OLK
Copyright (C) 2015 Stephen Tridgell

This file is part of a pipelined OLK application.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this code.  If not, see <http://www.gnu.org/licenses/>.
*/

package OLK.Dict

import Chisel._
import scala.collection.mutable.ArrayBuffer


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
  val features : Int, val pipelineStages : Int, val isNORMA : Boolean) extends Module {
  val io = new Bundle {
    val forceNA   = Bool(INPUT)
    val alpha     = Fixed(INPUT, bitWidth, fracWidth)
    val forget    = Fixed(INPUT, bitWidth, fracWidth)
    val example   = Vec.fill(features){Fixed(INPUT, bitWidth, fracWidth)}
    val addToDict = Bool(INPUT)

    val currentDict  = Vec.fill(dictSize){Vec.fill(features){Fixed(OUTPUT, bitWidth, fracWidth)}}
    val currentAlpha = Vec.fill(dictSize){Fixed(OUTPUT, bitWidth, fracWidth)}
    val currentPipeline = Vec.fill(pipelineStages){Vec.fill(features){Fixed(OUTPUT, bitWidth, fracWidth)}}
  }

  // Registers
  val pipelinedEx = RegInit( Vec.fill( pipelineStages ){Vec.fill( features ){ Fixed(0.0, bitWidth, fracWidth)}})
  val dict        = RegInit( Vec.fill( dictSize ){ Vec.fill( features ){ Fixed(0.0, bitWidth, fracWidth) }})
  val weights     = RegInit( Vec.fill( dictSize ){ Fixed(0.0, bitWidth, fracWidth) } )
  val forgetWeights = weights.toList.map(x => { io.forget*%x })
  val ZERO = Fixed(0, bitWidth, fracWidth)

  io.currentDict := dict
  io.currentPipeline := pipelinedEx
  io.currentAlpha := weights

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
    if (isNORMA)
      weights(0) := Mux(io.forceNA, weights(0), forgetWeights(0))
    else
      weights(0) := weights(0)
  }

  for (d <- 0 until (dictSize - 1)) {
    when (io.addToDict) {
      weights(d+1) := forgetWeights(d)
    } .otherwise {
      if (isNORMA)
        weights(d+1) := Mux(io.forceNA, weights(d+1), forgetWeights(d+1))
      else
        weights(d+1) := weights(d+1)
    }
  }
}
