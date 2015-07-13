/** Gaussian.scala -> This file computes Gaussian kernel function in fixed point
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

package OLK.Kernel

import Chisel._
import OLK._
import scala.collection.mutable.ArrayBuffer

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
class Gaussian(val bitWidth : Int, val fracWidth : Int, val dictSize : Int, val features : Int,
  val pCycles : Int, val stages : ArrayBuffer[Boolean], val tableSize : Int) extends Module {
  Predef.assert(features > 0, "There must be atleast one feature")
  def log2Features: Int = { log2Up(features) }
  //Predef.assert(features == 1 << log2Features, "Features must be a power of 2")
  Predef.assert(stages.length == log2Features + 2 + 5,
    "The length of the stages ArrayBuffer into l2norm must be " + (log2Features + 2 + 5) + " for " + features + " features")
  def l2Cycles  : Int = { stages.take(log2Features + 2).count(_ == true) } // Count the number of stages in the L2 evaluation
  def powCycles : Int = { stages.takeRight(5).count(_ == true) }// Count the number of stages in the power evaluation
  def kCycles   : Int = { powCycles + l2Cycles } // Count the number of stages in the kernel evaluation

  val ZERO = Fixed(0, bitWidth, fracWidth)

  val io = new Bundle {
    val gamma      = Fixed(INPUT, bitWidth, fracWidth)
    val example    = Vec.fill(features){Fixed(INPUT, bitWidth, fracWidth)}
    val pipeline   = Vec.fill(pCycles){Vec.fill(features){Fixed(INPUT, bitWidth, fracWidth)}}
    val dictionary = Vec.fill(dictSize){Vec.fill(features){Fixed(INPUT, bitWidth, fracWidth)}}
    val addToDict  = Bool(INPUT)

    val pipelineOut = Vec.fill(pCycles - kCycles){Fixed(OUTPUT, bitWidth, fracWidth)}
    val dictOut     = Vec.fill(dictSize){Fixed(OUTPUT, bitWidth, fracWidth)}
  }
  val l2Result = ArrayBuffer[Fixed]()

  val L2NormModules = ArrayBuffer.fill(pCycles) {
    val l2Mod = Module(new L2Norm(bitWidth, fracWidth, stages.take(log2Features + 2), features))
    l2Mod.io.x1 := io.example
    l2Mod.io.addToDict := Bool(true)
    l2Result += l2Mod.io.result
    l2Mod
  }
  L2NormModules.appendAll(ArrayBuffer.fill(dictSize) {
    val l2Mod = Module(new L2Norm(bitWidth, fracWidth, stages.take(log2Features + 2), features))
    l2Mod.io.x1 := io.example
    l2Mod.io.addToDict := io.addToDict
    l2Result += l2Mod.io.result
    l2Mod
  })
  for (i <- 0 until pCycles)
    L2NormModules(i).io.x2 := io.pipeline(i)
  for (i <- 0 until dictSize)
    L2NormModules(i + pCycles).io.x2 := io.dictionary(i)

  val L2NormConnections = (L2NormModules.dropRight(1) zip L2NormModules.drop(1)).map(pair => {
    pair._2.io.subalt   := pair._1.io.subout
    pair._2.io.sqralt   := pair._1.io.sqrout
    pair._2.io.adderalt := pair._1.io.adderout
  })

  val Pow2Modules = ArrayBuffer.fill(dictSize + pCycles - l2Cycles){Module(new Pow2(bitWidth, fracWidth, stages.takeRight(5), tableSize))}

  val pow2Inputs = (Pow2Modules zip l2Result.drop(l2Cycles)).zipWithIndex.map( pairwithindex => {
    val pair = pairwithindex._1
    pair._1.io.x := pair._2
    pair._1.io.gamma := io.gamma
    // if in pipeline the force true
    if ( pairwithindex._2 < (pCycles - l2Cycles) )
      pair._1.io.addToDict := Bool(true)
    else
      pair._1.io.addToDict := io.addToDict
  })

  val Pow2Connections = (Pow2Modules.dropRight(1) zip Pow2Modules.drop(1)).map(pair => {
    pair._2.io.xValAlt    := pair._1.io.xValOut
    pair._2.io.xintAlt    := pair._1.io.xintOut
    pair._2.io.xFracAlt   := pair._1.io.xFracOut
    pair._2.io.gradTabAlt := pair._1.io.gradTabOut
    pair._2.io.offTabAlt  := pair._1.io.offTabOut
    pair._2.io.xint1Alt   := pair._1.io.xint1Out
    pair._2.io.limitAlt   := pair._1.io.limitOut
    pair._2.io.gradAlt    := pair._1.io.gradOut
    pair._2.io.offAlt     := pair._1.io.offOut
    pair._2.io.xint2Alt   := pair._1.io.xint2Out
    pair._2.io.yFracAlt   := pair._1.io.yFracOut
    pair._2.io.yAlt       := pair._1.io.yOut
  })
  val firstPipe = Pow2Modules.head
  firstPipe.io.xValAlt    := ZERO
  firstPipe.io.xintAlt    := ZERO
  firstPipe.io.xFracAlt   := ZERO
  firstPipe.io.gradTabAlt := ZERO
  firstPipe.io.offTabAlt  := ZERO
  firstPipe.io.xint1Alt   := ZERO
  firstPipe.io.limitAlt   := Bool(false)
  firstPipe.io.gradAlt    := ZERO
  firstPipe.io.offAlt     := ZERO
  firstPipe.io.xint2Alt   := ZERO
  firstPipe.io.yFracAlt   := ZERO
  firstPipe.io.yAlt       := ZERO

  for( i <- 0 until (pCycles - kCycles))
    io.pipelineOut(i) := Pow2Modules(i + powCycles).io.y
  for( i <- 0 until dictSize)
    io.dictOut(i) := Pow2Modules(i + powCycles + pCycles - kCycles).io.y
}

class GaussianTests(c : Gaussian) extends Tester(c) {
  val r = scala.util.Random

  val cycles = 3*(c.kCycles + 1)

  val pipeline   = ArrayBuffer.fill(c.pCycles){ArrayBuffer.fill(c.features){BigInt(r.nextInt(1 << c.fracWidth))}}
  val dictionary = ArrayBuffer.fill(c.dictSize){ArrayBuffer.fill(c.features){BigInt(r.nextInt(1 << c.fracWidth))}}
  val addToDict = ArrayBuffer.fill(cycles + c.kCycles){ r.nextInt(2) == 1 }
  val gamma = BigInt(r.nextInt(1 << c.fracWidth))
  for (j <- 0 until c.features) {
    for (i <- 0 until c.pCycles)
      poke(c.io.pipeline(i)(j), pipeline(i)(j))
    for (i <- 0 until c.dictSize)
      poke(c.io.dictionary(i)(j), dictionary(i)(j))
  }
  poke(c.io.gamma, gamma)

  // Pad inital cycles with zeros
  // expectedPipeline(cycle)(index)
  val expectedPipeline = ArrayBuffer.fill(c.kCycles){ArrayBuffer.fill(c.pCycles - c.kCycles)(BigInt(0))}
  // expectedDict(cycle)(index)
  val expectedDict = ArrayBuffer.fill(c.kCycles){ArrayBuffer.fill(c.dictSize)(BigInt(0))}

  val log2Table = { (scala.math.log10(c.tableSize)/scala.math.log10(2)).ceil.toInt }

  // Generate Table for linear interpolation
  val gradients   = new ArrayBuffer[BigInt]()
  val offsets     = new ArrayBuffer[BigInt]()
  // Fixed point increment
  val increment   = 1.0 / (1 << log2Table)
  val tableEnd    = 1.0
  var x = 0.0
  // NOTE: x is positive, therefore gradient is negitive
  while (x < tableEnd) {
    // m = (y1 - y2)/(x1 - x2)
    val m = -(scala.math.pow(2, - x) - scala.math.pow(2,- x - increment))/increment
    // convert to Fixed
    gradients += BigInt((m  * (1 << c.fracWidth)).toLong)
    // b = y1 - m*x1
    val b = scala.math.pow(2, - x) - m*x
    // convert to Fixed
    offsets += BigInt((b * (1 << c.fracWidth)).toLong)
    x += increment
  }

  for ( cyc <- 0 until cycles ) {
    val example = ArrayBuffer.fill(c.features){BigInt(r.nextInt(1 << c.fracWidth))}
    // calculate the outputs for this example
    val subPipe = pipeline.map(x => {
      val subAry = new ArrayBuffer[BigInt]()
      for (i <- 0 until c.features)
        subAry += x(i) - example(i)
      subAry
    })
    val subDict = dictionary.map(x => {
      val subAry = new ArrayBuffer[BigInt]()
      for (i <- 0 until c.features)
        subAry += x(i) - example(i)
      subAry
    })
    val l2Pipe = subPipe.map(x => { x.map(y => { ((y*y) >> c.fracWidth) }).sum }).map( x => { (gamma*x) >> c.fracWidth })
    val l2Dict = subDict.map(x => { x.map(y => { ((y*y) >> c.fracWidth) }).sum }).map( x => { (gamma*x) >> c.fracWidth })
    val xIntPipe = l2Pipe.map(x => { x >> c.fracWidth }).map(x => x.toInt)
    val xIntDict = l2Dict.map(x => { x >> c.fracWidth }).map(x => x.toInt)
    val xTabPipe = l2Pipe.map(x => { (x >> (c.fracWidth - log2Table)) & ((1 << log2Table) - 1) }).map(x => x.toInt)
    val xTabDict = l2Dict.map(x => { (x >> (c.fracWidth - log2Table)) & ((1 << log2Table) - 1) }).map(x => x.toInt)
    val xFracPipe = l2Pipe.map(x => { x & ((1 << c.fracWidth) - 1) })
    val xFracDict = l2Dict.map(x => { x & ((1 << c.fracWidth) - 1) })
    val yPipe    = (xFracPipe zip xTabPipe).map(pair => { ((gradients(pair._2)*pair._1) >> c.fracWidth) + offsets(pair._2) })
    val yDict    = (xFracDict zip xTabDict).map(pair => { ((gradients(pair._2)*pair._1) >> c.fracWidth) + offsets(pair._2) })
    val yOutPipe = (yPipe zip xIntPipe).map(pair => { pair._1 >> pair._2 })

    val yPipeAdded = new ArrayBuffer[BigInt]()
    for (i <- 0 until c.kCycles) {
      if (addToDict(c.kCycles - 1 - i + cyc))
        yPipeAdded += yOutPipe(yOutPipe.length - c.kCycles + i)
    }

    val yOutDictTmp = (yDict zip xIntDict).map(pair => { pair._1 >> pair._2 })
    val yOutDict = yOutDictTmp.take(c.dictSize - yPipeAdded.length)

    // Add the values for k cycles in advance
    expectedPipeline += yOutPipe.take(c.pCycles - c.kCycles)
    yPipeAdded.appendAll(yOutDict)
    expectedDict     += yPipeAdded

    poke(c.io.addToDict, Bool(addToDict(cyc)).litValue())
    for (i <- 0 until c.features)
      poke(c.io.example(i), example(i))

    if (cyc >= c.kCycles) {
      for (i <- 0 until (c.pCycles - c.kCycles))
        expect(c.io.pipelineOut(i), expectedPipeline(cyc)(i))
      for (i <- 0 until (c.dictSize))
        expect(c.io.dictOut(i), expectedDict(cyc)(i))
    }

    step(1)
  }
}
