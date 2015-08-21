/** L2Norm.scala -> This file computes the L2 norm in fixed point
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

import scala.collection.mutable.MutableList
import scala.collection.mutable.ArrayBuffer

/** L2Norm
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
class L2Norm(val bitWidth : Int, val fracWidth : Int, val stages : ArrayBuffer[Boolean], val features : Int) extends Module {
  Predef.assert(features > 0, "There must be atleast one feature")
  def log2Features: Int = { (scala.math.log10(features)/scala.math.log10(2)).ceil.toInt }
  //Predef.assert(features == 1 << log2Features, "Features must be a power of 2")
  Predef.assert(stages.length == log2Features + 2,
    "The length of the stages ArrayBuffer into l2norm must be " + (log2Features + 2) + " for " + features + " features")

  // A function to create list groupings. ie) L(1,2,3,4) => L(L(1,2), L(3,4))
  def group[A](list : List[A], size : Int) : List[List[A]] = list.foldLeft( (List[List[A]](), 0) ) { (r, c) =>
    r match {
      case (head :: tail, num) =>
        if (num < size) ( (c :: head) :: tail, num + 1)
        else            ( List(c) :: head :: tail, 1)
      case (Nil, num) => (List(List(c)),1)
    }
  }._1.foldLeft(List[List[A]]())( (r,c) => c.reverse :: r)

  val ZERO = Fixed(0, bitWidth, fracWidth)
    def zipList(l1 : List[Fixed], l2 : List[Fixed]) =  (l1.padTo(l2.length, ZERO) zip l2.padTo(l1.length, ZERO))
    //def zipList(l1 : List[Fixed], l2 : List[Fixed]) =  l1 zip l2


  val io = new Bundle {
    val x1        = Vec.fill(features){Fixed(INPUT, bitWidth, fracWidth)}
    val x2        = Vec.fill(features){Fixed(INPUT, bitWidth, fracWidth)}
    val subalt    = Vec.fill(features){Fixed(INPUT, bitWidth, fracWidth)} // Optional input for alternative subtration
    val sqralt    = Vec.fill(features){Fixed(INPUT, bitWidth, fracWidth)} // Optional input for alternative sqr
    /* 
     Create adderalt as a vec of (1 << (log2Features -1)) + (1 << (log2Features - 2)) + ... etc
     So that 0:[(1 << (log2Features - 1)) - 1] is the first stage
     So that [(1 << (log2Features - 1))]:[(1 << (log2Features - 2)) - 1] is the second stage ... etc
     */
    val adderalt  = Vec.fill((1 << log2Features) - 1){Fixed(INPUT, bitWidth, fracWidth)}
    val addToDict = Bool(INPUT)

    val subout    = Vec.fill(features){Fixed(OUTPUT, bitWidth, fracWidth)}
    val sqrout    = Vec.fill(features){Fixed(OUTPUT, bitWidth, fracWidth)}
    val adderout  = Vec.fill((1 << log2Features) - 1){Fixed(OUTPUT, bitWidth, fracWidth)} // same as adderalt
    val result    = Fixed(OUTPUT, bitWidth, fracWidth) // = |x1-x2|^2
  }

  val adderaltList = io.adderalt.toList
  val adderoutList = io.adderout.toList

  // compute x1(i) - x2(i)
  val subStage = (io.x1 zip io.x2).map(pair => pair._1 - pair._2)
  // connect to substage output
  val suboutConn = (io.subout zip subStage).map(pair => (pair._1 := pair._2))

  // Square the elements
  val sqrStage = {
    if (stages(0)) {
      // Optional stage to mux in alternative input
      val muxStageSub = (io.subalt zip subStage).map(pair => (Mux(io.addToDict, pair._1, pair._2)))
      // Optional Register here
      val regStageSub = muxStageSub.map(pair => ({ val reg = Reg(init=Fixed(0.0, bitWidth, fracWidth)); reg := pair; reg }))
      regStageSub.map(x => (x*%x))
    } else {
      subStage.map(x => (x*%x)) } }
  // connect sqrstage output
  val sqroutConn = (io.sqrout zip sqrStage).map(pair => (pair._1 := pair._2))

  // Adder Tree
  // Tree Roots
  val adderTree = {
    if (stages(1)) {
      // Optional stage to mux in alternative input
      val muxStageSqr = (io.sqralt zip sqrStage).map(pair => (Mux(io.addToDict, pair._1, pair._2)))
      // Optional Register here
      val regStageSqr = muxStageSqr.map(pair => ({ val reg = Reg(init=Fixed(0.0, bitWidth, fracWidth)); reg := pair; reg }))
      MutableList(group(regStageSqr.toList,2))
    } else {
      MutableList(group(sqrStage.toList,2)) }}
  val adderAdditionTree = MutableList(adderTree(0).map(pair => if(pair.length == 1) pair(0) else (pair(0) + pair(1))))
  // Connection out for adder
  val adderoutConn = MutableList(zipList(adderoutList.take(1<<(log2Features-1)), adderAdditionTree(0)).map(pair => (pair._1 := pair._2)))
  // Optional Mux and Register stages
  val adderMuxs = {
    if (stages(2) ) {
      MutableList(zipList(adderaltList.take(1<<(log2Features-1)), adderAdditionTree(0)).map(pair => (Mux(io.addToDict, pair._1, pair._2))))
    } else {
      new MutableList[List[Fixed]]()  } }
  val adderRegs = {
    if ( stages(2) ) {
      MutableList(adderMuxs(0).map(pair => ({ val reg = Reg(init=Fixed(0.0, bitWidth, fracWidth)); reg := pair; reg })))
    } else {
      new MutableList[List[Fixed]]()  } }

  // GROW TREE GROW!!!!
  var adderPos = 1 << (log2Features - 1) // A counter to keep track of adder pos
  for (i <- 0 until (log2Features - 1)) {
    val noAdderVals = 1 << (log2Features - 2 - i)
    adderTree += ({ if (stages(i + 2)) { group(adderRegs.last.toList, 2) } else { group(adderAdditionTree.last.toList, 2) } }).toList
    adderAdditionTree += adderTree.last.map(pair => (pair(0) + pair(1))).toList
    adderoutConn += zipList(adderoutList.drop(adderPos).take(noAdderVals), adderAdditionTree.last).map(pair => (pair._1 := pair._2))
    if (stages(i + 3)) {
      adderMuxs += zipList(adderaltList.drop(adderPos).take(noAdderVals), adderAdditionTree.last).map(pair => (Mux(io.addToDict, pair._1, pair._2)))
      adderRegs += adderMuxs.last.map(pair => ({ val reg = Reg(init=Fixed(0.0, bitWidth, fracWidth)); reg := pair; reg }))
    }
    adderPos = adderPos + noAdderVals
    }
  // Output - last must have a register so ignore stages input
  if (stages.last) {
    io.result := adderRegs.last.last
  } else {
    io.result := adderAdditionTree.last.last
  }
}

class L2NormTests(c : L2Norm) extends Tester(c) {
  def toFixed(x : Double, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
  def toFixed(x : Float, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
  def toFixed(x : Int, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
  val r = scala.util.Random

  var noStages = 0
  for (i <- 0 until (c.stages.length))
    noStages = { if (c.stages(i)) { noStages + 1 } else { noStages } }

  val cycles = 10*noStages

  val expectResult = ArrayBuffer.fill(noStages)(0)
  val subRes = new ArrayBuffer[ArrayBuffer[Int]]()
  val sqrRes = new ArrayBuffer[ArrayBuffer[Int]]()
  if (c.stages(0)) {
    val sqrary = new ArrayBuffer[Int]()
    for (i <- 0 until c.features)
      sqrary += 0
    sqrRes += sqrary
  }
  // adderRes = stage[cycle[feature]]
  val adderRes = new ArrayBuffer[ArrayBuffer[ArrayBuffer[Int]]]()
  // add each stage
  for (i <- 0 until c.log2Features)
    adderRes += new ArrayBuffer[ArrayBuffer[Int]]()
  // compute cycles that are 0 in advance
  for (s <- 2 until c.stages.length) {
    if (c.stages(s - 1))
      adderRes(s - 2) += ArrayBuffer.fill(1 << (c.log2Features - 1 - (s - 2))){0}
  }

  for (cyc <- 0 until cycles) {
    val x1 = ArrayBuffer.fill(c.features)(r.nextInt(1 << (c.bitWidth/2)))
    val x2 = ArrayBuffer.fill(c.features)(r.nextInt(1 << (c.bitWidth/2)))
    val subalt = ArrayBuffer.fill(c.features)(r.nextInt(1 << (c.bitWidth/2)))
    val sqralt = ArrayBuffer.fill(c.features)(r.nextInt(1 << (c.bitWidth/2)))
    val adderalt  = ArrayBuffer.fill((1 << c.log2Features) - 1){r.nextInt(1 << (c.bitWidth/2))}
    val addToDict = (r.nextInt(2) == 1)
    poke(c.io.addToDict, Bool(addToDict).litValue())
    for (i <- 0 until adderalt.length)
      poke(c.io.adderalt(i), BigInt(adderalt(i)))
    for (i <- 0 until c.features) {
      poke(c.io.sqralt(i), BigInt(sqralt(i)))
      poke(c.io.subalt(i), BigInt(subalt(i)))
    }

    var sum = 0
    val subary = new ArrayBuffer[Int]()
    val sqrary = new ArrayBuffer[Int]()
    val sqrofalt = new ArrayBuffer[Int]()
    var sumalt = 0
    for (i <- 0 until c.features) {
      val tmp = (x1(i) - x2(i))
      subary += tmp
      val sqr = ((tmp*tmp) >> c.fracWidth)
      sqrary += sqr
      val sqr2 = (subalt(i)*subalt(i)) >> c.fracWidth
      sqrofalt += sqr2
      sum =  sum + sqr
      sumalt = sumalt + sqr2
      poke(c.io.x1(i), BigInt(x1(i)))
      poke(c.io.x2(i), BigInt(x2(i)))
    }
    subRes += subary
    if (addToDict && c.stages(0)) {
      sqrRes += sqrofalt
      expectResult += sumalt
    } else {
      sqrRes += sqrary
      expectResult += sum
    }
    if (addToDict) {
      // the last cycle has been added, correct the next ones for the sum
      // c.stages(end) forced true in implementation
      var tmpCyc = 0
      for (i <- 0 until (c.stages.length - 2)){
        if (c.stages(c.stages.length - 1 - i)) {
          var adderaltSum = 0
          for (j <- ((1 << i) - 1) until ((1 << (i+1)) - 1))
            adderaltSum += adderalt(adderalt.length - 1 - j)
          expectResult(cyc + 1 + tmpCyc) = adderaltSum
          tmpCyc += 1
        }
      }
      if (c.stages(1))
        expectResult(cyc + 1 + tmpCyc) = sqralt.sum
    }

    var adderStageCumSum = 0
    for (stage <- 0 until c.log2Features) {
      val adderFeatures = new ArrayBuffer[Int]()
      val adderStageSize = 1 << (c.log2Features - 1 - stage) 
      for (f <- 0 until adderStageSize) {
        if (stage == 0) {
          if (addToDict && c.stages(1)) {
            // change adderRes based on new alt inputs
            adderFeatures += (sqralt(2*f) + sqralt(2*f + 1))
          } else
            adderFeatures += (sqrRes(cyc)(2*f) + sqrRes(cyc)(2*f + 1))
        }
        else {
          if (addToDict && c.stages(stage + 1)) {
            // change adderRes based on new alt inputs
            adderFeatures += (adderalt(2*f + adderStageCumSum) + adderalt(2*f + adderStageCumSum + 1))
          } else
            adderFeatures += (adderRes(stage - 1)(cyc)(2*f) + adderRes(stage - 1)(cyc)(2*f + 1))
        }
      }
      if (stage != 0)
        adderStageCumSum += 2*adderStageSize
      // add the result to the next cycle
      adderRes(stage) += adderFeatures
    }

    for (i <- 0 until c.features) {
      expect(c.io.subout(i), BigInt(subRes(cyc)(i)))
      expect(c.io.sqrout(i), BigInt(sqrRes(cyc)(i)))
    }
    var adderPos = 0
    for (i <- 0 until c.log2Features) {
      for (j <- 0 until (1 << (c.log2Features - 1 - i))) {
        expect(c.io.adderout(adderPos), BigInt(adderRes(i)(cyc)(j)))
        adderPos = adderPos + 1
      }
    }

    expect(c.io.result, BigInt(expectResult(cyc)))
    step(1)
  }
}
