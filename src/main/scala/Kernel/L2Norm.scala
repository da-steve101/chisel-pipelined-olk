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
    adderAdditionTree += adderTree.last.map(pair => { if (pair.length > 1 ) { pair(0) + pair(1) } else pair(0) }).toList
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
