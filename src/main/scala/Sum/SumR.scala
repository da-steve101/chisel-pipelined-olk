/** SumR.scala -> This file computes the sum of all terms in the dictionary that should be included in the update
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

package OLK.Sum

import Chisel._
import OLK._
import scala.collection.mutable.ArrayBuffer

/** SumR
  This function sums the results of the kernel function
  that are currently in the dictionary
  parameter d : dictionary size
  parameter s : stages

  input vi     = SFix[d]  // Outputs from kernel function
  input alphai = SFix[d] // Current weights of dictionary
  input addToDict = 1 Bit
  
  output sumR  = SFix
  output wD    = SFix
  output WD1   = SFix

  Registers:
  Stage_0, Stage_1, ... Stage_(s-1)[1]
  Spare_0[s+1], ... Spare_(s-1)[2]

  Logic:
  wi = vi.*alphai

  Stage_0[0:k] = sum(w1 : w(d-s-2)) + (addTODict) ? w(d-s-1) : 0
  Stage_1[0:l] = sum(Stage_0) + (addToDict) ? Spare_0[0] : 0
  Stage_2[0:m] = sum(Stage_1) + (addToDict) ? Spare_1[0] : 0
  ... etc where m < l < k

  Spare_0 = (addToDict) ? [w(d-s-2):w(d)] : [w(d-s-1):w(d-1)]
  Spare_1 = (addToDict) ? Spare_0[1:end]  : Spare_0[0:end-1]
  Spare_2 = (addToDict) ? Spare_1[1:end]  : Spare_1[0:end-1]
  ... etc

  sumR = Stage_(s-1)
  wD1  = Spare_(s-1)[0]
  wD   = Spare_(s-1)[1]
  */

trait stageCalc { 
  def calculatedStages(dictionarySize : Int, stages : ArrayBuffer[Boolean]) : Int = {
    var layerCount = dictionarySize - stages.count(_ == true) - 2
    var sum = 1
    var i = 0
    while ( layerCount > 1 ) {
      layerCount = (layerCount >> 1) + (layerCount & 1)
      if ( i < stages.length ) {
        if ( stages(i) )
          layerCount += 1
      }
      sum += 1
      i += 1
    }
    sum
  } // Only adds a stage if moved up to higher power of 2
}

class SumR(val bitWidth : Int, val fracWidth : Int, val dictionarySize : Int,
  val stages : ArrayBuffer[Boolean]) extends Module with stageCalc {
    def log2Dict : Int = { log2Up(dictionarySize) }
    def activeStages : Int = { stages.count(_ == true) }
    //Predef.assert(stages.length == calculatedStages(dictionarySize, activeStages, stages),
    //  "Length of stages must be = " + (calculatedStages(dictionarySize, activeStages, stages)) +
    //    " but got stages of length = " + stages.length)

    def group[A](list : List[A], size : Int) : List[List[A]] = list.foldLeft( (List[List[A]](), 0) ) { (r, c) => 
        r match {
            case (head :: tail, num) =>
                if (num < size) ( (c :: head) :: tail, num + 1)
                else            ( List(c) :: head :: tail, 1)
            case (Nil, num) => (List(List(c)),1)
        }
    }._1.foldLeft(List[List[A]]())( (r,c) => c.reverse :: r)

    def buildLevel[A](list : List[A], op : (A, A) => A, op2 : A => A) : List[A] = group(list, 2).map(l =>  {
        if (l.length == 1) op2(l(0))
        else l.reduce[A](op(_,_))})

    def pipeline(hasStage : Boolean)(wire : Fixed) = if (hasStage) Reg(init=Fixed(0, wire.getWidth(), wire.fractionalWidth), next=wire) else wire

    def pipeAdd(hasStage : Boolean)(a : Fixed, b : Fixed = ZERO) : Fixed = pipeline(hasStage)(a + b)

    val lnOf2 = scala.math.log(2)
    def log2(x : Int) : Int = (scala.math.log(x.toDouble) / lnOf2).toInt
    def log2(x : Double) : Int = (scala.math.log(x) / lnOf2).toInt

    def spareLevel(list : List[Fixed]) : Vec[Fixed] = Mux(io.addToDict, Vec(list.dropRight(1)), Vec(list.drop(1)))

    val ZERO = Fixed(0, bitWidth, fracWidth)

    val io = new Bundle {
        val vi  = Vec.fill(dictionarySize){Fixed(INPUT, bitWidth, fracWidth)}
        val alphai = Vec.fill(dictionarySize){Fixed(INPUT, bitWidth, fracWidth)}
        val addToDict = Bool(INPUT)

        val sumR = Fixed(OUTPUT, bitWidth, fracWidth)
        val wD = Fixed(OUTPUT, bitWidth, fracWidth)
        val wD1 = Fixed(OUTPUT, bitWidth, fracWidth)
    }

    /**
     * Initial Implementation
     */
    // Setup inputs as list to make it easier to work with
    val viList = io.vi.toList
    val alphaiList = io.alphai.toList

    // wi = vi.*alphai
    val wiList = (viList, alphaiList).zipped.map(_*%_)
    val wiListPipe = wiList.map( x => { pipeline(stages(0))(x) } )

    val n = stages.length

    var sumrInput = wiListPipe.dropRight(activeStages + 2)
    var spareInput = wiListPipe.drop(dictionarySize - activeStages - 2)

    if ( stages(0) ) {
      sumrInput = sumrInput :+ pipeline(stages(0))(Mux(io.addToDict, ZERO, wiList(dictionarySize - activeStages - 2)))
      spareInput = spareLevel( wiList.drop(dictionarySize - activeStages - 2)).toList.map( x => { pipeline(stages(0))(x) } )
    }

    val sumrTree = scala.collection.mutable.MutableList(sumrInput)
    val spareTree = scala.collection.mutable.MutableList(spareInput)

    // Build Adder Tree
    for (i <- 1 until n) {
        if (stages(i)) {
            val adderLevel = buildLevel(sumrTree.last.dropRight(1).toList, pipeAdd(stages(i)), pipeline(stages(i)))
            val shiftDict = pipeline(stages(i))(Mux(io.addToDict, sumrTree.last.last, sumrTree.last.last + spareTree.last.head))
            sumrTree += adderLevel :+ shiftDict
            val spare = spareLevel(spareTree.last.toList).toList
            spareTree += spare.map(x => { pipeline(stages(i))(x) } )
        } else {
            val adderLevel = buildLevel(sumrTree.last.toList, pipeAdd(stages(i)), pipeline(stages(i)))
            sumrTree += adderLevel
        }
    }

    println("sumrTree is:")
    for ( i <- 0 until sumrTree.length )
        println(sumrTree(i).length)
    println("spareTree is:")
    for ( i <- 0 until spareTree.length )
        println(spareTree(i).length)

    Predef.assert(sumrTree.last.length == 1, "Last stage in sum tree must have length of one")
    Predef.assert(spareTree.last.length == 2, "Last stage in spare tree must length of two")

    // Output
    io.sumR := sumrTree.last.head
    io.wD   := spareTree.last.last
    io.wD1  := spareTree.last.head
}
