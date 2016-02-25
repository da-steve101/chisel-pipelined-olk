/** top.scala -> Top level module
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

package OLK

import Chisel._
import OLK.Sum._
import scala.collection.mutable.ArrayBuffer
import com.github.tototoshi.csv._
import scala.util.control.Breaks._

class NORMATests(c : NORMA) extends Tester(c) {
  // Generate Table for linear interpolation
  /*  val gradients   = new ArrayBuffer[Int]()
   val offsets     = new ArrayBuffer[Int]()
   // Fixed point increment
   val increment   = 1.0 / (1 << c.log2Table)
   val tableEnd    = 1.0
   var xtmp = 0.0
   // NOTE: xtmp is positive, therefore gradient is negitive
   while (xtmp < tableEnd) {
   // m = (y1 - y2)/(x1 - x2)
   val m = -(scala.math.pow(2, - xtmp) - scala.math.pow(2,- xtmp - increment))/increment
   // convert to Fixed
   gradients += (m  * (1 << c.fracWidth)).toInt
   // b = y1 - m*x1
   val b = scala.math.pow(2, - xtmp) - m*xtmp
   // convert to Fixed
   offsets += (b * (1 << c.fracWidth)).toInt
   xtmp += increment
   }
   */
  def checkFile(filename : String, ext : String) : Unit = {
    Predef.assert(filename.substring(filename.lastIndexOf(".") + 1) == ext, "File " + filename + " must have extension of ." + ext)
    //Predef.assert(java.nio.file.Files.exists(java.nio.file.Paths.get(filename)), "File " + filename + " does not exist")
  }

  def tryToBool(myBool : String, message : String) : Boolean = {
    try {
      myBool.trim.toBoolean
    } catch {
      case x:Exception => throw new Exception(message)
    }
  }

  def tryToFixed(myDouble : String, message : String) : BigInt = {
    try {
      (myDouble.trim.toDouble * BigDecimal( BigInt(1) << c.fracWidth )).toBigInt
    } catch {
      case x:Exception => throw new Exception(message)
    }
  }

  def fromPeek(myNum : BigInt) : BigInt = {
    if (myNum >= (BigInt(1) << (c.bitWidth - 1)))
      myNum - (BigInt(1) << c.bitWidth)
    else
      myNum
  }

  //  val r = scala.util.Random

  // read parameters
  val reader = CSVReader.open(new java.io.File(c.paramFilename))
  val lines = reader.all
  reader.close
  Predef.assert(lines.length >= 3, "Insufficient lines in parameter file " + c.paramFilename)
  Predef.assert(lines(2).length <= 4, "Insufficient number of parameters in line 3 of parameter file " + c.paramFilename)
  val gamma  = tryToFixed(lines(2)(0), "Could not cast gamma = "  + lines(2)(0) + " to a double or int")
  val forget = tryToFixed(lines(2)(1), "Could not cast forget = " + lines(2)(1) + " to a double or int")
  val eta    = tryToFixed(lines(2)(2), "Could not cast eta = "    + lines(2)(2) + " to a double or int")
  val nu     = tryToFixed(lines(2)(3), "Could not cast nu = "     + lines(2)(3) + " to a double or int")
  /*
   def addExample(alpha : Int, example : ArrayBuffer[Int], forget : Int,
   weights : ArrayBuffer[Int], dictionary : ArrayBuffer[ArrayBuffer[Int]]) : Unit = {
   var newWeight = weights(0)
   var newExample = dictionary(0)
   weights(0) = alpha
   dictionary(0) = example
   for (i <- 1 until weights.length) {
   val oldWeight = weights(i)
   val oldExample = dictionary(i)
   weights(i) = (newWeight*forget) >> c.fracWidth
   dictionary(i) = newExample
   newWeight = oldWeight
   newExample = oldExample
   }
   }

   def forgetDict(forget : Int, weights : ArrayBuffer[Int]) : Unit = {
   for (i <- 0 until weights.length)
   weights(i) = (weights(i)*forget) >> c.fracWidth
   }

   def gaussian(x : Int) : Int = {
   val xVal = (gamma*x) >> c.fracWidth
   val xFrac = xVal & ((1 << c.fracWidth ) - 1)
   val xInt = xVal >> c.fracWidth
   val xTabl = xFrac >> (c.fracWidth - c.log2Table)
   val yFrac = ((gradients(xTabl)*xFrac) >> c.fracWidth) + offsets(xTabl)
   yFrac >> xInt
   }

   def computeK(example : ArrayBuffer[Int], dictionary : ArrayBuffer[ArrayBuffer[Int]]) : ArrayBuffer[Int] = {
   val K = new ArrayBuffer[Int]()
   for (i <- 0 until dictionary.length) {
   val l2norm = (example zip dictionary(i)).map(pair => { pair._1 - pair._2 }).map(x => { (x*x) >> c.fracWidth }).sum
   K.append(gaussian(l2norm))
   }
   K
   }

   def computeFT(example : ArrayBuffer[Int], weights : ArrayBuffer[Int], dictionary : ArrayBuffer[ArrayBuffer[Int]]) : Int = {
   val K = computeK(example, dictionary)
   (weights zip K).map(pair => { (pair._1 * pair._2) >> c.fracWidth }).sum
   }

   val dictionary = ArrayBuffer.fill(c.dictionarySize){ArrayBuffer.fill(c.features){0}}
   val weights = ArrayBuffer.fill(c.dictionarySize){0}
   */
  val cycles = 5*c.pCycles
  poke(c.io.gamma, gamma)
  poke(c.io.forget, forget)
  poke(c.io.nu, nu)
  poke(c.io.eta, eta)
  /*
   val expectedFt = ArrayBuffer.fill(c.pCycles - 1){0}
   val expectedAlpha = ArrayBuffer.fill(c.pCycles - 1){0}
   val expectedAdd = ArrayBuffer.fill(c.pCycles - 1){false}
   val expectedyC = ArrayBuffer.fill(c.pCycles - 2){false}
   val expectedEx = ArrayBuffer.fill(c.pCycles - 1){ArrayBuffer.fill(c.features){0}}
   var rho = 0
   var b = 0
   var alpha = 0 */
  val expectedyReg = ArrayBuffer.fill(c.pCycles - 1){BigInt(0)}
  val ONE = (BigInt(1) << c.fracWidth).toDouble
  var cyc = 0

  var numFeatures = -1

  // Open input and output files for reading and writing respectively
  checkFile(c.inputFilename, "csv")
  Predef.assert(c.outputFilename.substring(c.outputFilename.lastIndexOf(".") + 1) == "csv",
    "File " + c.outputFilename + " must have extension of .csv")
  val inputReader   = CSVReader.open(new java.io.File(c.inputFilename))
  val inputIterator = inputReader.iterator
  val outputWriter  = CSVWriter.open(new java.io.File(c.outputFilename))
  outputWriter.writeAll(lines.take(3)) // Write parameters to output so a record is attached
  outputWriter.writeRow(List(c.inputFilename))

  while ( inputIterator.hasNext ) {
    val inputExLine = inputIterator.next
    if (numFeatures == -1)
      numFeatures = inputExLine.length - 3
    if (numFeatures < 1 || inputExLine.length != numFeatures + 3 || numFeatures != c.features) {
      println("Line " + cyc + " has incorrect number of values, aborting")
      break
    }
    // val reset   = tryToBool(inputExLine(0), "Could not read reset as Boolean on line " + cyc)
    val forceNA = tryToBool(inputExLine(1), "Could not read forceNA as Boolean on line " + cyc)
    val yReg    = tryToFixed(inputExLine(2), "Could not convert y to fixed on line " + cyc)
    val yC      = (yReg == ONE) // use 1 and 0 or 1 and -1 for yC
    val example = inputExLine.drop(3).map(x => {
      tryToFixed(x, "Could not convert a feature to fixed on line " + cyc)
    }).to[ArrayBuffer]

    expectedyReg += yReg
    /*
     //val example = ArrayBuffer.fill(c.features){r.nextInt(1 << (c.bitWidth/2))}
     //val yC   = (r.nextInt(2) == 1)
     //val yReg = r.nextInt(1 << c.fracWidth)

     val ft = computeFT(example, weights, dictionary)
     expectedFt += { if (c.appType == 2) ft - rho else ft }
     expectedyC += yC
     expectedEx += example

     println("weights: " + weights)
     var addToDict = false
     // Calculate Stage update
     if (c.appType == 1) {
     // classification
     val g = { if (yC) { ft + b } else { - ft - b } }
     addToDict = (rho - g) > 0
     alpha = { if (yC) { eta } else { -eta } }
     b = { if (addToDict) {
     if (yC) { b + eta } else { b - eta }
     } else b }
     rho = rho + ((eta*nu) >> c.fracWidth )
     if (addToDict)
     rho = rho - eta
     } else if (c.appType == 2) {
     // novelty
     addToDict = (rho - ft) > 0
     alpha = eta
     rho = rho + ((eta*nu) >> c.fracWidth )
     if (addToDict)
     rho = rho - eta
     } else {
     // regression
     val d = yReg - ft
     val sign = { if (d > 0) 1 else -1 }
     addToDict = (sign*d - rho) > 0
     alpha = sign*eta
     rho = rho - ((eta*nu) >> c.fracWidth )
     if (addToDict)
     rho = rho + eta
     }
     if (addToDict)
     addExample(alpha, example, forget, weights, dictionary)
     else
     forgetDict(forget, weights)

     expectedAlpha += alpha
     expectedAdd   += addToDict
     println("yC(" + cyc + "): " + expectedyC(cyc))
     println("example(" + cyc + "): " + expectedEx(cyc))
     println("ft(" + cyc + "): " + expectedFt(cyc))
     println("alpha(" + cyc + "): " + expectedAlpha(cyc))
     println("addToDict(" + cyc + "): " + expectedAdd(cyc))
     */

    // Send values
    poke(c.io.forceNA, Bool(forceNA).litValue())
      (c.io.example zip example).map(pair => { poke(pair._1, pair._2) } )
    if (c.appType == 1) {
      val NORMAcIO = c.io.asInstanceOf[IOBundle_C]
      poke(NORMAcIO.yC, Bool(yC).litValue())
    }
    if (c.appType == 3) {
      val NORMArIO = c.io.asInstanceOf[IOBundle_R]
      poke(NORMArIO.yReg, yReg)
    }

    step(1)
    // Expect output
    // expect(c.io.ft, BigInt(expectedFt(cyc)))
    // put Output into file
    if ( cyc >= (c.pCycles - 1))
      outputWriter.writeRow(List(peek(c.io.addToDict) == 1, (expectedyReg(cyc).toDouble / ONE), (fromPeek(peek(c.io.ft)).toDouble / ONE)))

    cyc += 1
  }
  // Push the last few through the pipeline
  for ( i <- 0 until (c.pCycles - 1) ) {
    step(1)
    outputWriter.writeRow(List(peek(c.io.addToDict) == 1, (expectedyReg(cyc + i).toDouble / ONE), (fromPeek(peek(c.io.ft)).toDouble / ONE)))
  }
  inputReader.close()
  outputWriter.close()
}

/** Top Level Function
  */
object Top extends stageCalc {
    def checkFile(filename : String, ext : String) : Unit = {
      Predef.assert(filename.substring(filename.lastIndexOf(".") + 1) == ext, "File " + filename + " must have extension of ." + ext)
      //Predef.assert(java.nio.file.Files.exists(java.nio.file.Paths.get(filename)), "File " + filename + " does not exist")
    }

    def tryToInt(number : String, message : String) : Int = {
      try {
        number.trim.toInt
      } catch {
        case x:Exception => throw new Exception(message)
      }
    }

    def tryToBool(myBool : String, message : String) : Boolean = {
      try {
        myBool.trim.toBoolean
      } catch {
        case x:Exception => throw new Exception(message)
      }
    }

    def generateSumRStages(dictSize : Int) : ArrayBuffer[Boolean] = {
      val r = scala.util.Random
      var sStages = ArrayBuffer.fill(30){ r.nextInt(2) == 1 }
      sStages = (0 until 30).to[ArrayBuffer].map(x => { (x % 2) == 1 })
      var calcStage = sStages.length - 1
      while(sStages.length != calcStage) {
        sStages = sStages.dropRight(1)
        calcStage = calculatedStages(dictSize, sStages.count(_ == true), sStages)
        if (calcStage > sStages.length)
          sStages.appendAll(ArrayBuffer.fill(calcStage - sStages.length){ r.nextInt(2) == 1 })
      }
      sStages
    }

    def main(args: Array[String]): Unit = {

      val chiselArgs = args.drop(1)
      val runArgs = args.drop(4)
      val r = scala.util.Random
      var bitWidth = 18
      var fracWidth = 12
      var log2Table = 4
      var dictSize = 16
      var features = 8
      var appType = r.nextInt(3) + 1
      var isNORMA = true
      args(0) match {

        case "NORMARUN" => {
          val paramFilename  = args(1)
          val inputFilename  = args(2)
          val outputFilename = args(3)
          // Add checks in case the args have screwed up
          checkFile(paramFilename, "csv")
          // Load csv files
          // paramFile: bitWidth, fracWidth, log2Table, dictSize, features, appType
          //            stages(0) == T/F, stages(1) == T/F ...
          //            (gamma) (forget) (eta) (nu)
          // inputfile: one example on each line, input should be in floating point
          // (reset) (forceNA) (yC/0/yReg) (feature(0)) (feature(1)) ...
          // output file is:
          // (addToDict) (y) (ft)
          // NB: first three lines of the output file are the parameters repeated
          //     fourth line of output file is the name of the input file
          val reader = CSVReader.open(new java.io.File(paramFilename))
          val lines = reader.all
          reader.close
          Predef.assert(lines.length >= 3, "There must be at least three lines in the parameters file")
          Predef.assert(lines(0).length == 6,
            "Must have 6 parameters in first line\nbitWidth, fracWidth, log2Table, dictSize, features, appType")
          bitWidth  = tryToInt(lines(0)(0), "bitWidth of "  + lines(0)(0) + " is not an integer")
          fracWidth = tryToInt(lines(0)(1), "fracWidth of " + lines(0)(1) + " is not an integer")
          log2Table = tryToInt(lines(0)(2), "log2Table of " + lines(0)(2) + " is not an integer")
          dictSize  = tryToInt(lines(0)(3), "dictSize of "  + lines(0)(3) + " is not an integer")
          features  = tryToInt(lines(0)(4), "features of "  + lines(0)(4) + " is not an integer")
          appType   = tryToInt(lines(0)(5), "appType of "   + lines(0)(5) + " is not an integer")
          Predef.assert(appType == 1 || appType == 2 || appType == 3,
            "appType must be classification = 1, novelty detection = 2 or regression = 3")

          val stages = lines(1).map(x => {
            tryToBool(x, "Could not convert " + x + " to a boolean")
          }).to[ArrayBuffer]

          chiselMainTest(runArgs, () => Module(
            new NORMA(bitWidth, fracWidth, stages, log2Table, dictSize, features, appType, paramFilename, inputFilename, outputFilename))
          ) { c => new NORMATests(c) }

        }
      }
    }
}

