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

import OLK.Dict._
import OLK.Sum._
import OLK.NORMAStage._
import OLK.Manage._
import OLK.Kernel._
import Chisel._
import scala.collection.mutable.ArrayBuffer
import com.github.tototoshi.csv._

/** Top Level Function
  */
object Top extends stageCalc {
    def checkFile(filename : String, ext : String) : Unit = {
      Predef.assert(filename.substring(filename.lastIndexOf(".") + 1) == ext, "File " + filename + " must have extension of ." + ext)
      Predef.assert(java.nio.file.Files.exists(java.nio.file.Paths.get(filename)), "File " + filename + " does not exist")
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
          checkFile(inputFilename, "csv")
          Predef.assert(outputFilename.substring(outputFilename.lastIndexOf(".") + 1) == "csv",
            "File " + outputFilename + " must have extension of .csv")
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

        case "SumR" => {
          val stages = generateSumRStages(dictSize)
          println("Stages: " + stages)
          chiselMainTest(chiselArgs, () => Module(new SumR(bitWidth, fracWidth, dictSize, stages))) {
            c => new SumRTests(c) }
        }

        case "SumStage" => {
          val stages = generateSumRStages(dictSize)
          stages.append(true)
          println("Stages: " + stages)
          chiselMainTest(chiselArgs, () => Module(new SumStage(bitWidth, fracWidth, stages, dictSize, isNORMA))) {
            c => new SumStageTests(c) }
        }

        case "Gaussian" => {
          val stages = ArrayBuffer.fill(log2Up(features) + 7){ r.nextInt(2) == 1 }
          println("Stages: " + stages)
          chiselMainTest(chiselArgs, () => Module(
            new Gaussian(bitWidth, fracWidth, dictSize, features, stages.length + 4, stages, 1 << log2Table))
          ) { c => new GaussianTests(c) }
        }

        case "Pow2" => {
          val stages = ArrayBuffer.fill(5){ r.nextInt(2) == 1 }
          println("Stages: " + stages)
          chiselMainTest(chiselArgs, () => Module(new Pow2(bitWidth, fracWidth, stages, 1 << log2Table))) {
            c => new Pow2Tests(c) }
        }

        case "Manage" => {
          val stages = r.nextInt(15) + 3
          chiselMainTest(chiselArgs, () => Module(new Manage(bitWidth, fracWidth, stages, isNORMA, appType))) {
            c => new ManageTests(c) }
        }

        case "Dict" => {
          val stages = r.nextInt(15) + 3
          chiselMainTest(chiselArgs, () => Module(new Dict(bitWidth, fracWidth, dictSize, features, stages, isNORMA))) {
            c => new DictTests(c) }
        }
      }
  }

}
