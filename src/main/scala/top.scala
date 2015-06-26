package OLK

import OLK.Dict._
import OLK.Sum._
import OLK.NORMAStage._
import OLK.Manage._
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

    def main(args: Array[String]): Unit = {

      val paramFilename  = args(0)
      val inputFilename  = args(1)
      val outputFilename = args(2)
      // Add checks in case the args have screwed up
      checkFile(paramFilename, "csv")
      checkFile(inputFilename, "csv")
      Predef.assert(outputFilename.substring(outputFilename.lastIndexOf(".") + 1) == "csv",
        "File " + outputFilename + " must have extension of .csv")
      // Load csv files
      // paramFile: bitWidth, fracWidth, log2Table, dictSize, features, appType
      //            stages(0) == T/F, stages(1) == T/F ...
      //            (gamma) (forget) (eta) (nu)
      // inputfile depends on appType
      // one example on each line for (classification/novelty/regression)
      // input should be in floating point
      // (reset) (forceNA) (yC/0/yReg) (feature(0)) (feature(1)) ...
      // output file is
      // (addToDict) (y) (ft)
      // NB: first three lines of the output file are the parameters repeated
      //     third line of output file is the name of the input file
      val reader = CSVReader.open(new java.io.File(paramFilename))
      val lines = reader.all
      reader.close
      Predef.assert(lines.length >= 3, "There must be at least three lines in the parameters file")
      Predef.assert(lines(0).length == 6,
        "Must have 6 parameters in first line\nbitWidth, fracWidth, log2Table, dictSize, features, appType")
      val bitWidth  = tryToInt(lines(0)(0), "bitWidth of "  + lines(0)(0) + " is not an integer")
      val fracWidth = tryToInt(lines(0)(1), "fracWidth of " + lines(0)(1) + " is not an integer")
      val log2Table = tryToInt(lines(0)(2), "log2Table of " + lines(0)(2) + " is not an integer")
      val dictSize  = tryToInt(lines(0)(3), "dictSize of "  + lines(0)(3) + " is not an integer")
      val features  = tryToInt(lines(0)(4), "features of "  + lines(0)(4) + " is not an integer")
      val appType   = tryToInt(lines(0)(5), "appType of "   + lines(0)(5) + " is not an integer")
      Predef.assert(appType == 1 || appType == 2 || appType == 3,
        "appType must be classification = 1, novelty detection = 2 or regression = 3")

      val stages = lines(1).map(x => {
        tryToBool(x, "Could not convert " + x + " to a boolean")
      }).to[ArrayBuffer]

      /*
      val r = scala.util.Random
      var sStages = ArrayBuffer.fill(30){ r.nextInt(2) == 1 }
      sStages = (0 until 30).to[ArrayBuffer].map(x => { (x % 2) == 1 })
      var calcStage = sStages.length - 1
      while(sStages.length != calcStage) {
          sStages = sStages.dropRight(1)
          sStages(sStages.length - 1) = true
          calcStage = calculatedStages(dictSize, sStages.count(_ == true), sStages)
          if (calcStage > sStages.length)
            sStages.appendAll(ArrayBuffer.fill(calcStage - sStages.length){ r.nextInt(2) == 1 })
          println("calcStage: " + calcStage)
      }
      println(sStages)
      chiselMainTest(args, () => Module(new SumR(18, 12, dictSize, sStages))) {
        c => new SumRTests(c) }

      sStages.append(true) // For final sum

      //      chiselMainTest(args, () => Module(new SumStage(18, 12, sStages, dictSize, true))) {
      //        c => new SumStageTests(c) }

      sStages.append(true) // For Update
      // generate stages for kernel evaluation
      val stages = ArrayBuffer.fill(log2Features + 7){ r.nextInt(2) == 1 }
      stages.appendAll(sStages)
      val isNorma = true
      val appType = 3
       */

      chiselMainTest(args.drop(3), () => Module(
        new NORMA(bitWidth, fracWidth, stages, log2Table, dictSize, features, appType, paramFilename, inputFilename, outputFilename))
      ) { c => new NORMATests(c) }

      //      chiselMainTest(args, () => Module(new Manage(18, 12, stages, isNORMA, appType))) {
      //        c => new ManageTests(c) }
      //      chiselMainTest(args, () => Module(new Dict(18, 12, 32, 8, 10, isNORMA))) {
      //        c => new DictTests(c) }

  }

}
