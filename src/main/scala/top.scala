package OLK

import OLK.Dict._
import OLK.Sum._
import OLK.NORMAStage._
import OLK.Manage._
import Chisel._
import scala.collection.mutable.ArrayBuffer

/** Top Level Function
  */
object Top extends stageCalc {

    def main(args: Array[String]): Unit = {

      val r = scala.util.Random
      val dictSize = 16
      val log2Features = 3
      val log2Table = 4
      var sStages = ArrayBuffer.fill(30){ r.nextInt(2) == 1 }
      var calcStage = sStages.length - 1
      while(sStages.length != calcStage) {
          sStages = sStages.dropRight(1)
          sStages(sStages.length - 1) = false
          calcStage = calculatedStages(dictSize, sStages.count(_ == true), sStages)
      }

      //      chiselMainTest(args, () => Module(new SumR(18, 12, dictSize, sStages))) {
      //        c => new SumRTests(c) }

      sStages.append(true) // For final sum

      //      chiselMainTest(args, () => Module(new SumStage(18, 12, sStages, dictSize, true))) {
      //        c => new SumStageTests(c) }

      sStages.append(true) // For Update
      // generate stages for kernel evaluation
      val stages = ArrayBuffer.fill(log2Features + 7){ r.nextInt(2) == 1 }
      stages.appendAll(sStages)
      val isNorma = true
      val appType = 3

      chiselMainTest(args, () => Module(new NORMA(18, 12, stages, log2Table, dictSize, 1 << log2Features, appType))) {
        c => new NORMATests(c) }

      //      chiselMainTest(args, () => Module(new Manage(18, 12, stages, isNORMA, appType))) {
      //        c => new ManageTests(c) }
      //      chiselMainTest(args, () => Module(new Dict(18, 12, 32, 8, 10, isNORMA))) {
      //        c => new DictTests(c) }

  }

}
