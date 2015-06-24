package OLK

import OLK.Dict._
import OLK.Sum._
import Chisel._
import scala.collection.mutable.ArrayBuffer

/** Top Level Function
  */
object Top extends stageCalc {

    def main(args: Array[String]): Unit = {

      val r = scala.util.Random
      val log2dictSize = 4
      var stages = ArrayBuffer.fill(log2dictSize + 20)( r.nextInt(2) == 1 ) ++ ArrayBuffer.fill(100)(false)
      var calcStage = stages.length - 1
      var prevStage = stages.length
      while(prevStage != calcStage) {
          val activeStages = stages.count(_ == true)
          prevStage = calcStage
          calcStage = calculatedStages(log2dictSize, 1 << log2dictSize, activeStages, stages)
          stages = stages.slice(0, calcStage)
      }
      val isNorma = true
      //stages(0) = true
      //stages(1) = false
      //stages(2) = true
      //stages(3) = true
      /*
      stages(log2dictSize) = true
      stages(log2dictSize + 1) = true
      chiselMainTest(args, () => Module(new SumStage(18, 12, stages, 1 << log2dictSize, isNorma))) {
        c => new SumStageTests(c) }
       */
      chiselMainTest(args, () => Module(new SumR(18, 12, 1 << log2dictSize, stages))) {
        c => new SumRTests(c) }
  }

}
