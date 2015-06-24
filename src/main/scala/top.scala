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
      val dictSize = 1024
      var stages = ArrayBuffer.fill(30)( r.nextInt(2) == 1 )
      var calcStage = stages.length - 1
      while(stages.length != calcStage) {
          stages = stages.dropRight(1)
          stages(stages.length - 1) = false
          stages(stages.length - 2) = false
          calcStage = calculatedStages(dictSize, stages.count(_ == true), stages)
      }
      val isNorma = true

      chiselMainTest(args, () => Module(new SumR(18, 12, dictSize, stages))) {
        c => new SumRTests(c) }
  }

}
