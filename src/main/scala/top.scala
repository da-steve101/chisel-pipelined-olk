package OLK

import OLK.Dict._
import OLK.Sum._
import Chisel._
import scala.collection.mutable.ArrayBuffer

/** Top Level Function
  */
object Top {

    def main(args: Array[String]): Unit = {

      val r = scala.util.Random
      val log2dictSize = 3
      val activeStages = 1
      val stages = ArrayBuffer.fill(log2dictSize + 1 + activeStages)( false )//r.nextInt(2) == 1 )
      val isNorma = true
      stages(0) = true
      stages(1) = true
      stages(2) = true
      stages(3) = true
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
