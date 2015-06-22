package OLK

import OLK.Dict._
import OLK.Sum._
import Chisel._

/** Top Level Function
  */
object Top {

    def main(args: Array[String]): Unit = {
        val bitWidth = 16
        val fracWidth = 8
        val dictionarySize = 8
        val sumRStages = 4
        //val theArgs = args.slice(1, args.length)
        //args(0) match {
            //case "Dict" =>
                //chiselMainTest(args, () => Module(new Dict(18, 4, 20, 5, 10))) {
                    //c => new DictTests(c) }
            //case "SumR" =>
                chiselMainTest(args, () => Module(new SumR(bitWidth, fracWidth, dictionarySize, sumRStages))) {
                    c => new SumRTests(c) }
        //}
    //    chiselMainTest(args, () => Module(new AlphaFunctOLKn(18, 4))) {
    //      c => new AlphaFunctOLKnTests(c, 18, 4) }

    //    chiselMainTest(args, () => Module(new JustTestingCrap(18))) {
    //      c => new JustTestingCrapTests(c) }

    chiselMainTest(args, () => Module(new Dict(18, 4, 20, 5, 10, false))) {
      c => new DictTests(c) }
  }

}
