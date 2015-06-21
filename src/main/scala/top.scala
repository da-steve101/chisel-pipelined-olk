package OLK

import OLK.Dict._
import Chisel._

/** Top Level Function
  */
object Top {

  def main(args: Array[String]): Unit = {
//    chiselMainTest(args, () => Module(new AlphaFunctOLKn(18, 4))) {
//      c => new AlphaFunctOLKnTests(c, 18, 4) }

//    chiselMainTest(args, () => Module(new JustTestingCrap(18))) {
//      c => new JustTestingCrapTests(c) }

    chiselMainTest(args, () => Module(new Dict(18, 4, 20, 5, 10, false))) {
      c => new DictTests(c) }
  }

}
