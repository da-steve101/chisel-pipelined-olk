package OLK

import OLK.Dict._
import OLK.Sum._
import Chisel._

/** Top Level Function
  */
object Top {

    def main(args: Array[String]): Unit = {

    chiselMainTest(args, () => Module(new Dict(18, 4, 20, 5, 10, false))) {
      c => new DictTests(c) }
  }

}
