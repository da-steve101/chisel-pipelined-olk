package OLK.NORMAStage

import Chisel._
import FixedPoint._

/** NORMAn

  */
class NORMAn(val bitWidth : Int, val fracWidth : Int) extends Module {
  val io = new Bundle {
  }
}

class NORMAnTests(c: NORMAn) extends Tester(c) {

}
