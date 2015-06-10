package OLK.NORMAStage

import Chisel._
import FixedPoint._

/** NORMAStage

  */
class NORMAStage(val bitWidth : Int, val intLength : Int) extends Module {
  val io = new Bundle {

  }
}

class NORMAStageTests(c: NORMAStage) extends Tester(c) {

}
