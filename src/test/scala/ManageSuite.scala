import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import Chisel._
import OLK.Manage._

class ManageSuite extends TestSuite {

  @Test def manageTests {

    class ManageTests(c : Manage) extends Tester(c) {
      val cycles = 5*c.stages
      val r = scala.util.Random

      val forceNAexpect = new ArrayBuffer[Boolean]()
      val yCexpect      = new ArrayBuffer[Boolean]()
      val yRegexpect    = new ArrayBuffer[Int]()
      for (i <- 1 until c.stages){
        forceNAexpect += true
        yCexpect      += true
        yRegexpect    += 0
      }

      var etanuOld = 0
      for (i <- 0 until cycles) {
        val forceNAin  = (r.nextInt(2) == 1)

        val yCin      = (r.nextInt(2) == 1)
        val yRegin    = r.nextInt(1 << (c.bitWidth/2))
        val forgetin  = r.nextInt(1 << (c.bitWidth/2))

        // For NORMA
        val eta    = r.nextInt(1 << (c.bitWidth/2))
        val nu     = r.nextInt(1 << (c.bitWidth/2))

        // For OLK
        val fracCin  = r.nextInt(1 << (c.bitWidth/2))
        val epsilon  = r.nextInt(1 << (c.bitWidth/2))

        forceNAexpect += forceNAin
        yCexpect += yCin
        yRegexpect += yRegin

        poke(c.io.forceNAin, Bool(forceNAin).litValue())
        poke(c.io.forgetin, BigInt(forgetin))

        if ( c.isNORMA ) {
          val normaIO = c.io.asInstanceOf[NORMAIOBundle]
          poke(normaIO.eta, BigInt(eta))
          poke(normaIO.nu,  BigInt(nu))
          if ( c.appType == 1 ) {
            val normacIO = normaIO.asInstanceOf[NORMAcIOBundle]
            poke(normacIO.yCin, Bool(yCin).litValue())
          }
          if ( c.appType == 3 ) {
            val normarIO = normaIO.asInstanceOf[NORMArIOBundle]
            poke(normarIO.yRegin, BigInt(yRegin))
          }
        } else {
          val olkIO = c.io.asInstanceOf[OLKIOBundle]
          poke(olkIO.fracCin, BigInt(fracCin))
          if ( c.appType == 1 ) {
            val olkcIO = olkIO.asInstanceOf[OLKcIOBundle]
            poke(olkcIO.yCin, Bool(yCin).litValue())
          }
          if ( c.appType == 3 ) {
            val olkrIO = olkIO.asInstanceOf[OLKrIOBundle]
            poke(olkrIO.epsilon, BigInt(epsilon))
            poke(olkrIO.yRegin,  BigInt(yRegin))
          }
        }

        step(1)

        if ( i >= c.stages - 1 ) {
          expect(c.io.forceNAout, Bool(forceNAexpect(i)).litValue())
          expect(c.io.forgetout, BigInt(forgetin))

          if ( c.isNORMA ) {
            val normaIO = c.io.asInstanceOf[NORMAIOBundle]
            expect(normaIO.etapos, BigInt(eta))
            expect(normaIO.etaneg, BigInt(-eta))
            expect(normaIO.etanu,  BigInt((eta*nu) >> c.fracWidth))
            expect(normaIO.etanu1, BigInt( etanuOld - eta ))

            if ( c.appType == 1 ) {
              val normacIO = normaIO.asInstanceOf[NORMAcIOBundle]
              expect(normacIO.yCout, Bool(yCexpect(i)).litValue())
            }
            if ( c.appType == 3 ) {
              val normarIO = normaIO.asInstanceOf[NORMArIOBundle]
              expect(normarIO.yRegout, BigInt(yRegexpect(i)))
            }
          } else {
            val olkIO = c.io.asInstanceOf[OLKIOBundle]
            expect(olkIO.fracCout, BigInt(fracCin))

            if ( c.appType == 1 ) {
              val olkcIO = olkIO.asInstanceOf[OLKcIOBundle]
              expect(olkcIO.yCout, Bool(yCexpect(i)).litValue())
            }
            if ( c.appType == 3 ) {
              val olkrIO = olkIO.asInstanceOf[OLKrIOBundle]
              expect(olkrIO.yepos, BigInt(yRegexpect(i) - epsilon))
              expect(olkrIO.yeneg, BigInt(- (yRegexpect(i) + epsilon)))
            }
          }
        }
        etanuOld = (eta*nu) >> c.fracWidth
      }
    }

    val myRand = new Random
    val fracWidth = myRand.nextInt(24) + 1
    val bitWidth = myRand.nextInt(24) + fracWidth + 4
    val stages = myRand.nextInt(50)
    val isNORMA = true
    val appType = myRand.nextInt(3) + 1
    println("val fracWidth = " + fracWidth)
    println("val bitWidth = " + bitWidth)
    println("val stages = " + stages)
    println("val isNORMA = " + isNORMA)
    println("val appType = " + appType)
    chiselMainTest(Array("--genHarness", "--compile", "--test", "--backend", "c"), () => {
      Module( new Manage( bitWidth, fracWidth, stages, isNORMA, appType) )
    } ) { c => new ManageTests( c ) }
  }

}
