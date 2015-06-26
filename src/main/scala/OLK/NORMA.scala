package OLK

import Chisel._
import OLK.Dict._
import OLK.Kernel._
import OLK.Manage._
import OLK.NORMAStage._
import OLK.Sum._
import scala.collection.mutable.ArrayBuffer

/*
 This file implements the NORMA algorithm
 */

class IOBundle(val bitWidth : Int, val fracWidth : Int, val features : Int) extends Bundle {
  val reset   = Bool(INPUT)
  val forceNA = Bool(INPUT)

  val gamma   = Fixed(INPUT, bitWidth, fracWidth)
  val forget  = Fixed(INPUT, bitWidth, fracWidth)
  val eta     = Fixed(INPUT, bitWidth, fracWidth)
  val nu      = Fixed(INPUT, bitWidth, fracWidth)
  val example = Vec.fill(features){Fixed(INPUT, bitWidth, fracWidth)}

  val ft      = Fixed(OUTPUT, bitWidth, fracWidth)
}

class IOBundle_C(bitWidth : Int, fracWidth : Int, features : Int) extends IOBundle(bitWidth, fracWidth, features) {
  val yC = Bool(INPUT)
}

class IOBundle_R(bitWidth : Int, fracWidth : Int, features : Int) extends IOBundle(bitWidth, fracWidth, features) {
  val yReg = Fixed(INPUT, bitWidth, fracWidth)
}

class NORMA(val bitWidth : Int, val fracWidth : Int, val stages : ArrayBuffer[Boolean],
  val log2Table : Int, val dictionarySize : Int, val features : Int, val appType : Int) extends Module {

  Predef.assert(fracWidth > log2Table, "Frac width too small or table too big")

  def log2Dict : Int = { log2Up(dictionarySize) }
  def log2Feat : Int = { log2Up(features) }
  def kStages : Int = { log2Feat + 7 }
  def sStages : Int = { stages.length - kStages - 1 }
  def pCycles : Int = { stages.count(_ == true) }
  def kCycles : Int = { stages.take(kStages).count(_ == true) }
  def sCycles : Int = { pCycles - kCycles - 1 }

  val ZERO = Fixed(0, bitWidth, fracWidth)
  var yReg = ZERO
  var yC   = Bool(true)
  val io = {
    if ( appType == 1 ) {
      val res = new IOBundle_C(bitWidth, fracWidth, features)
      yC = res.yC
      res
    } else if ( appType == 2 )
      new IOBundle(bitWidth, fracWidth, features)
    else {
      val res = new IOBundle_R(bitWidth, fracWidth, features)
      yReg = res.yReg
      res
    }
  }
  val dictModule   = Module(new Dict(bitWidth, fracWidth, dictionarySize, features, pCycles, true))
  val manageModule = Module(new Manage(bitWidth, fracWidth, pCycles - 1, true, appType))
  val manageModuleIO = manageModule.io.asInstanceOf[NORMAIOBundle]
  val kernelModule = Module(new Gaussian(bitWidth, fracWidth, dictionarySize, features, pCycles, stages.take(kStages), 1 << log2Table))
  val sumModule    = Module(new SumStage(bitWidth, fracWidth, stages.drop(kStages).take(sStages), dictionarySize, true))
  val normaModule  = Module(new NORMAStage(bitWidth, fracWidth, appType))
  val gammaReg     = Reg(init=ZERO, next = io.gamma) // Register to buffer gamma

  // Variable Connections
  if (appType == 1){
    val manageModuleIOc = manageModuleIO.asInstanceOf[NORMAcIOBundle]
    val normaModuleIOc  = normaModule.io.asInstanceOf[NORMAStage.IOBundle_C]
    manageModuleIOc.yCin := yC
    normaModuleIOc.yC    := manageModuleIOc.yCout
    printf("yC: %d\n", manageModuleIOc.yCout)
  }
  if (appType == 3) {
    val manageModuleIOr = manageModuleIO.asInstanceOf[NORMArIOBundle]
    val normaModuleIOr  = normaModule.io.asInstanceOf[NORMAStage.IOBundle_R]
    manageModuleIOr.yRegin := yReg
    normaModuleIOr.yReg    := manageModuleIOr.yRegout
    printf("yReg: %d\n", manageModuleIOr.yRegout)
  }

  // Dict Inputs
  dictModule.io.forget    := manageModuleIO.forgetout
  dictModule.io.reset     := manageModuleIO.resetout
  dictModule.io.example   := io.example
  dictModule.io.alpha     := normaModule.io.alpha
  dictModule.io.addToDict := normaModule.io.addToDict

  // Manage Inputs
  manageModuleIO.eta      := io.eta
  manageModuleIO.nu       := io.nu
  manageModuleIO.forgetin := io.forget
  manageModuleIO.resetin  := io.reset
  manageModuleIO.forceNAin := io.forceNA

  // Kernel Inputs
  kernelModule.io.pipeline   := dictModule.io.currentPipeline
  kernelModule.io.dictionary := dictModule.io.currentDict
  kernelModule.io.example    := io.example
  kernelModule.io.gamma      := gammaReg
  kernelModule.io.addToDict  := normaModule.io.addToDict

  // Sum Inputs
  sumModule.io.zi := kernelModule.io.pipelineOut
  sumModule.io.vi := kernelModule.io.dictOut
  sumModule.io.alphai := dictModule.io.currentAlpha
  sumModule.io.alpha  := normaModule.io.alpha
  sumModule.io.forget := manageModuleIO.forgetout
  sumModule.io.addToDict := normaModule.io.addToDict

  // Norma Stage Inputs
  normaModule.io.reset   := manageModuleIO.resetout
  normaModule.io.forceNA := manageModuleIO.forceNAout
  normaModule.io.sum     := sumModule.io.sum
  normaModule.io.zp      := sumModule.io.zp
  normaModule.io.wD      := sumModule.io.wD
  normaModule.io.forget  := manageModuleIO.forgetout
  normaModule.io.etapos  := manageModuleIO.etapos
  normaModule.io.etaneg  := manageModuleIO.etaneg
  normaModule.io.etanu   := manageModuleIO.etanu
  normaModule.io.etanu1  := manageModuleIO.etanu1

  // Outputs
  io.ft := normaModule.io.ft

  // Debugging
  printf("addToDict: %d\n", normaModule.io.addToDict)
  printf("sum: %d\n", sumModule.io.sum)
  printf("zp: %d\n", sumModule.io.zp)
  printf("wD: %d\n", sumModule.io.wD)
  printf("forceNA: %d\n", manageModuleIO.forceNAout)
  printf("alpha: %d\n", normaModule.io.alpha)

  printf("currentPipeline\n")
  for (i <- 0 until pCycles ){
    printf("{")
    for (j <- 0 until features)
      printf("%d, ", dictModule.io.currentPipeline(i)(j))
    printf("}\n")
  }

  printf("currentDict\n")
  for (i <- 0 until dictionarySize ) {
    printf("%d {", dictModule.io.currentAlpha(i))
    for (j <- 0 until features)
      printf("%d, ", dictModule.io.currentDict(i)(j))
    printf("}\n")
  }

}

class NORMATests(c : NORMA) extends Tester(c) {
  // Generate Table for linear interpolation
  val gradients   = new ArrayBuffer[Int]()
  val offsets     = new ArrayBuffer[Int]()
  // Fixed point increment
  val increment   = 1.0 / (1 << c.log2Table)
  val tableEnd    = 1.0
  var xtmp = 0.0
  // NOTE: xtmp is positive, therefore gradient is negitive
  while (xtmp < tableEnd) {
    // m = (y1 - y2)/(x1 - x2)
    val m = -(scala.math.pow(2, - xtmp) - scala.math.pow(2,- xtmp - increment))/increment
    // convert to Fixed
    gradients += (m  * (1 << c.fracWidth)).toInt
    // b = y1 - m*x1
    val b = scala.math.pow(2, - xtmp) - m*xtmp
    // convert to Fixed
    offsets += (b * (1 << c.fracWidth)).toInt
    xtmp += increment
  }

  val r = scala.util.Random

  // generate parameters
  val gamma = r.nextInt( 1 << c.fracWidth )
  val forget = r.nextInt( 1 << c.fracWidth )
  val eta = r.nextInt( 1 << c.fracWidth )
  val nu = r.nextInt( 1 << c.fracWidth )

  def addExample(alpha : Int, example : ArrayBuffer[Int], forget : Int,
    weights : ArrayBuffer[Int], dictionary : ArrayBuffer[ArrayBuffer[Int]]) : Unit = {
    var newWeight = weights(0)
    var newExample = dictionary(0)
    weights(0) = alpha
    dictionary(0) = example
    for (i <- 1 until weights.length) {
      val oldWeight = weights(i)
      val oldExample = dictionary(i)
      weights(i) = (newWeight*forget) >> c.fracWidth
      dictionary(i) = newExample
      newWeight = oldWeight
      newExample = oldExample
    }
  }

  def forgetDict(forget : Int, weights : ArrayBuffer[Int]) : Unit = {
    for (i <- 0 until weights.length)
      weights(i) = (weights(i)*forget) >> c.fracWidth
  }

  def gaussian(x : Int) : Int = {
    val xVal = (gamma*x) >> c.fracWidth
    val xFrac = xVal & ((1 << c.fracWidth ) - 1)
    val xInt = xVal >> c.fracWidth
    val xTabl = xFrac >> (c.fracWidth - c.log2Table)
    val yFrac = ((gradients(xTabl)*xFrac) >> c.fracWidth) + offsets(xTabl)
    yFrac >> xInt
  }

  def computeK(example : ArrayBuffer[Int], dictionary : ArrayBuffer[ArrayBuffer[Int]]) : ArrayBuffer[Int] = {
    val K = new ArrayBuffer[Int]()
    for (i <- 0 until dictionary.length) {
      val l2norm = (example zip dictionary(i)).map(pair => { pair._1 - pair._2 }).map(x => { (x*x) >> c.fracWidth }).sum
      K.append(gaussian(l2norm))
    }
    K
  }

  def computeFT(example : ArrayBuffer[Int], weights : ArrayBuffer[Int], dictionary : ArrayBuffer[ArrayBuffer[Int]]) : Int = {
    val K = computeK(example, dictionary)
    (weights zip K).map(pair => { (pair._1 * pair._2) >> c.fracWidth }).sum
  }

  val dictionary = ArrayBuffer.fill(c.dictionarySize){ArrayBuffer.fill(c.features){0}}
  val weights = ArrayBuffer.fill(c.dictionarySize){0}

  val cycles = 5*c.pCycles
  poke(c.io.reset, Bool(false).litValue())
  poke(c.io.forceNA, Bool(false).litValue())
  poke(c.io.gamma, BigInt(gamma))
  poke(c.io.forget, BigInt(forget))
  poke(c.io.nu, BigInt(nu))
  poke(c.io.eta, BigInt(eta))

  val expectedFt = ArrayBuffer.fill(c.pCycles - 1){0}
  val expectedAlpha = ArrayBuffer.fill(c.pCycles - 1){0}
  val expectedAdd = ArrayBuffer.fill(c.pCycles - 1){false}
  val expectedyC = ArrayBuffer.fill(c.pCycles - 2){false}
  val expectedEx = ArrayBuffer.fill(c.pCycles - 1){ArrayBuffer.fill(c.features){0}}
  val ONE = 1 << c.fracWidth
  var rho = 0
  var b = 0
  var alpha = 0

  for ( cyc <- 0 until cycles ) {
    val example = ArrayBuffer.fill(c.features){r.nextInt(1 << (c.bitWidth/2))}
    val yC   = (r.nextInt(2) == 1)
    val yReg = r.nextInt(1 << c.fracWidth)
    val ft = computeFT(example, weights, dictionary)
    expectedFt += { if (c.appType == 2) ft - rho else ft }
    expectedyC += yC
    expectedEx += example

    println("weights: " + weights)
    var addToDict = false
    // Calculate Stage update
    if (c.appType == 1) {
      // classification
      val g = { if (yC) { ft + b } else { - ft - b } }
      addToDict = (rho - g) > 0
      alpha = { if (yC) { eta } else { -eta } }
      b = { if (addToDict) {
        if (yC) { b + eta } else { b - eta }
      } else b }
      rho = rho + ((eta*nu) >> c.fracWidth )
      if (addToDict)
        rho = rho - eta
    } else if (c.appType == 2) {
      // novelty
      addToDict = (rho - ft) > 0
      alpha = eta
      rho = rho + ((eta*nu) >> c.fracWidth )
      if (addToDict)
        rho = rho - eta
    } else {
      // regression
      val d = yReg - ft
      val sign = { if (d > 0) 1 else -1 }
      addToDict = (sign*d - rho) > 0
      alpha = sign*eta
      rho = rho - ((eta*nu) >> c.fracWidth )
      if (addToDict)
        rho = rho + eta
    }
    if (addToDict)
      addExample(alpha, example, forget, weights, dictionary)
    else
      forgetDict(forget, weights)

    expectedAlpha += alpha
    expectedAdd   += addToDict
    println("yC(" + cyc + "): " + expectedyC(cyc))
    println("example(" + cyc + "): " + expectedEx(cyc))
    println("ft(" + cyc + "): " + expectedFt(cyc))
    println("alpha(" + cyc + "): " + expectedAlpha(cyc))
    println("addToDict(" + cyc + "): " + expectedAdd(cyc))

    // Send values
    (c.io.example zip example).map(pair => { poke(pair._1, BigInt(pair._2)) } )
    if (c.appType == 1) {
      val NORMAcIO = c.io.asInstanceOf[IOBundle_C]
      poke(NORMAcIO.yC, Bool(yC).litValue())
    }
    if (c.appType == 3) {
      val NORMArIO = c.io.asInstanceOf[IOBundle_R]
      poke(NORMArIO.yReg, BigInt(yReg))
    }

    step(1)
    // Expect output
    expect(c.io.ft, BigInt(expectedFt(cyc)))
  }
}

