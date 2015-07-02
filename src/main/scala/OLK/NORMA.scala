/** NORMA.scala -> This file is the top level for NORMA in Chisel
Copyright (C) 2015 Stephen Tridgell

This file is part of a pipelined OLK application.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this code.  If not, see <http://www.gnu.org/licenses/>.
*/

package OLK

import Chisel._
import OLK.Dict._
import OLK.Kernel._
import OLK.Manage._
import OLK.NORMAStage._
import OLK.Sum._
import scala.collection.mutable.ArrayBuffer
import com.github.tototoshi.csv._
import scala.util.control.Breaks._

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

  val ft        = Fixed(OUTPUT, bitWidth, fracWidth)
  val addToDict = Bool(OUTPUT)
}

class IOBundle_C(bitWidth : Int, fracWidth : Int, features : Int) extends IOBundle(bitWidth, fracWidth, features) {
  val yC = Bool(INPUT)
}

class IOBundle_R(bitWidth : Int, fracWidth : Int, features : Int) extends IOBundle(bitWidth, fracWidth, features) {
  val yReg = Fixed(INPUT, bitWidth, fracWidth)
}

class NORMA(val bitWidth : Int, val fracWidth : Int, val stages : ArrayBuffer[Boolean],
  val log2Table : Int, val dictionarySize : Int, val features : Int, val appType : Int,
  val paramFilename : String, val inputFilename : String, val outputFilename : String) extends Module {

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
    //printf("yC: %d\n", manageModuleIOc.yCout)
  }
  if (appType == 3) {
    val manageModuleIOr = manageModuleIO.asInstanceOf[NORMArIOBundle]
    val normaModuleIOr  = normaModule.io.asInstanceOf[NORMAStage.IOBundle_R]
    manageModuleIOr.yRegin := yReg
    normaModuleIOr.yReg    := manageModuleIOr.yRegout
    //printf("yReg: %d\n", manageModuleIOr.yRegout)
  }

  // Dict Inputs
  dictModule.io.forget    := manageModuleIO.forgetout
  dictModule.io.reset     := manageModuleIO.resetout
  dictModule.io.forceNA   := normaModule.io.forceNAout
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
  io.ft        := normaModule.io.ft
  io.addToDict := normaModule.io.addToDict

  // Debugging
  /*
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
   */
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

  def checkFile(filename : String, ext : String) : Unit = {
    Predef.assert(filename.substring(filename.lastIndexOf(".") + 1) == ext, "File " + filename + " must have extension of ." + ext)
    Predef.assert(java.nio.file.Files.exists(java.nio.file.Paths.get(filename)), "File " + filename + " does not exist")
  }

  def tryToBool(myBool : String, message : String) : Boolean = {
    try {
      myBool.trim.toBoolean
    } catch {
      case x:Exception => throw new Exception(message)
    }
  }

  def tryToFixed(myDouble : String, message : String) : Int = {
    try {
      (myDouble.trim.toDouble * ( 1 << c.fracWidth )).toInt
    } catch {
      case x:Exception => throw new Exception(message)
    }
  }

  def fromPeek(myNum : BigInt) : Int = {
    if (myNum.toInt >= (1 << (c.bitWidth - 1)))
      myNum.toInt - (1 << c.bitWidth)
    else
      myNum.toInt
  }

  //  val r = scala.util.Random

  // read parameters
  val reader = CSVReader.open(new java.io.File(c.paramFilename))
  val lines = reader.all
  reader.close
  Predef.assert(lines.length >= 3, "Insufficient lines in parameter file " + c.paramFilename)
  Predef.assert(lines(2).length <= 4, "Insufficient number of parameters in line 3 of parameter file " + c.paramFilename)
  val gamma  = tryToFixed(lines(2)(0), "Could not cast gamma = "  + lines(2)(0) + " to a double or int")
  val forget = tryToFixed(lines(2)(1), "Could not cast forget = " + lines(2)(1) + " to a double or int")
  val eta    = tryToFixed(lines(2)(2), "Could not cast eta = "    + lines(2)(2) + " to a double or int")
  val nu     = tryToFixed(lines(2)(3), "Could not cast nu = "     + lines(2)(3) + " to a double or int")

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
  poke(c.io.gamma, BigInt(gamma))
  poke(c.io.forget, BigInt(forget))
  poke(c.io.nu, BigInt(nu))
  poke(c.io.eta, BigInt(eta))

  val expectedFt = ArrayBuffer.fill(c.pCycles - 1){0}
  val expectedAlpha = ArrayBuffer.fill(c.pCycles - 1){0}
  val expectedAdd = ArrayBuffer.fill(c.pCycles - 1){false}
  val expectedyC = ArrayBuffer.fill(c.pCycles - 2){false}
  val expectedyReg = ArrayBuffer.fill(c.pCycles - 1){0}
  val expectedEx = ArrayBuffer.fill(c.pCycles - 1){ArrayBuffer.fill(c.features){0}}
  val ONE = (1 << c.fracWidth)
  var rho = 0
  var b = 0
  var alpha = 0
  var cyc = 0

  var numFeatures = -1

  // Open input and output files for reading and writing respectively
  checkFile(c.inputFilename, "csv")
  Predef.assert(c.outputFilename.substring(c.outputFilename.lastIndexOf(".") + 1) == "csv",
    "File " + c.outputFilename + " must have extension of .csv")
  val inputReader   = CSVReader.open(new java.io.File(c.inputFilename))
  val inputIterator = inputReader.iterator
  val outputWriter  = CSVWriter.open(new java.io.File(c.outputFilename))
  outputWriter.writeAll(lines.take(3)) // Write parameters to output so a record is attached
  outputWriter.writeRow(List(c.inputFilename))

  while ( inputIterator.hasNext ) {
    val inputExLine = inputIterator.next
    if (numFeatures == -1)
      numFeatures = inputExLine.length - 3
    if (numFeatures < 1 || inputExLine.length != numFeatures + 3 || numFeatures != c.features) {
      println("Line " + cyc + " has incorrect number of values, aborting")
      break
    }
    val reset   = tryToBool(inputExLine(0), "Could not read reset as Boolean on line " + cyc)
    val forceNA = tryToBool(inputExLine(1), "Could not read forceNA as Boolean on line " + cyc)
    val yReg    = tryToFixed(inputExLine(2), "Could not convert y to fixed on line " + cyc)
    val yC      = (yReg == ONE) // use 1 and 0 or 1 and -1 for yC
    val example = inputExLine.drop(3).map(x => {
      tryToFixed(x, "Could not convert a feature to fixed on line " + cyc)
    }).to[ArrayBuffer]
    /*
    val example = ArrayBuffer.fill(c.features){r.nextInt(1 << (c.bitWidth/2))}
    val yC   = (r.nextInt(2) == 1)
    val yReg = r.nextInt(1 << c.fracWidth)
     */

    val ft = computeFT(example, weights, dictionary)
    expectedFt += { if (c.appType == 2) ft - rho else ft }
    expectedyC += yC
    expectedyReg += yReg
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
    poke(c.io.reset, Bool(reset).litValue())
    poke(c.io.forceNA, Bool(forceNA).litValue())
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
    // expect(c.io.ft, BigInt(expectedFt(cyc)))
    // put Output into file
    if ( cyc >= (c.pCycles - 1))
      outputWriter.writeRow(List(peek(c.io.addToDict) == 1, (expectedyReg(cyc).toDouble / ONE), (fromPeek(peek(c.io.ft)).toDouble / ONE)))

    cyc += 1
  }
  // Push the last few through the pipeline
  for ( i <- 0 until (c.pCycles - 1) ) {
    step(1)
    outputWriter.writeRow(List(peek(c.io.addToDict) == 1, (expectedyReg(cyc + i).toDouble / ONE), (fromPeek(peek(c.io.ft)).toDouble / ONE)))
  }
  inputReader.close()
  outputWriter.close()
}

