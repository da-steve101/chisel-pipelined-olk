package OLK.Sum

import Chisel._
import OLK._

/** SumR
  This function sums the results of the kernel function
  that are currently in the dictionary
  parameter d : dictionary size
  parameter s : stages

  input vi     = SFix[d]  // Outputs from kernel function
  input alphai = SFix[d] // Current weights of dictionary
  input addToDict = 1 Bit
  
  output sumR  = SFix
  output wD    = SFix
  output WD1   = SFix
  output q     = UInt(width=log2(s))

  Registers:
  Stage_0, Stage_1, ... Stage_(s-1)[1]
  Spare_0[s+1], ... Spare_(s-1)[2]
  QReg_0, QReg_1, QReg_2, ... QReg_(s-1) // Count the number of missed factors

  Logic:
  wi = vi.*alphai

  QReg_0 = (addToDict) ? 1 : 0
  QReg_1 = (addToDict) ? QReg_0 + 1 : QReg_0
  QReg_2 = (addToDict) ? QReg_1 + 1 : QReg_1
  QReg_3 = (addToDict) ? QReg_2 + 1 : QReg_2
  ... etc

  Stage_0[0:k] = sum(w1 : w(d-s-2)) + (addTODict) ? w(d-s-1) : 0
  Stage_1[0:l] = sum(Stage_0) + (addToDict) ? Spare_0[0] : 0
  Stage_2[0:m] = sum(Stage_1) + (addToDict) ? Spare_1[0] : 0
  ... etc where m < l < k

  Spare_0 = (addToDict) ? [w(d-s-2):w(d)] : [w(d-s-1):w(d-1)]
  Spare_1 = (addToDict) ? Spare_0[1:end]  : Spare_0[0:end-1]
  Spare_2 = (addToDict) ? Spare_1[1:end]  : Spare_1[0:end-1]
  ... etc

  sumR = Stage_(s-1)
  wD1  = Spare_(s-1)[0]
  wD   = Spare_(s-1)[1]
  q    = QReg_(s-1)
  */
