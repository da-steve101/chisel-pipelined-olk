/** OLK.scala -> This file is the top level for an OLK implementation
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
import OLK._

/** OLK
  This file links the pipeline stages together
  to form the OLK pipeline
  parameter k : kernel stages
  parameter s : sum stages
  parameter f : no features
  parameter d : no dictionary
  parameter type : OLKc, OLKr, OLKn

  parameter p = k + s + 1

  input example = SFix[f]
  input y       = (0 : OLKn, 1 Bit : OLKc, SFix : OLKr)
  input forceNoAdd = 1 Bit

  output queryVal = SFix

  Blocks:
  Dict_ = Dict(example)
  Manage_ = Manage(y, forceNoAdd)
  Kernel_ = Kernel()
  SumStage_ = SumStage()
  AlphaStage_ = AlphaStage()

  ft = AlphaStage_ft
  OLKn:
      queryVal = ft - 1
  OLKc/OLKr:
      queryVal = ft

  */
