package com.winvector.demo

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */


import com.winvector.definition.NumberBase
import com.winvector.definition.VectorFN
import com.winvector.implementation.MDouble
import com.winvector.implementation.FDouble
import com.winvector.implementation.HelperFns._
import com.winvector.opt.CG
import com.winvector.implementation.FwdDiff
import com.winvector.implementation.NumDiff
import com.winvector.reva.RevDiff
import com.winvector.reva.RevDiffQ


object Print {
  def main(args : Array[String]) : Unit = {
    val genericFx = new VectorFN {
      def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
        (p(0).sin - p(1).tan).sq/p(2)
      }
    }

    val rdq = new RevDiffQ(genericFx)
    val p0:Array[Double] = Array(3.1,-2.2,2.0)
    printPD(rdq.gradEval(p0))
    println(rdq)
  }
}
