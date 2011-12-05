package com.winvector.reva

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */

import com.winvector.definition.GFunction
import com.winvector.definition.VectorFN
import com.winvector.implementation.FDouble
import com.winvector.implementation.MDouble
import com.winvector.implementation.HelperFns._

class RevDiff(fn:VectorFN) extends GFunction {

  def apply(x: Array[Double]): Double = FDouble.project(fn.apply[MDouble](injectV(FDouble,x)))

  def gradEval(x:Array[Double]):(Double,Array[Double]) = {
    val dim = x.length
    // define variables
    val xc = new Array[CaptureNumber](dim)
    for(i <- 0 until dim) {
      xc(i) = FCapture.variable(x(i),i)
    }
    // capture calculation as straight line program
    val rc = fn.apply[CaptureNumber](xc)
    val rd = new ReverseDiff(rc)
    //println(rd)
    // compute gradient
    val (fx,gx) = rd.gradient(null)
    //printPD((fx,gx))
    (fx,gx)    
  }

}
