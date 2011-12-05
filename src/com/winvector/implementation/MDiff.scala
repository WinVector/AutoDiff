package com.winvector.implementation

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */

import com.winvector.definition.VectorFN
import com.winvector.definition.GFunction

class MDiff(f:VectorFN) extends GFunction {

  def apply(x: Array[Double]): Double = FDouble.project(f(HelperFns.injectV(FDouble,x)))
  
  def gradEval(x: Array[Double]):(Double,Array[Double]) = { 
    val xdim = x.length
    val grad = new Array[Double](xdim)
    val delta:Double = 1.0e-6
    val x2 = HelperFns.injectV(FDouble,x)
    val fbase = FDouble.project(f(x2))
    for(i <- 0 to (xdim-1)) {
      val xorig = x2(i) 
      x2(i) = FDouble.inject(x(i) + delta)
      val fplus = FDouble.project(f(x2))
      x2(i) = xorig
      grad(i) = (fplus-fbase)/delta
    }
    (fbase,grad)
  }
}
