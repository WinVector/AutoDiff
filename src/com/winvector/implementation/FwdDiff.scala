package com.winvector.implementation

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */

import com.winvector.definition.VectorFN
import com.winvector.implementation.HelperFns._
import com.winvector.definition.GFunction

class FwdDiff(fn:VectorFN) extends GFunction {
  
  def apply(x: Array[Double]): Double = fn.apply[MDouble](injectV(FDouble,x)).project
  
  def gradEval(x: Array[Double]):(Double,Array[Double]) = { 
    val xdim = x.length
    val x2 = injectV(FDualNumber,x)
    val grad = new Array[Double](xdim)
    var std:Double = 0.0
    for(i <- 0 to (xdim-1)) {
      val xorig = x2(i)
      x2(i) = FDualNumber.inject(x(i)) + FDualNumber.delta
      val fplus = fn.apply[DualNumber](x2)
      x2(i) = xorig
      std = fplus.project
      grad(i) = FDualNumber.ideal(fplus)
    }
    (std,grad)
  }
}
