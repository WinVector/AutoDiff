package com.winvector.implementation

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */

import com.winvector.definition.GFunction
import com.winvector.implementation.HelperFns._

class NumDiff(f:Array[Double]=>Double) extends GFunction {

  def apply(x: Array[Double]): Double = f(x)
  
  def gradEval(x: Array[Double]):(Double,Array[Double]) = { 
    val xdim = x.length
    val x2 = copy(x)
    val grad = new Array[Double](xdim)
    val delta:Double = 1.0e-6
    val fbase = f(x)
    for(i <- 0 until xdim) {
      val xorig = x(i)
      x2(i) = x(i) + delta
      val fplus = f(x2)
      x2(i) = xorig
      grad(i) = (fplus-fbase)/delta
    }
    (fbase,grad)
  }
}
