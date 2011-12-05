package com.winvector.definition

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */

trait GFunction {
  def apply(x:Array[Double]):Double                      // f(x)
  def gradEval(x:Array[Double]):(Double,Array[Double]) // f(x) gradient(f(x))
}
