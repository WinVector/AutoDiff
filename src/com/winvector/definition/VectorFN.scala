package com.winvector.definition

import com.winvector.implementation.MDouble
import com.winvector.implementation.FDouble

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */

// not really the Scala Function1 trait (due to extra free type parameters) but imitating the form for later use
abstract trait VectorFN {
  def apply[Y<:NumberBase[Y]](x:Array[Y]):Y
  
  /**
   * specialize to standard double array for convenience
   */
  def apply(x:Array[Double]):Double = {
    val field = FDouble
    val n = x.length
    val y = field.array(n)
    for(i <- 0 until n) {
      y(i) = field.inject(x(i))
    }
    val fy = apply(y)
    fy.project
  }
}
