package com.winvector.definition

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */

// not really the Scala Function1 trait (due to extra free type parameters) but imitating the form for later use
abstract trait SummableFN {
  def dim: Int
  def apply[Y<:NumberBase[Y]](parameterArg:Array[Y],varyingArg:Array[Y]):Y
}
