package com.winvector.reva

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */

class IDSource {
  // manage variable IDs (reserve-1 for constants)
  private var nextIndex:Int = 0
  
  def nextID:Int = {
    val r = nextIndex
    nextIndex += 1
    r
  }
}
