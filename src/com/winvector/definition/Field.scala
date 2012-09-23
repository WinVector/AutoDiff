package com.winvector.definition

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */

import com.winvector.lin.Matrix

abstract class Field [NUMBERTYPE <: NumberBase[NUMBERTYPE]] {
  def zero:NUMBERTYPE            // return canonical zero in field
  def one:NUMBERTYPE             // return canonical one in field
  def inject(v:Double):NUMBERTYPE  // return canonical representation of number in field
  def array(n:Int):Array[NUMBERTYPE] // return an array of this type
  def injectA(v:Array[Double]):Array[NUMBERTYPE] = {
    val n = v.length
    val r = array(n)
    for(i <- 0 until n) {
      r(i) = inject(v(i))
    }
    r
  }
  def matrix(rows:Int,columns:Int):Matrix[NUMBERTYPE] = {
    new Matrix[NUMBERTYPE](this,rows,columns)
  }
  def identity(n:Int):Matrix[NUMBERTYPE] = {
    Matrix.identity(this,n)
  }
  def injectM(v:Array[Array[Double]]):Matrix[NUMBERTYPE] = {
    Matrix[NUMBERTYPE](this,v)
  }
  def representationNorm(v:NUMBERTYPE):Double // check distance in representation from standard zero, use for testing DualNumber
}
