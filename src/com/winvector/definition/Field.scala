package com.winvector.definition

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */

abstract class Field [NUMBERTYPE <: NumberBase[NUMBERTYPE]] {
  def zero:NUMBERTYPE            // return canonical zero in field
  def one:NUMBERTYPE             // return canonical one in field
  def inject(v:Double):NUMBERTYPE  // return canonical representation of number in field
  def project(v:NUMBERTYPE):Double // return standard-number represented in field
  def array(n:Int):Array[NUMBERTYPE] // return an array of this type
  def representationNorm(v:NUMBERTYPE):Double // check distance in representation from standard zero 
}
