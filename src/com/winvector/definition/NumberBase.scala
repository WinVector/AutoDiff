package com.winvector.definition

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */


abstract class NumberBase[NUMBERTYPE <: NumberBase[NUMBERTYPE]] {
  // basic arithmetic
  def + (that: NUMBERTYPE):NUMBERTYPE
  def - (that: NUMBERTYPE):NUMBERTYPE
  def unary_-():NUMBERTYPE 
  def * (that: NUMBERTYPE):NUMBERTYPE
  def / (that: NUMBERTYPE):NUMBERTYPE  // that not equal to zero
  // more complicated
  def pow(that:Double):NUMBERTYPE // this is positive and anything to the zero is one (since power is not a varying argument)
  def exp:NUMBERTYPE
  def log:NUMBERTYPE // this is positive
  def sq:NUMBERTYPE
  def sqrt:NUMBERTYPE
  def abs:NUMBERTYPE
  def max(that: NUMBERTYPE):NUMBERTYPE
  def min(that: NUMBERTYPE):NUMBERTYPE
  // more special fns
  def sin:NUMBERTYPE
  def cos:NUMBERTYPE
  def tan:NUMBERTYPE
  def asin:NUMBERTYPE
  def acos:NUMBERTYPE
  def atan:NUMBERTYPE
  def sinh:NUMBERTYPE
  def cosh:NUMBERTYPE
  def tanh:NUMBERTYPE
  // comparison functions
  def > (that: NUMBERTYPE):Boolean
  def >= (that: NUMBERTYPE):Boolean
  def == (that: NUMBERTYPE):Boolean
  def != (that: NUMBERTYPE):Boolean
  def < (that: NUMBERTYPE):Boolean
  def <= (that: NUMBERTYPE):Boolean
  // utility
  def field:Field[NUMBERTYPE]
}
