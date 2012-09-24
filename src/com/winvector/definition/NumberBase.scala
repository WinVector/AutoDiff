package com.winvector.definition

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */


abstract class NumberBase[NUMBERTYPE <: NumberBase[NUMBERTYPE]] {
  this: NUMBERTYPE => // self type, see http://stackoverflow.com/questions/12556558/how-to-declare-type-parametrized-arithmetic-in-scala and http://stackoverflow.com/questions/1990948/what-is-the-difference-between-scala-self-types-and-trait-subclasses
  // basic arithmetic
  def + (that: NUMBERTYPE):NUMBERTYPE
  def - (that: NUMBERTYPE):NUMBERTYPE
  def * (that: NUMBERTYPE):NUMBERTYPE
  def / (that: NUMBERTYPE):NUMBERTYPE  // that not equal to zero
  // more complicated
  def pospow(that:Double):NUMBERTYPE // both "this" and that should be positive 
  def exp:NUMBERTYPE
  def log:NUMBERTYPE // this is positive
  def sqrt:NUMBERTYPE
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
  // to standard number
  def project:Double
  // comparison functions
  def nonZero:Boolean = {
    scala.math.abs(project)!=0.0
  }
  def > (that: NUMBERTYPE):Boolean = {
    project>that.project
  }
  def >= (that: NUMBERTYPE):Boolean = {
    project>=that.project
  }
  def == (that: NUMBERTYPE):Boolean = {
    project==that.project
  }
  def != (that: NUMBERTYPE):Boolean = {
    project!=that.project
  }
  def < (that: NUMBERTYPE):Boolean = {
    project<that.project
  }
  def <= (that: NUMBERTYPE):Boolean = {
    project<=that.project
  }
  // utility
  def field:Field[NUMBERTYPE]
  // derived
  def neg:NUMBERTYPE = {
    field.zero - this
  }
  def unary_-():NUMBERTYPE = {
    field.zero - this
  } 
  def sq:NUMBERTYPE = { 
    this*this
  }
  def sigmoid:NUMBERTYPE = {
    val e = exp
    e/(field.one + e)
  }
  def abs:NUMBERTYPE = {
    if(project>=0.0) {
      this
    } else {
      field.zero - this
    }
  }
  def max(that: NUMBERTYPE):NUMBERTYPE = {
    if(this>=that) {
      this
    } else {
      that
    }
  }
  def min(that: NUMBERTYPE):NUMBERTYPE = {
    if(this<=that) {
      this
    } else {
      that
    }
  }
  def ipow(pw:Int):NUMBERTYPE = {
    if(pw<=3) { // small cases and all negative numbers
      pw match {
        case 3 => this*this*this
        case 2 => this*this
        case 1 => this
        case 0 => field.one
        case -1 => field.one/this
        case -2 => field.one/(this*this)
        case -3 => field.one/(this*this*this)
        case _ => (field.one/this).ipow(-pw) 
      }
    } else { // pw>0 and pw = 2*p2 + p1
      // recurse on x^pw = (x^p2)^2 (x^p1)
      val p2 = pw/2
      val p1 = pw - 2*p2
      val x2 = ipow(p2)
      p1 match {
        case 1 => x2*x2*this
        case _ => x2*x2
      }
    }
  }
}
