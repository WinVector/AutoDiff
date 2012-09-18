package com.winvector.definition

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */


abstract class NumberBase[NUMBERTYPE <: NumberBase[NUMBERTYPE]] {
  // get around type issues
  def self:NUMBERTYPE  // return this (do it where we know concrete types, can't be implemented here as def self:NUMBERTYPE = { this } )
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
  def sq:NUMBERTYPE = { 
    val e = self
    e*e
  }
  def sigmoid:NUMBERTYPE = {
    val e = exp
    e/(field.one + e)
  }
  def abs:NUMBERTYPE = {
    if(project>=0.0) {
      self
    } else {
      -self
    }
  }
  def max(that: NUMBERTYPE):NUMBERTYPE = {
    if(this>=that) {
      self
    } else {
      that
    }
  }
  def min(that: NUMBERTYPE):NUMBERTYPE = {
    if(this<=that) {
      self
    } else {
      that
    }
  }
}
