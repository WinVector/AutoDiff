package com.winvector.implementation

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */

import com.winvector.definition.NumberBase
import com.winvector.definition.Field

object FDualNumber extends Field[DualNumber] {
  private val z = new DualNumber(0.0,0.0)
  private val o = new DualNumber(1.0,0.0)
  private val i = new DualNumber(0.0,1.0)
  def zero = z
  def one = o
  def delta = i
  def inject(v:Double) = new DualNumber(v,0.0)
  def ideal(v:DualNumber) = v.inf
  def array(n:Int) = { 
	  val a = new Array[DualNumber](n)
	  for(i <- 0 until n) {
	 	  a(i) = zero
	  }
	  a
  }
  override def toString = "FDualNumber"
  def representationNorm(v:DualNumber):Double = { scala.math.abs(v.std) + scala.math.abs(v.inf) }
}


class DualNumber private[implementation] (private [implementation] val std:Double, private [implementation] val inf:Double) extends NumberBase[DualNumber] {
  // basic arithmetic
  def + (that: DualNumber) = new DualNumber(std + that.std,inf + that.inf)
  def - (that: DualNumber) = new DualNumber(std - that.std,inf - that.inf)
  def * (that: DualNumber) = new DualNumber(std * that.std,std * that.inf + inf * that.std)
  def / (that: DualNumber) = {
    if(that.std==0.0) {
      throw new IllegalArgumentException("Tried to divide by zero")
    }
    new DualNumber(std/that.std,(inf*that.std-std*that.inf)/(that.std*that.std))
  }
  
  def project = std
  
  // more complicated
  def pow(exp:Double) = {
    if(std<0) {
      throw new IllegalStateException("Tried to pow() negative number")
    }
    if(exp==0.0) {
      FDualNumber.one
    } else { 
      val p = scala.math.pow(std,exp) 
      new DualNumber(p,exp*p*inf/std)
    }
  }
  
  def log = {
    if(std<=0) {
      throw new IllegalStateException("Tried to log() non-positive number")
    }
    new DualNumber(scala.math.log(std),inf/std)
  }
  
  def exp = {
    val e = scala.math.exp(std)
    new DualNumber(e,e*inf)
  }
  def sqrt = {
    if(std<0) {
      throw new IllegalStateException("Tried to sqrt() negative number")
    }
    val s = scala.math.sqrt(std)
    new DualNumber(s,inf/(2*s))
  }
              
  // utility
  def field:Field[DualNumber] = FDualNumber
  override def toString = "(" + std + "," + inf +")"
  
  // more special fns
  def sin = new DualNumber(scala.math.sin(std),inf*scala.math.cos(std))
  def cos = new DualNumber(scala.math.cos(std),-inf*scala.math.sin(std))
  def tan = new DualNumber(scala.math.tan(std),inf/(scala.math.cos(std)*scala.math.cos(std)))
  def asin = new DualNumber(scala.math.asin(std),inf/scala.math.sqrt(1-std*std))
  def acos = new DualNumber(scala.math.acos(std),-inf/scala.math.sqrt(1-std*std))
  def atan = new DualNumber(scala.math.atan(std),inf/(1.0+std*std))
  def sinh = new DualNumber(scala.math.sinh(std),inf*scala.math.cosh(std))
  def cosh = new DualNumber(scala.math.cosh(std),inf*scala.math.sinh(std))
  def tanh = new DualNumber(scala.math.tanh(std),inf/(scala.math.cosh(std)*scala.math.cosh(std)))
}
