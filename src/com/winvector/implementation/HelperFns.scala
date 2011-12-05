package com.winvector.implementation

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */

import com.winvector.definition.NumberBase
import com.winvector.definition.Field
import com.winvector.definition.VectorFN

object HelperFns {

  val copyrightStr = "Copyright 2010 John Mount / Win-Vector LLC \n Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html) \n For details/instructions see 'Automatic Differentiation with Scala' from www.win-vector.com"
  
  // small helper fns
  
  def dot(x:Array[Double],y:Array[Double]):Double = {
    var r:Double = 0.0
    val dim = x.length
    for(i <- 0 to (dim-1)) {
      r = r + x(i)*y(i)
    }
    r
  }

  def normSQ(x:Array[Double]) = dot(x,x)
  
  def addD(a:Array[Double],wb:Double,b:Array[Double]) = {
    val dim = a.length
    val r = new Array[Double](dim)
    for(i <- 0 to (dim-1)) {
      r(i) = a(i) + wb*b(i)
    }
    r
  }
  
  def neg(x:Array[Double]) = {
    val dim = x.length
    val r = new Array[Double](dim)
    for(i <- 0 to (dim-1)) {
      r(i) = -x(i)
    }
    r
  }
  
  def mean(d:Array[Array[Double]]) = {
    val npt = d.length
    val dim = d(0).length
    val r = new Array[Double](dim)
    for(i <- 0 to npt-1) {
      for(j <- 0 to dim-1) {
        r(j) = r(j) + d(i)(j)
      }
    }
    val scale = 1.0/(npt+0.0)
    for(j <- 0 to dim-1) {
      r(j) = scale*r(j)
    }
    r
  } 

  def copy(x:Array[Double]) = {
    val dim = x.length
    val r = new Array[Double](dim)
    for(i <- 0 to (dim-1)) {
      r(i) = x(i)
    }
    r
  }

  def sub(x:Array[Double],y:Array[Double]) = addD(x,-1.0,y)

  
  def injectV[Y<:NumberBase[Y]](field:Field[Y],p:Array[Double]):Array[Y] = {
    val dim = p.length
    val ret = field.array(dim)
    for(i <- 0 to (dim-1)) {
      ret(i) = field.inject(p(i))
    }
    ret
  }
  
  def projectV[Y<:NumberBase[Y]](p:Array[Y]):Array[Double] = {
    val dim = p.length
    val ret = new Array[Double](dim)
    if(dim>0) {
      val field = p(0).field
      for(i <- 0 to (dim-1)) {
    	ret(i) = field.project(p(i))
      }
    }
    ret
  }
  
  // printing  
  
  def printV[Y<:NumberBase[Y]](v:Array[Y]) = {
    val dim = v.length
    print("[")
    for(i <- 0 to (dim-1)) {
      print("\t" + v(i))
    }
    println("\t]")
  }
  
  def printD(v:Array[Double]) = {
    val dim = v.length
    print("[")
    for(i <- 0 to (dim-1)) {
      print("\t" + v(i))
    }
    println("\t]")
  }
  
  def printPD(v:(Double,Array[Double])) = {
    val (va,vb) = v
    val dim = vb.length
    print("( " + va + " [")
    for(i <- 0 to (dim-1)) {
      print("\t" + vb(i))
    }
    println("\t] )")
  }
  
  def printM(m:Array[Array[Double]]) = {
    val dim = m.length
    println("[")
    for(i <- 0 to (dim-1)) {
      print("\t")
      printD(m(i))
    }
    println("]")
  }

}
