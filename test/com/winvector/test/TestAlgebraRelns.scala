package com.winvector.test

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */


import junit.framework.TestCase

import com.winvector.definition.NumberBase
import com.winvector.implementation.DualNumber
import com.winvector.implementation.FDouble
import com.winvector.implementation.FDualNumber
import com.winvector.implementation.MDouble
import com.winvector.reva.CaptureNumber
import com.winvector.reva.FCapture

import junit.framework.Assert.assertTrue

class TestAlgebraRelns extends TestCase {
  val debug:Boolean = false
  val epsilon:Double = 1.0e-6
  val base:Array[Double] = Array(1.3,4.5,-2,0,6.0,0.0,1.0,-1.0,2,-1.3,-6,-4.5,10,20,100,-100,1.0e-3,-1.0e-3,0.1,-0.1,1e+3,-1e+3)


  def testAlgF1[Y<:NumberBase[Y]](a:Y):Unit = {
    val field = a.field
    assertTrue(field.representationNorm(a + -a)<epsilon)
    assertTrue(field.representationNorm((a+field.zero)-a)<epsilon)
    assertTrue(field.representationNorm((a-field.zero)-a)<epsilon)
    assertTrue(field.representationNorm(a*field.zero)<epsilon)
    assertTrue(field.representationNorm((a*field.one)-a)<epsilon)
    assertTrue(field.representationNorm((a/field.one)-a)<epsilon)
    assertTrue(field.representationNorm(a-a)<epsilon)
    assertTrue(field.representationNorm(a+(field.zero-a))<epsilon)
    if(scala.math.abs(field.project(a))<20) {
      assertTrue(field.representationNorm(a.exp.log-a)<epsilon)
    }
    if(a!=field.zero) {
      assertTrue(field.representationNorm((a/a)-field.one)<epsilon)
      assertTrue(field.representationNorm(a*(field.one/a)-field.one)<epsilon)
      assertTrue(field.representationNorm(field.one/a + field.one/(field.zero-a))<epsilon)
    }
    if(a>field.zero) {
      assertTrue(field.representationNorm(a.log.exp-a)<epsilon)
      assertTrue(field.representationNorm(((a.log*field.inject(1.3)).exp)-(a.pow(1.3)))<epsilon)
      assertTrue(field.representationNorm(a.pow(2.0)-a*a)<epsilon)
      assertTrue(field.representationNorm(a.pow(0.5)*a.pow(0.5)-a)<epsilon)
      assertTrue(field.representationNorm(a.pow(-1.0)*a-field.one)<epsilon)
      assertTrue(field.representationNorm(a.pow(-0.5)*a.pow(-0.5)-field.one/a)<epsilon)
      assertTrue(field.representationNorm(a.pow(-1.0)*a-field.one)<epsilon)
      assertTrue(field.representationNorm(a*a.pow(-1.0)-field.one)<epsilon)
      assertTrue(field.representationNorm(a.pow(0.0)-field.one)<epsilon)
    }
  }

  def testAlgF2[Y<:NumberBase[Y]](a:Y,b:Y):Unit = {
    val field = a.field
    assertTrue(field.representationNorm((a+b)-(b+a))<epsilon)
    assertTrue(field.representationNorm(((a+b)-a)-b)<epsilon)
    assertTrue(field.representationNorm((a-b)+(b-a))<epsilon)
    assertTrue(field.representationNorm((a*b)-(b*a))<epsilon)
    if(a+b>field.zero) {
      assertTrue(field.representationNorm((a+b).pow(2.0) - (a*a + b*b + a*b + a*b))<epsilon)
    }
    if(b!=field.zero) {
      assertTrue(field.representationNorm(((a/b)*b)-a)<epsilon)
      if(a!=field.zero) {
        assertTrue(field.representationNorm(((a/b)*b)/a-field.one)<epsilon)
        assertTrue(field.representationNorm((a/b)-(field.one/(b/a)))<epsilon)
      }
    }
    if((a>field.zero)&&(b>field.zero)) {
      assertTrue(field.representationNorm((a.log + b.log).exp - a*b)<epsilon)
    }
  }

  def testAlgF3[Y<:NumberBase[Y]](a:Y,b:Y,c:Y):Unit = {
    val field = a.field
    assertTrue(field.representationNorm( (a*(b+c)) - ((a*b) + (a*c)) )<epsilon)
    assertTrue(field.representationNorm( ((b+c)*a) - (b*a + c*a) )<epsilon)
    if(a!=field.zero) {
      assertTrue(field.representationNorm( ((b+c)/a) - (b/a + c/a) )<epsilon)
    }
  }

  
  // check that expected algebra relations hold, even when non-standard elements are considered
  def testAlg[Y<:NumberBase[Y]](a:Array[Y]):Unit = {
    val n = a.length
    for(i <- 0 to (n-1)) {
      testAlgF1(a(i)) 
      for(j <- 0 to (n-1)) {
        testAlgF2(a(i),a(j))
        for(k <- 0 to (n-1)) {
          testAlgF3(a(i),a(j),a(k))
        }
      }
    }
  }

  
  def testAlgMDouble:Unit = {
    val n = base.length
    val a = new Array[MDouble](n)
    for(i <- 0 to (n-1)) {
      a(i) = FDouble.inject(base(i))
    }
    testAlg(a)
  }

  
  def testAlgDualNumber:Unit = {
    val zeroEquivs:Array[DualNumber] = Array( 
      FDualNumber.zero, 
      FDualNumber.delta, 
      -FDualNumber.delta,
      FDualNumber.delta + FDualNumber.delta)
    val nbase = base.length
    val nzero = zeroEquivs.length
    val a = new Array[DualNumber](nbase*nzero)
    var i = 0
    for(bi <- 0 to (nbase-1)) {
      for(zi <- 0 to (nzero-1)) {
        a(i) = FDualNumber.inject(base(bi)) + zeroEquivs(zi)
        i = i + 1
      }
    }
    testAlg(a)
  }
  
  
  def testAlgCaptureNumber:Unit = {
    val n = base.length
    val a = new Array[CaptureNumber](n)
    for(i <- 0 to (n-1)) {
      a(i) = FCapture.inject(base(i))
    }
    testAlg(a)
  }

}
