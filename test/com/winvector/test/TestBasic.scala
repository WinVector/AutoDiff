package com.winvector.test

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */


import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test

import com.winvector.definition.NumberBase
import com.winvector.definition.Field
import com.winvector.implementation.FDouble
import com.winvector.implementation.FDualNumber
import com.winvector.reva.FCapture



class TestBasic extends AssertionsForJUnit {
  
  def checkNear[Y<:NumberBase[Y]](field:Field[Y],a:Double,b:Y):Unit = {
    assertTrue(scala.math.abs(a-b.project)<=1.0e-5)  
  }
  
  // check that number system ops are the same as operating on floating point numbers (so project(a op b) == project(a) op project(b), always or ops commute with projection)
  def testStdF[Y<:NumberBase[Y]](field:Field[Y]):Unit = {
    val a:Array[Double] = Array(1.3,4.5,-2,0,6.0,0.0,1.0,-1.0,2,-1.3,-6,-4.5,10,20,100,-100,1.0e-3,-1.0e-3,0.1,-0.1,1e+3,-1e+3)
    checkNear(field,0,field.zero)
    checkNear(field,1,field.one)
    val n = a.length
    for(i <- 0 to (n-1)) {
      checkNear(field,a(i),field.inject(a(i)))
      checkNear(field,scala.math.abs(a(i)),field.inject(a(i)).abs)
      if(scala.math.abs(a(i))<20) {
        checkNear(field,scala.math.exp(a(i)),field.inject(a(i)).exp)
      }
      if(a(i)>0.0) {
        checkNear(field,scala.math.log(a(i)),field.inject(a(i)).log)
      }
      checkNear(field,scala.math.sin(a(i)),field.inject(a(i)).sin)
      checkNear(field,scala.math.cos(a(i)),field.inject(a(i)).cos)
      checkNear(field,scala.math.tan(a(i)),field.inject(a(i)).tan)
      if(scala.math.abs(a(i))<20) {
        checkNear(field,scala.math.sinh(a(i)),field.inject(a(i)).sinh)
        checkNear(field,scala.math.cosh(a(i)),field.inject(a(i)).cosh)
        checkNear(field,scala.math.tanh(a(i)),field.inject(a(i)).tanh)
      }
      if(scala.math.abs(a(i))<=1.0) {
        checkNear(field,scala.math.asin(a(i)),field.inject(a(i)).asin)
        checkNear(field,scala.math.acos(a(i)),field.inject(a(i)).acos)
        checkNear(field,scala.math.atan(a(i)),field.inject(a(i)).atan)
      }
      for(j <- 0 to (n-1)) {
        checkNear(field,a(i)+a(j),field.inject(a(i)) + field.inject(a(j)))
        checkNear(field,a(i)-a(j),field.inject(a(i)) - field.inject(a(j)))
        checkNear(field,a(i)*a(j),field.inject(a(i)) * field.inject(a(j)))
        if(a(j)!=0.0) {
          checkNear(field,a(i)/a(j),field.inject(a(i)) / field.inject(a(j)))
        }
        checkNear(field,scala.math.max(a(i),a(j)),field.inject(a(i)).max(field.inject(a(j))))
        checkNear(field,scala.math.min(a(i),a(j)),field.inject(a(i)).min(field.inject(a(j))))
        assertEquals(a(i)==a(j),field.inject(a(i))==field.inject(a(j)))
        assertEquals(a(i)!=a(j),field.inject(a(i))!=field.inject(a(j)))
        assertEquals(a(i)>a(j),field.inject(a(i))>field.inject(a(j)))
        assertEquals(a(i)>=a(j),field.inject(a(i))>=field.inject(a(j)))
        assertEquals(a(i)<a(j),field.inject(a(i))<field.inject(a(j)))
        assertEquals(a(i)<=a(j),field.inject(a(i))<=field.inject(a(j)))
        if((a(i)>0.0)&&(scala.math.abs(a(j))<20)) {
          checkNear(field,scala.math.pow(a(i),a(j)),field.inject(a(i)).pow(a(j)))
        }
      }
    }
  }
  
  
  @Test def testMDouble:Unit = {
    testStdF(FDouble)
  }
  
  
  @Test def testDual:Unit = {
    testStdF(FDualNumber)
  }

  
  @Test def testCapture:Unit = {
    testStdF(FCapture)
  }

}
