package com.winvector.opt

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */


import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
import org.junit.Test

import com.winvector.definition.NumberBase
import com.winvector.definition.VectorFN
import com.winvector.implementation.FwdDiff



class TestLinMin extends JUnitSuite {
  
  
  @Test def testParabola:Unit = {
    val center:Double = 7.2
    def f(x:Double):Double = { (x-center)*(x-center) + 3.3 }
    val b = LinMin.parabolaMin(1.0,f(1.0),2.0,f(2.0),3.0,f(3.0))
    assertTrue(scala.math.abs(center-b)<1.0e-5)
  }
  
  
  @Test def testLinMin:Unit = {
    val center:Double = 7.2
    def f(x:Double):Double = { (x-center)*(x-center) + 3.3 }
    val (xmin,fxmin) = LinMin.linMin(f,-1000.0,1000.0,100)
    assertTrue(scala.math.abs(center-xmin)<1.0e-5)
  }
  
  
  @Test def testLinMinGeneric:Unit = {
    val center:Double = 7.2
    
    val genericFx = new VectorFN {
      def dim = 1
      def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
        val x = p(0)
        val field = x.field
        val c = field.inject(center)
        (x-c)*(x-c) + field.inject(3.3)
      }
    }
    
    val x:Array[Double] = Array(0.0)
    val d:Array[Double] = Array(1.0)
    val lb:Array[Double] = Array(-1000.0)
    val ub:Array[Double] = Array(1000.0)
    
    val (xmin,fxmin) = LinMin.lineMinV((new FwdDiff(genericFx)).apply,x,d,lb,ub)
    assertTrue(scala.math.abs(center-xmin(0))<1.0e-5)
  }
  
}
