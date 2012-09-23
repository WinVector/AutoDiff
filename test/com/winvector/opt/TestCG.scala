package com.winvector.opt

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */



import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test

import com.winvector.definition.NumberBase
import com.winvector.definition.VectorFN
import com.winvector.implementation.FwdDiff





class TestCG extends AssertionsForJUnit {

  
  @Test def testGC:Unit = {
    val center:Array[Double] = Array(2.2,-3,3.4,5.0)
    
    val genericFx = new VectorFN {
      def dim = center.length
      def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
        val dim = center.length
        val field = p(0).field
        var r = field.inject(3.3)
        for(i <- 0 to (dim-1)) {
          val di = p(i) - field.inject(center(i))
          r = r + di*di
        }
        r
      }
    }
    
    val x:Array[Double] = new Array(center.length)
    val (xmin,fxmin) = CG.minimize(new FwdDiff(genericFx),x)
    for(i <- 0 to (center.length-1)) {
      assertTrue(scala.math.abs(center(i)-xmin(i))<1.0e-5)        
    }     
  }
}
