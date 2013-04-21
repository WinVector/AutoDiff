package com.winvector.test

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
import com.winvector.implementation.HelperFns.mean
import com.winvector.implementation.FwdDiff
import com.winvector.opt.CG
import com.winvector.reva.RevDiff




class TestDemo extends JUnitSuite {
  
  
  @Test def testDemo:Unit = {
    val dat:Array[Array[Double]] = Array(
      Array( 20, 0.0),
      Array( -1.0, 1.0),
      Array( -1.0, -1.0)
    )

    def smoothSQRT[Y <: NumberBase[Y]](x:Y) = {
      val field = x.field
      val tau = field.inject(1.0e-4)
      (x+tau).pospow(0.5)
    }
    
    val genericFx = new VectorFN {
      def dim = 1
      def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
        val field = p(0).field
        val dim = p.length
        val npoint = dat.length
        var total = field.zero
        for(k <- 0 to (npoint-1)) {
          var term = field.zero
          for(i <- 0 to (dim-1)) {
            val diff = p(i) - field.inject(dat(k)(i))
            term = term + diff*diff 
          }
          total = total + smoothSQRT(term)
        }
        total
      }
    }
    
    
    val p0R:Array[Double] = mean(dat)
    val (lbvR,ubvR) = CG.defaultBounds(p0R.length,1000.0)
    val (pR,fpR) = CG.minimize(new RevDiff(genericFx),p0R,lbvR,ubvR)
    assertTrue(scala.math.abs(pR(0)-(-0.422))<1.0e-3)
    assertTrue(scala.math.abs(pR(1)-(0.0))<1.0e-3)

    val p0F:Array[Double] = mean(dat)
    val (lbvF,ubvF) = CG.defaultBounds(p0F.length,1000.0)
    val (pF,fpF) = CG.minimize(new FwdDiff(genericFx),p0F,lbvF,ubvF)
    assertTrue(scala.math.abs(pF(0)-(-0.422))<1.0e-3)
    assertTrue(scala.math.abs(pF(1)-(0.0))<1.0e-3)
  }
}
