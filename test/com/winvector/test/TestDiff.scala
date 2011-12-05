package com.winvector.test

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */


import java.util.Random

import junit.framework.TestCase

import com.winvector.definition.NumberBase
import com.winvector.definition.VectorFN
import com.winvector.demo.GradSpeed
import com.winvector.implementation.HelperFns.mean
import com.winvector.implementation.FwdDiff
import com.winvector.implementation.HelperFns
import com.winvector.implementation.MDiff
import com.winvector.implementation.MDouble
import com.winvector.reva.RevDiff
import com.winvector.reva.RevDiffQ
import com.winvector.reva.RevDiffS

import junit.framework.Assert.assertTrue



class TestDiff extends TestCase {
  
  // a is reference truth
  def equiv(a:Double,b:Double):Boolean = {
    val aWacky = (a.isNaN)||(a.isInfinite)
    val bWacky = (b.isNaN)||(b.isInfinite)
    if(aWacky || bWacky) {
      aWacky == bWacky
    } else {
      scala.math.abs(a-b)/scala.math.max(1.0,scala.math.abs(a))<1.0e-5
    }
  }

  // a is reference truth
  def equiv(a:Array[Double],b:Array[Double]):Boolean = {
    val n = a.length
    if(n!=b.length) {
      return false
    }
    for( i <- 0 until n) {
      if(!equiv(a(i),b(i))) {
        return false
      }
    }
    return true
  }

  def testGrad(f:VectorFN,x:Array[Double]):Unit = {
    val num = new MDiff(f)
    val nume = num.apply(x)
    val (reffx,refg) = num.gradEval(x)
    assertTrue(equiv(nume,reffx))
    val fwd = new FwdDiff(f)
    val fwde = fwd.apply(x)
    assertTrue(equiv(reffx,fwde))
    val (fwdfx,fwdg) = fwd.gradEval(x)
    assertTrue(equiv(reffx,fwdfx))
    assertTrue(equiv(refg,fwdg))
    val rev = new RevDiff(f)
    val reve = fwd.apply(x)
    assertTrue(equiv(reffx,reve))
    val (revfx,revg) = rev.gradEval(x)
    assertTrue(equiv(reffx,revfx))
    assertTrue(equiv(refg,revg))
    val revq = new RevDiffQ(f)
    // prep at wrong point to confirm re-calc works
    val x2 = HelperFns.copy(x)
    val dim = x.length
    for(i <- 0 until dim) {
    	x2(i) = x(i) + 1
    }
    revq.gradEval(x2)
    val revqe = revq.apply(x)
    assertTrue(equiv(reffx,revqe))
    val (revqfx,revqg) = revq.gradEval(x)
    assertTrue(equiv(reffx,revqfx))
    assertTrue(equiv(refg,revqg))
  }
  
  
  def testFPlus:Unit = {
    val genericFx = new VectorFN {
      def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
        p(0) + p(1)
      }
    }
    val x:Array[Double] = Array(15.2,2.2)
    testGrad(genericFx,x)
  }
  
  
  def testFMinus:Unit = {
    val genericFx = new VectorFN {
      def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
        p(0) - p(1)
      }
    }
    val x:Array[Double] = Array(15.2,2.2)
    testGrad(genericFx,x)
  }

  
  def testFNeg:Unit = {
    val genericFx = new VectorFN {
      def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
        -p(0)
      }
    }
    val x:Array[Double] = Array(15.2)
    testGrad(genericFx,x)
  }

  
  def testFTimes:Unit = {
    val genericFx = new VectorFN {
      def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
        p(0) * p(1)
      }
    }
    val x:Array[Double] = Array(15.2,2.2)
    testGrad(genericFx,x)
  }

  
  def testFDiv:Unit = {
    val genericFx = new VectorFN {
      def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
        p(0) / p(1)
      }
    }
    val x:Array[Double] = Array(15.2,2.2)
    testGrad(genericFx,x)
  }

  
  def testFExp:Unit = {
    val genericFx = new VectorFN {
      def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
        p(0).exp
      }
    }
    val x:Array[Double] = Array(5.2)
    testGrad(genericFx,x)
  }

  
  def testFLog:Unit = {
    val genericFx = new VectorFN {
      def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
        p(0).log
      }
    }
    val x:Array[Double] = Array(15.2)
    testGrad(genericFx,x)
  }

  
  def testFSq:Unit = {
    val genericFx = new VectorFN {
      def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
        p(0).sq
      }
    }
    val x:Array[Double] = Array(15.2)
    testGrad(genericFx,x)
  }

  
  def testFSqrt:Unit = {
    val genericFx = new VectorFN {
      def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
        p(0).sqrt
      }
    }
    val x:Array[Double] = Array(15.2)
    testGrad(genericFx,x)
  }

  
  def testFAbs:Unit = {
    val genericFx = new VectorFN {
      def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
        p(0).abs
      }
    }
    val x:Array[Double] = Array(15.2)
    testGrad(genericFx,x)
  }

  
  def testFMax:Unit = {
    val genericFx = new VectorFN {
      def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
        p(0).max(p(1))
      }
    }
    val x:Array[Double] = Array(15.2,2.2)
    testGrad(genericFx,x)
  }

  
  def testFMin:Unit = {
    val genericFx = new VectorFN {
      def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
        p(0).min(p(1))
      }
    }
    val x:Array[Double] = Array(15.2,2.2)
    testGrad(genericFx,x)
  }
  
  
  def testFSin:Unit = {
    val genericFx = new VectorFN {
      def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
        p(0).sin
      }
    }
    val x:Array[Double] = Array(5.2)
    testGrad(genericFx,x)
  }
  
  
  def testFCos:Unit = {
    val genericFx = new VectorFN {
      def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
        p(0).cos
      }
    }
    val x:Array[Double] = Array(5.2)
    testGrad(genericFx,x)
  }
  
  
  def testFTan:Unit = {
    val genericFx = new VectorFN {
      def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
        p(0).tan
      }
    }
    val x:Array[Double] = Array(5.2)
    testGrad(genericFx,x)
  }
  
  
  def testFASin:Unit = {
    val genericFx = new VectorFN {
      def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
        p(0).asin
      }
    }
    val x:Array[Double] = Array(5.2)
    testGrad(genericFx,x)
  }
  
  
  def testFACos:Unit = {
    val genericFx = new VectorFN {
      def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
        p(0).acos
      }
    }
    val x:Array[Double] = Array(5.2)
    testGrad(genericFx,x)
  }
  
  
  def testFAtan:Unit = {
    val genericFx = new VectorFN {
      def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
        p(0).atan
      }
    }
    val x:Array[Double] = Array(5.2)
    testGrad(genericFx,x)
  }
  
  
  def testFSinh:Unit = {
    val genericFx = new VectorFN {
      def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
        p(0).sinh
      }
    }
    val x:Array[Double] = Array(5.2)
    testGrad(genericFx,x)
  }
  
  
  def testFCosh:Unit = {
    val genericFx = new VectorFN {
      def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
        p(0).cosh
      }
    }
    val x:Array[Double] = Array(5.2)
    testGrad(genericFx,x)
  }
  
  
  def testFTanh:Unit = {
    val genericFx = new VectorFN {
      def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
        p(0).tanh
      }
    }
    val x:Array[Double] = Array(5.2)
    testGrad(genericFx,x)
  }

  
  def testRevSumDiff:Unit = {
    val rand = new Random(532523)
    val n = 20
    val m = n + n/2
    val dat:Array[Array[Double]] = new Array[Array[Double]](m)
    for(i <- 0 until m) {
      dat(i) = new Array[Double](n)
      for(j <- 0 until n) {
        dat(i)(j) = (j+1)*scala.math.abs(rand.nextDouble)
      }
    }
    val p0:Array[Double] = mean(dat)
    
    // set up data-driven generic fns
    val genericFx = GradSpeed.lengthFN(dat)
    val gFwd = new FwdDiff(genericFx)
    val gRev = new RevDiffQ(genericFx)
    val gRSum = RevDiffS.convertToGFunction(GradSpeed.partialLengthFN,dat)

    // check
    val fe = gFwd.apply(p0)
    val (fv,fg) = gFwd.gradEval(p0)
    assertTrue(equiv(fe,fv))
    val re = gRev.apply(p0)
    assertTrue(equiv(fe,re))
    val (rv,rg) = gRev.gradEval(p0)
    assertTrue(equiv(fv,rv))
    assertTrue(equiv(fg,rg))
    val se = gRSum.apply(p0)
    assertTrue(equiv(fe,se))
    val sf = gRSum.apply(p0)
    val (sv,sg) = gRSum.gradEval(p0)
    assertTrue(equiv(sf,sv))
    assertTrue(equiv(fv,sv))
    assertTrue(equiv(fg,sg))
  }

}
