package com.winvector.reva

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */

import com.winvector.definition.GFunction
import com.winvector.definition.VectorFN
import com.winvector.implementation.FDouble
import com.winvector.implementation.MDouble
import com.winvector.implementation.HelperFns._

// uses a single captured straight-line program to re-do computations (instead of re-capturing it for different inputs)
// faster, but depends on no if() statements changing in original execution
class RevDiffQ(fn:VectorFN) extends GFunction {
  private var rd:ReverseDiff = null

  override def toString:String = {
    if(rd==null) {
      "RevDiffQ(not prepped)"
    } else {
      "RevDiffQ\n" + rd.toString
    }
  }

  private def prep(x:Array[Double]):Unit = {
    // define variables
    val dim = x.length
    val xc = new Array[CaptureNumber](dim)
    for(i <- 0 until dim) {
      xc(i) = FCapture.variable(x(i),i)
    }
    val rc = fn.apply(xc)
    rd = new ReverseDiff(rc)
  }
  
  def apply(x: Array[Double]): Double = FDouble.project(fn.apply[MDouble](injectV(FDouble,x)))

  def gradEval(x:Array[Double]):(Double,Array[Double]) = {
    if(rd==null) {
      prep(x)
    }
    rd.gradient(x) 
  }
}
