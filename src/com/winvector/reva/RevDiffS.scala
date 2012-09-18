package com.winvector.reva

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */

import com.winvector.definition.GFunction
import com.winvector.definition.VectorFN
import com.winvector.definition.SummableFN
import com.winvector.implementation.FDouble
import com.winvector.implementation.MDouble
import com.winvector.implementation.HelperFns._


object RevDiffS {
  def convertToGFunction(fn:SummableFN,varyingData:Iterable[Array[Double]]):RevDiffS[Array[Double]] = {
    new RevDiffS(fn,varyingData,(d:Array[Double])=>d)
  }

  def convertToGFunction[D](fn:SummableFN,varyingData:Iterable[D],conv:D=>Array[Double]):RevDiffS[D] = {
    new RevDiffS(fn,varyingData,conv)
  }
}

// uses a single captured straight-line program to re-do computations (instead of re-capturing it for different inputs)
// faster, but depends on no if() statements changing in original execution
// specialized for sums over data
// for the D=Array[Double] version supply (d:Array[Double])=>d as the third argument
class RevDiffS[D](fn:SummableFN,varyingData:Iterable[D],conv:D=>Array[Double]) extends GFunction {
  private var varyingDim:Int = -1
  private var parameterDim:Int = -1
  private var rd:ReverseDiff = null

  override def toString:String = {
    if(rd==null) {
      "RevDiffS(not prepped: " + fn.toString + ")"
    } else {
      "RevDiffS\n" + rd.toString
    }
  }

  
  def apply(p: Array[Double]):Double = {
     val field = FDouble
     var r = 0.0
     var varyingDim:Int = -1
     var tmp:Array[MDouble] = null
     val pM = injectV(field,p)
     for(row <- varyingData) {
        val x = conv(row)
        if(tmp==null) {
           varyingDim = x.length
           tmp = field.array(varyingDim)
        }
        for(j <- 0 until varyingDim) {
           tmp(j) = field.inject(x(j))
        }
        r += fn.apply(pM,tmp).project
     }
     r
  }

  private def prep(x:Array[Double],d0:Array[Double]):Unit = {
    // get dimensions
    varyingDim = d0.length
    parameterDim = x.length
    // define variables
    val dv = new Array[CaptureNumber](varyingDim)
    val pv = new Array[CaptureNumber](parameterDim)
    // varying componets must be encoded as variables- so their initial prep values don't get permanently encoded into
    // the derivation of derivatives
    for(j <- 0 until varyingDim) {
      dv(j) = FCapture.variable(d0(j),j)            // ignore these variables (make sure they have lower ids and externalids)
    }
    for(j <- 0 until parameterDim) {
      pv(j) = FCapture.variable(x(j),j+varyingDim)  // these are the variables we want
    }
    val rc = fn.apply(pv,dv)
    rd = new ReverseDiff(rc)
  }

  def gradEval(x:Array[Double]):(Double,Array[Double]) = {
    var tmp:Array[Double] = null
    var fx:Double = 0.0
    var gx:Array[Double] = null
    for(row <- varyingData) {
      val di = conv(row)
      if(rd==null) {
        prep(x,di)
      }
      if(tmp==null) {
        tmp = new Array[Double](varyingDim + parameterDim)
        for(j <- 0 until parameterDim) {
          tmp(j+varyingDim) = x(j)
        }
        gx = new Array[Double](parameterDim)
      }
      for(j <- 0 until varyingDim) {
         tmp(j) = di(j)
      }
      val (fxi,gxi) = rd.gradient(tmp,varyingDim)  // tell Reversediff not to compute first varyingDim elements of gradient
      fx += fxi
      for(j <- 0 until parameterDim) {
         gx(j) += gxi(j+varyingDim)
      }
    }
    (fx,gx)
  }
}
