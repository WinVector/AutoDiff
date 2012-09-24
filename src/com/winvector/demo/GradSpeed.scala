package com.winvector.demo

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */

import java.util.Random
import java.util.Date

import com.winvector.definition.NumberBase
import com.winvector.definition.VectorFN
import com.winvector.definition.SummableFN
import com.winvector.definition.GFunction
import com.winvector.implementation.MDouble
import com.winvector.implementation.FDouble
import com.winvector.implementation.HelperFns._
import com.winvector.implementation.FwdDiff
import com.winvector.implementation.NumDiff
import com.winvector.reva.RevDiffQ
import com.winvector.reva.RevDiffS


object GradSpeed {
  

  def smoothSQRTD(x:Double):Double = {
    val tau = 1.0e-4
    scala.math.sqrt(x+tau)
  }
  
  def lengthFD(dat:Array[Array[Double]])(p:Array[Double]):Double = {
    val dim = p.length
    val npoint = dat.length
    var total = 0.0
    for(k <- 0 until npoint) {
      var term = 0.0
      for(i <- 0 until dim) {
        val diff = p(i) - dat(k)(i)
        term = term + diff*diff 
      }
      total = total + smoothSQRTD(term)
    }
    total
  }

  def smoothSQRT[Y <: NumberBase[Y]](x:Y):Y = {
    val field = x.field
    val tau = field.inject(1.0e-4)
    (x+tau).pospow(0.5)
  }
  
  def lengthFN(dat:Array[Array[Double]]):VectorFN = {
    new VectorFN {
      def dim = { dat(0).length }
      def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
        val field = p(0).field
        val dim = p.length
        val npoint = dat.length
        var total = field.zero
        for(k <- 0 until npoint) {
          var term = field.zero
          for(i <- 0 until dim) {
            val diff = p(i) - field.inject(dat(k)(i))
            term = term + diff*diff 
          }
          total = total + smoothSQRT(term)
        }
        total
      }
    }
  }

  def partialLengthFN(pdim:Int):SummableFN = {
    new SummableFN {
      def dim = { pdim }
      def apply[Y<:NumberBase[Y]](p:Array[Y],dk:Array[Y]):Y = {
        val field = p(0).field
        val dim = p.length
        var term = field.zero
        for(i <- 0 until dim) {
          val diff = p(i) - dk(i)
          term = term + diff*diff 
        }
        smoothSQRT(term)
      }
    }
  }

  def main(args : Array[String]) : Unit = {
    val outterReps:Int = 10
    val rand = new Random(532523)
    val step:Int= 20
    val nMax:Double = 4000
    var nContinuous:Double = step
    var nPrev:Int = 0
    println("method" + "\t" + "n" + "\t" + "time in MS")
    while(nContinuous<=nMax) {
      val n:Int = step*((nContinuous/step).toInt)
      if(n-nPrev>=step) {
        // set up data
        val m:Int = n + n/2
        val dat:Array[Array[Double]] = new Array[Array[Double]](m)
        for(i <- 0 until m) {
          dat(i) = new Array[Double](n)
          for(j <- 0 until n) {
            dat(i)(j) = (j+1)*scala.math.abs(rand.nextDouble)
          }
        }
        val p0:Array[Double] = mean(dat)
        
        // set up data-driven generic fns
        val genericFx = lengthFN(dat)
        val gNum = new NumDiff(lengthFD(dat))
        val gRSum = RevDiffS.convertToGFunction(partialLengthFN(dat(0).length),dat)

        var gFwd:GFunction = null
        var gRev:GFunction = null

        if(n<=150) {
          gRev = new RevDiffQ(genericFx)
          gFwd = new FwdDiff(genericFx)
        }

        val innerReps = scala.math.max(1,300/n)
        gRSum.gradEval(p0) // cause a prep
        if(gRev!=null) {
          gRev.gradEval(p0)  // cause a prep
        }
        if(gFwd!=null) {
          gFwd.gradEval(p0)  // maybe warm up code path
        }
        gNum.gradEval(p0)  // maybe warm up code path
        for(ii <-0 until outterReps) {
          val tn0 = java.lang.System.currentTimeMillis
          for(i <- 0 until innerReps) {
            gNum.gradEval(p0)
          }
          val tn1 = java.lang.System.currentTimeMillis
          println("num" + "\t" + n + "\t" + (tn1-tn0)/(0.0+innerReps))
          if(gFwd!=null) {
            val tf0 = java.lang.System.currentTimeMillis
            for(i <- 0 until innerReps) {
              gFwd.gradEval(p0)
            }
            val tf1 = java.lang.System.currentTimeMillis
            println("fwd" + "\t" + n + "\t" + (tf1-tf0)/(0.0+innerReps))
          }
          if(gRev!=null) {
            val tr0 = java.lang.System.currentTimeMillis
            for(i <- 0 until innerReps) {
              gRev.gradEval(p0)
            }
            val tr1 = java.lang.System.currentTimeMillis
            println("rev" + "\t" + n + "\t" + (tr1-tr0)/(0.0+innerReps))
          }
          val ts0 = java.lang.System.currentTimeMillis
          for(i <- 0 until innerReps) {
            gRSum.gradEval(p0) 
          }
          val ts1 = java.lang.System.currentTimeMillis
          println("sum" + "\t" + n + "\t" + (ts1-ts0)/(0.0+innerReps))
        }
        // prepare for next pass through loop
         nPrev = n
      }
      // prepare for next pass through loop
      nContinuous *= 1.2
    }
  }
}

