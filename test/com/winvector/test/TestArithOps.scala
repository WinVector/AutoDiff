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
import com.winvector.definition.Field
import com.winvector.implementation.DualNumber
import com.winvector.implementation.FDouble
import com.winvector.implementation.FDualNumber
import com.winvector.implementation.MDouble
import com.winvector.reva.CaptureNumber
import com.winvector.reva.FCapture
import com.winvector.implementation.StandardOperations



/**
 * This set of tests performs operations on both MDoubles (our reference implementation) and the test types to see they get equivalent results
 * @author johnmount
 * 
 * TODO: (check if this obsoletes TestDiff or at least if TestDiff should move up to using StandardOperations)
 *
 */
class TestArithOps extends JUnitSuite {
  
  def equiv(a:Double,b:Double):Boolean = {
    val aWacky = (a.isNaN)||(a.isInfinite)
    val bWacky = (b.isNaN)||(b.isInfinite)
    if(aWacky || bWacky) {
      aWacky == bWacky
    } else {
      scala.math.abs(a-b)<1.0e-5
    }
  }

  def testUnaryOp[Y<:NumberBase[Y]](a:Double,av:Y,field:Field[Y],op:String):Unit = {
    val method = StandardOperations.fns(op)
    require(method.dim==1)
    var refResult:Double = Double.NaN
    var testResult:Double = Double.NaN
    try {
      val ar = FDouble.array(1)
      ar(0) = FDouble.inject(a)
      refResult = method.apply(ar).project
    } catch {
      case ex:Exception =>
    }
    try {
      val ar = field.array(1)
      ar(0) = av
      testResult = method.apply(ar).project
    } catch {
      case ex:Exception =>
    }
    if(!equiv(refResult,testResult)) {
      println(field.toString + "\top:\t" + op + "\t" + a)
      println("\treference:\t" + "\t" + refResult)
      println("\ttest:\t" + "\t" + testResult)
      fail
    }
  }
  
 
  def testBinaryOp[Y<:NumberBase[Y]](a:Double,b:Double,av:Y,bv:Y,field:Field[Y],op:String):Unit = {
    val method = StandardOperations.fns(op)
    require(method.dim==2)
    var refResult:Double = Double.NaN
    var testResult:Double = Double.NaN
    try {
      val ar = FDouble.array(2)
      ar(0) = FDouble.inject(a)
      ar(1) = FDouble.inject(b)
      refResult = method.apply(ar).project
    } catch {
      case ex:Exception =>
    }
    try {
      val ar = field.array(2)
      ar(0) = av
      ar(1) = bv
      testResult = method.apply(ar).project
    } catch {
      case ex:Exception =>
    }
    if(!equiv(refResult,testResult)) {
      println(field.toString + "\top:\t" + op + "\t" + a + "\t" + b)
      println("\treference:\t" + "\t" + refResult)
      println("\ttest:\t" + "\t" + testResult)
      fail
    }
  }
  
  def testParamiterizedOp[Y<:NumberBase[Y]](a:Double,b:Double,av:Y,field:Field[Y],op:String):Unit = {
    val method = StandardOperations.paramOps(op)
    var refResult:Double = Double.NaN
    var testResult:Double = Double.NaN
    try {
      refResult = method.apply(FDouble.inject(a),b).project 
    } catch {
      case ex:Exception =>
    }
    try {
      testResult = method.apply(av,b).project
    } catch {
      case ex:Exception =>
    }
    if(!equiv(refResult,testResult)) {
      println(field.toString + "\top:\t" + op + "\t" + a + "\t" + b)
      println("\treference:\t" + "\t" + refResult)
      println("\ttest:\t" + "\t" + testResult)
      fail
    }
  }
  
  
  def testComparionOp[Y<:NumberBase[Y]](a:Double,b:Double,av:Y,bv:Y,field:Field[Y],op:String):Unit = {
    val method = StandardOperations.tests(op)
    require(method.dim==2)
    var refResult:Boolean = false
    var testResult:Boolean = false
    try {
      val ar = FDouble.array(2)
      ar(0) = FDouble.inject(a)
      ar(1) = FDouble.inject(b)
      refResult = method.apply(ar)
    } catch {
      case ex:Exception =>
    }
    try {
      val ar = field.array(2)
      ar(0) = av
      ar(1) = bv
      testResult = method.apply(ar)
    } catch {
      case ex:Exception =>
    }
    if(refResult!=testResult) {
      println(field.toString + "\top:\t" + op + "\t" + a + "\t" + b)
      println("\treference:\t" + "\t" + refResult)
      println("\ttest:\t" + "\t" + testResult)
      fail
    }
  }
  
  def testTestOp[Y<:NumberBase[Y]](a:Double,av:Y,field:Field[Y],op:String):Unit = {
    val method = StandardOperations.tests(op)
    require(method.dim==1)
    var refResult:Boolean = false
    var testResult:Boolean = false
    try {
      val ar = FDouble.array(1)
      ar(0) = FDouble.inject(a)
      refResult = method.apply(ar)
    } catch {
      case ex:Exception =>
    }
    try {
      val ar = field.array(1)
      ar(0) = av
      testResult = method.apply(ar)
    } catch {
      case ex:Exception =>
    }
    if(refResult!=testResult) {
      println(field.toString + "\top:\t" + op + "\t" + a)
      println("\treference:\t" + "\t" + refResult)
      println("\ttest:\t" + "\t" + testResult)
      fail
    }
  }
  
  def testOps[Y<:NumberBase[Y]](field:Field[Y],zeroEquiv:Array[Y]):Unit = {
    val base:Array[Double] = Array(1.3,4.5,-2,0,6.0,0.0,1.0,-1.0,2,-1.3,-6,-4.5,10,20,100,-100,1.0e-3,-1.0e-3,0.1,-0.1,1e+3,-1e+3)
    for(a <- base ) {
      for(za <- zeroEquiv) {
        val av = field.inject(a) + za
        for((op,meth) <- StandardOperations.fns) {
          if(meth.dim==1) {
            testUnaryOp(a,av,field,op)
          }
        }
        for((op,meth) <- StandardOperations.tests) {
           if(meth.dim==1) {
              testTestOp(a,av,field,op)
           }
        }
        for(b <- base) {
          for((op,meth) <- StandardOperations.paramOps) {
            testParamiterizedOp(a,b,av,field,op)
          }
          for(zb <- zeroEquiv) {
            val bv = field.inject(b) + zb
            for((op,meth) <- StandardOperations.fns) {
               if(meth.dim==2) {
                  testBinaryOp(a,b,av,bv,field,op)
               }
            }
            for((op,meth) <- StandardOperations.tests) {
               if(meth.dim==2) {
                  testComparionOp(a,b,av,bv,field,op)
               }
            }
          }
        }
      }
    }
  }
  
  
  @Test def testNull:Unit = {
    val zeroEquivs:Array[MDouble] = Array(FDouble.zero)
    testOps(FDouble, zeroEquivs) // not much of a test as test == ref
  }
  
  
  @Test def testDualNumber:Unit = {
    val zeroEquivs:Array[DualNumber] = Array( 
      FDualNumber.zero, 
      FDualNumber.delta, 
      - FDualNumber.delta,
      FDualNumber.delta + FDualNumber.delta)
    testOps(FDualNumber,zeroEquivs)
  }
  
  
  @Test def testCaptureNumber:Unit = {
    val zeroEquivs:Array[CaptureNumber] = Array(FCapture.zero)
    testOps(FCapture, zeroEquivs)
  }
}
