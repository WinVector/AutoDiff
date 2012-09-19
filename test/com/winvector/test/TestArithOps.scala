package com.winvector.test

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */


import junit.framework.TestCase

import com.winvector.definition.NumberBase
import com.winvector.definition.Field
import com.winvector.implementation.DualNumber
import com.winvector.implementation.FDouble
import com.winvector.implementation.FDualNumber
import com.winvector.implementation.MDouble
import com.winvector.reva.CaptureNumber
import com.winvector.reva.FCapture

import junit.framework.Assert.fail

/**
 * This set of tests performs operations on both MDoubles (our reference implementation) and the test types to see they get equivalent results
 * @author johnmount
 * 
 * TODO: get away from Java reflection for driving this test, as it does not work for methods in the base class
 * TODO: put back suppressed ops and add in new ops (sigmoid)
 *
 */
class TestArithOps extends TestCase {
  val unaryOps:Array[String] = Array("abs", "sqrt", "log", "exp", "unary_$minus", "sq",
		                     "sin", "cos", "tan", "sinh", "cosh",  "tanh", "asin", "acos", "atan")
  val binaryOps:Array[String] = Array(/* "min", "max",*/ "$plus", "$minus", "$times", "$div")
  val paramiterizedOps:Array[String] = Array("pow")
  val comparisonOps:Array[String] = Array(/*"$greater", "$greater$eq", "$eq$eq", "$bang$eq", "$less", "$less$eq"*/)
  
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
    val refClass = FDouble.zero.getClass
    val testClass = field.zero.getClass
    val refMethod = refClass.getMethod(op)
    val testMethod = testClass.getMethod(op)
    var refResult:Double = Double.NaN
    var testResult:Double = Double.NaN
    try {
      refResult = refMethod.invoke(FDouble.inject(a)).asInstanceOf[MDouble].project
    } catch {
      case ex:Exception =>
    }
    try {
      testResult = testMethod.invoke(av).asInstanceOf[Y].project
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
    val refClass = FDouble.zero.getClass
    val testClass = field.zero.getClass
    val refMethod = refClass.getMethod(op,refClass)
    val testMethod = testClass.getMethod(op,testClass)
    var refResult:Double = Double.NaN
    var testResult:Double = Double.NaN
    try {
      refResult = refMethod.invoke(FDouble.inject(a),FDouble.inject(b)).asInstanceOf[MDouble].project
    } catch {
      case ex:Exception =>
    }
    try {
      testResult = testMethod.invoke(av,bv).asInstanceOf[Y].project
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
    val refClass = FDouble.zero.getClass
    val testClass = field.zero.getClass
    val refMethod = refClass.getMethod(op,java.lang.Double.TYPE)
    val testMethod = testClass.getMethod(op,java.lang.Double.TYPE)
    var refResult:Double = Double.NaN
    var testResult:Double = Double.NaN
    try {
      refResult = refMethod.invoke(FDouble.inject(a),new java.lang.Double(b)).asInstanceOf[MDouble].project
    } catch {
      case ex:Exception =>
    }
    try {
      testResult = testMethod.invoke(av,new java.lang.Double(b)).asInstanceOf[Y].project
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
    val refClass = FDouble.zero.getClass
    val testClass = field.zero.getClass
    val refMethod = refClass.getMethod(op,refClass)
    val testMethod = testClass.getMethod(op,testClass)
    var refResult:Boolean = refMethod.invoke(FDouble.inject(a),FDouble.inject(b)).asInstanceOf[Boolean]
    var testResult:Boolean = testMethod.invoke(av,bv).asInstanceOf[Boolean]
    if(refResult!=testResult) {
      println(field.toString + "\top:\t" + op + "\t" + a + "\t" + b)
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
        for(op <- unaryOps) {
          testUnaryOp(a,av,field,op)
        }
        for(b <- base) {
          for(op <- paramiterizedOps) {
            testParamiterizedOp(a,b,av,field,op)
          }
          for(zb <- zeroEquiv) {
            val bv = field.inject(b) + zb
            for(op <- binaryOps) {
              testBinaryOp(a,b,av,bv,field,op)
            }
            for(op <- comparisonOps) {
              testComparionOp(a,b,av,bv,field,op)
            }
          }
        }
      }
    }
  }
  
  
  def testNull:Unit = {
    val zeroEquivs:Array[MDouble] = Array(FDouble.zero)
    testOps(FDouble, zeroEquivs) // not much of a test as test == ref
  }
  
  
  def testDualNumber:Unit = {
    val zeroEquivs:Array[DualNumber] = Array( 
      FDualNumber.zero, 
      FDualNumber.delta, 
      - FDualNumber.delta,
      FDualNumber.delta + FDualNumber.delta)
    testOps(FDualNumber,zeroEquivs)
  }
  
  
  def testCaptureNumber:Unit = {
    val zeroEquivs:Array[CaptureNumber] = Array(FCapture.zero)
    testOps(FCapture, zeroEquivs)
  }
}
