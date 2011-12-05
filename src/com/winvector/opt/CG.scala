package com.winvector.opt

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */

import com.winvector.definition.GFunction
import com.winvector.implementation.HelperFns._

object CG {
  val debug = false;

  // main state
  class CGState(fn:GFunction) {
    var curX:Array[Double] = null
    var curFx:Double = 0.0
    var gradPrev:Array[Double] = null
    var gradCur:Array[Double] = null
    var dirPrev:Array[Double] = null
    var dirCur:Array[Double] = null


    // different formulas for Beta
    def fletcherReeves() = dot(gradCur,gradCur)/dot(gradPrev,gradPrev)
    def polakRibiere() = dot(gradCur,sub(gradCur,gradPrev))/dot(gradPrev,gradPrev)

    val betaFn:()=>Double = fletcherReeves

    def init(x:Array[Double]):Array[Double] = {
      val (fx,gf) = fn.gradEval(x)
      curX = x
      curFx = fx
      gradCur = neg(gf)
      gradPrev = gradCur
      dirCur = gradCur
      dirPrev = dirCur
      dirCur
    }

    // needs gradCur and dirCur to be set
    def nextDirection(x:Array[Double]):Array[Double] = {
      gradPrev = gradCur
      dirPrev = dirCur
      val (fx,gf) = fn.gradEval(x)
      curX = x
      curFx = fx
      gradCur = neg(gf)
      val beta:Double = betaFn()
      if(debug) {
        print("grad: ")
        printD(gradCur)
        println("beta: " + beta)
      }
      dirCur = addD(gradCur,beta,dirPrev)
      dirCur
    }
  }
  

  def minimize(fn:GFunction,x0:Array[Double]):(Array[Double],Double) = { // return x,f(x)
    val cg = new CGState(fn)
    val tol = 1.0e-8
    var normTooSmall = false
    val maxSteps = 200
    var step = 0
    while((step<=0) || ((step<maxSteps)&&(!normTooSmall))) {
      step += 1
      var di:Array[Double] = null
      if(step<=1) {
        di = cg.init(x0)
      } else {
        di = cg.nextDirection(cg.curX)
        val dnormSq = normSQ(di)
        if(dnormSq<tol*tol) {
          di = cg.gradCur;
        }
      }
      if(debug) {
        println()
        println("----------------------------------------")
        println()
        println("step " + step)
        print("x: ")
        printD(cg.curX)
        println("fx: " + cg.curFx)
        print("g(f): ")
        printD(cg.gradCur)
        print("di: ")
        printD(di)
      }
      val norm = normSQ(di)
      normTooSmall = norm<=tol*tol
      if(norm>0.0) {
        val (a,b) = LinMin.lineMinV(fn.apply,cg.curX,di)
        cg.curX = a
        cg.curFx = b
        if(debug) {
          print("->x: ")
          printD(cg.curX)
          println("->f(x): " + cg.curFx)
        }
      }
      if(debug) {
        println()
      }
    }
    if(debug) {
      println("done")
    }
    (cg.curX,cg.curFx)
  }
}

