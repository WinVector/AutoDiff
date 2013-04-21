package com.winvector.opt

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */

import com.winvector.definition.GFunction
import com.winvector.implementation.HelperFns._
import scala.util.Random

object CG {
  var debug = false

  // main state
  class CGState(fn: GFunction) {
    var curX: Array[Double] = null
    var curFx: Double = 0.0
    var gradPrev: Array[Double] = null
    var gradCur: Array[Double] = null
    var dirPrev: Array[Double] = null
    var dirCur: Array[Double] = null

    // different formulas for Beta
    def fletcherReeves() = dot(gradCur, gradCur) / dot(gradPrev, gradPrev)
    def polakRibiere() = dot(gradCur, sub(gradCur, gradPrev)) / dot(gradPrev, gradPrev)

    val betaFn: () => Double = fletcherReeves

    def init(x: Array[Double]): Array[Double] = {
      val (fx, gf) = fn.gradEval(x)
      curX = x
      curFx = fx
      gradCur = neg(gf)
      gradPrev = gradCur
      dirCur = gradCur
      dirPrev = dirCur
      dirCur
    }

    // needs gradCur and dirCur to be set
    def nextDirection(x: Array[Double]): Array[Double] = {
      gradPrev = gradCur
      dirPrev = dirCur
      val (fx, gf) = fn.gradEval(x)
      curX = x
      curFx = fx
      gradCur = neg(gf)
      val beta: Double = betaFn()
      if (debug) {
        print("grad: ")
        printD(gradCur)
        println("beta: " + beta)
      }
      dirCur = addD(gradCur, beta, dirPrev)
      dirCur
    }
  }

  def subnormalize(x: Array[Double]): Array[Double] = {
    val n = 1.0 + scala.math.sqrt(normSQ(x))
    val r = new Array[Double](x.length);
    for (i <- 0 to (x.length - 1)) {
      r(i) = x(i) / n
    }
    r
  }

  def defaultBounds(dim: Int, v: Double): (Array[Double], Array[Double]) = {
    assert(v > 0.0)
    val lbv = new Array[Double](dim) // TODO: take these in as arguments
    val ubv = new Array[Double](dim)
    for (i <- 0 to (dim - 1)) {
      lbv(i) = -v
      ubv(i) = v
    }
    (lbv, ubv)
  }
  
  def randV(lv:Array[Double],uv:Array[Double],rand:Random):Array[Double] = {
    val dim = lv.length
    val r = new Array[Double](dim)
    for(i <- 0 to (dim-1)) {
      val ui = rand.nextDouble()
      r(i) = lv(i) + ui*(uv(i)-lv(i))
    }
    r
  }

  def minimize(fn: GFunction, x0: Array[Double], lbv: Array[Double], ubv: Array[Double]): (Array[Double], Double) = { // return x,f(x)
    val cg = new CGState(fn)
    val tol = 1.0e-8
    var normTooSmall = false
    var stepTooSmall = false
    val maxSteps = 200
    var step = 0
    if (debug) {
      print("lbv: ")
      printD(lbv)
      print("ubv: ")
      printD(ubv)
    }
    while ((step <= 0) || ((step < maxSteps) && (!normTooSmall) && (!stepTooSmall))) {
      step += 1
      var di: Array[Double] = null
      if (step <= 1) {
        di = cg.init(x0)
      } else {
        di = cg.nextDirection(cg.curX)
        val dnormSq = normSQ(di)
        if ((step % 5 == 0) || (dnormSq < tol * tol)) {
          di = cg.gradCur;
        }
      }
      if (debug) {
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
      normTooSmall = norm <= 0
      if (!normTooSmall) {
        val diNorm = subnormalize(di)
        val (a, b) = LinMin.lineMinV(fn.apply, cg.curX, diNorm, lbv, ubv)
        val distSq = distSQ(cg.curX, a)
        stepTooSmall = (distSq <= tol * tol)&&(b>=cg.curFx)
        if(stepTooSmall) {
          if(debug) {
            println("step too small")
          }
        }
        cg.curX = a
        cg.curFx = b
        if (debug) {
          print("->x: ")
          printD(cg.curX)
          println("->f(x): " + cg.curFx)
        }
      } else {
        if (debug) {
          println("di norm too small")
        }
      }
    }
    if (debug) {
      println("done")
    }
    (cg.curX, cg.curFx)
  }

  def minimize(fn: GFunction, x0: Array[Double]): (Array[Double], Double) = { // return x,f(x)
    val (lbv, ubv) = defaultBounds(x0.length, 100000000.0)
    minimize(fn, x0, lbv, ubv)
  }
}

