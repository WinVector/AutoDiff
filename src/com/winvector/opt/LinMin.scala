package com.winvector.opt

/**
 * Copyright 2010-2011 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */

import com.winvector.implementation.HelperFns

object LinMin {
  val debug = false

  class Bracket(f: Double => Double, lb: Double, ub:Double) {
    if(ub<=0) {
    	assert(ub>0)
     }
    assert(lb<0)
    val f0 = f(0)
    var m = 0.0
    var step0 = 0.2*scala.math.min(1.0,scala.math.min(ub,-lb))
    var u = m + step0
    var l = m - step0
    var fm = f0
    var fu = f(u)
    var fl = f(l)

    // probe for an upper bound on right and shift bracket to the right
    // fu<=fm, p>u
    def insertU(p: Double) = {
      assert(p >= u)
      if (p > u) {
        val fp = f(p)
        if (fu < fm) {
          m = u
          fm = fu
        }
        u = p
        fu = fp
      }
    }

    // probe for an upper bound on left and shift bracket to the left
    // fl<=fm, p<l
    def insertL(p: Double) = {
      assert(p <= l)
      if (p < l) {
        val fp = f(p)
        if (fl < fm) {
          m = l
          fm = fl
        }
        l = p
        fl = fp
      }
    }

    // maintain bracket
    // starting invariant
    // l<m<u f(m)<=f(l), f(m)<=f(u)
    // l<p<u, p!=m
    // ending invariant the same with smaller bracket
    def insertMid(p: Double): Boolean = {
      assert(l <= p)
      assert(p <= u)
      var improvedMin: Boolean = false
      if ((p > l) && (p < u) && (p != m)) {
        val fp = f(p)
        if (p > m) {
          if (fp < fm) {
            l = m
            fl = fm
            m = p
            fm = fp
            improvedMin = true
          } else {
            u = p
            fu = fp
          }
        } else {
          if (fp < fm) {
            u = m
            fu = fm
            m = p
            fm = fp
            improvedMin = true
          } else {
            l = p
            fl = fp
          }
        }
      }
      improvedMin
    }

    override def toString = "(" + l + "(" + fl + ")" + "\t" + m + "(" + fm + ")" + "\t" + u + "(" + fu + ")"
  }

  def parabolaMin(x1: Double, fx1: Double, x2: Double, fx2: Double, x3: Double, fx3: Double) = {
    val num = fx1 * (x3 - x2) * (x2 + x3) + fx2 * (x1 - x3) * (x1 + x3) + fx3 * (x2 - x1) * (x1 + x2)
    val den = 2 * (fx1 * (x3 - x2) + fx2 * (x1 - x3) + fx3 * (x2 - x1))
    num / den
  }

  def linMin(f: Double => Double, lb: Double, ub:Double, maxEvals: Int): (Double, Double) = { // return: x, f(x)
	assert(ub>=lb)
    val bracket = new Bracket(f, lb, ub)
    var evals: Int = 3
    var step = bracket.u - bracket.l
    // first: try to bracket a minimum
    while ((bracket.u<ub) && (bracket.fu <= bracket.fm) && (evals < maxEvals / 3)) {
      bracket.insertU(scala.math.min(ub,bracket.u + step))
      evals += 1
      step = (step + 1) * 2
    }
    while ((bracket.l>lb) && (bracket.fl <= bracket.fm) && (evals < (2 * maxEvals) / 3)) {
      bracket.insertL(scala.math.max(lb,bracket.l - step))
      evals += 1
      step = (step + 1) * 2
    }
    if ((bracket.fm < bracket.fl) && (bracket.fm < bracket.fu)) {
      // now have l<m<u f(m) < min(f(l),f(u))
      if (debug) {
        println("\tlinmin initial bracket(" + evals + "): " + bracket)
      }
      val c = 0.5 * (scala.math.sqrt(5.0) - 1) // about 0.618, the Fibonacci ratio
      val tol = 1.0e-12
      while (((bracket.u - bracket.l) > tol) && (evals < maxEvals)) {
        evals += 1
        if ((bracket.u - bracket.m) >= (bracket.m - bracket.l)) {
          val p = (1.0 - c) * bracket.m + c * bracket.u
          bracket.insertMid(p)
        } else {
          val p = c * bracket.l + (1.0 - c) * bracket.m
          bracket.insertMid(p)
        }
      }
      // try to polish with a final parabola step
      val p = parabolaMin(bracket.l, bracket.fl, bracket.m, bracket.fm, bracket.u, bracket.fu)
      if ((p > bracket.l) && (p < bracket.u) && (p != bracket.m)) {
        evals += 1
        bracket.insertMid(p)
      }
      if (debug) {
        println("\tlinmin final bracket(" + evals + "): " + bracket)
      }
    } else {
      // never established initial bracket, return best point seen
      if (debug) {
        println("\tfailed to establish bracket(" + evals + "): " + bracket)
      }
      if (bracket.fl < bracket.fm) {
        bracket.m = bracket.l
        bracket.fm = bracket.fl
      }
      if (bracket.fu < bracket.fm) {
        bracket.m = bracket.u
        bracket.fm = bracket.fu
      }
    }
    (bracket.m, bracket.fm)
  }
  

  def addD(a:Array[Double],wb:Double,b:Array[Double], lbv:Array[Double], ubv:Array[Double]) = {
    val dim = a.length
    val r = new Array[Double](dim)
    for(i <- 0 to (dim-1)) {
      r(i) = scala.math.max(lbv(i),scala.math.min(ubv(i),a(i) + wb*b(i)))
    }
    r
  }

  // return xmin f(xmin)
  def lineMinV(f: Array[Double] => Double, xm: Array[Double], di: Array[Double], lbv:Array[Double], ubv:Array[Double]): (Array[Double], Double) = {
    def pf(g: Double) = addD(xm, g, di, lbv, ubv)
    val dim = xm.length
    var lb = Double.NegativeInfinity
    var ub = Double.PositiveInfinity
    for(i <- 0 to (dim-1)) {
      assert(xm(i)<=ubv(i))
      assert(xm(i)>=lbv(i))
      val div = di(i)
      if(div!=0.0) {
        val a = (ubv(i)-xm(i))/div
        val b = (lbv(i)-xm(i))/div
        val l = scala.math.min(a,b)
        val r = scala.math.max(a,b)
        //println("interval:\t" + l + "\t" + r)
        ub = scala.math.max(ub,r)  // this is a false union instead of intersection interval
        lb = scala.math.min(lb,l)  // this is a false union instead of intersection interval
      }
    }
    def fi(x: Double): Double = f(pf(x))
    val (xMin, fXMin) = LinMin.linMin(fi,lb,ub,300)
    val xNew = pf(xMin)
    for(i <- 0 to (dim-1)) {
      assert(xNew(i)<=ubv(i))
      assert(xNew(i)>=lbv(i))
    }
    (xNew, fXMin)
  }

}
