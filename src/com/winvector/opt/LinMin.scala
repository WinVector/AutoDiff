package com.winvector.opt

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */

import com.winvector.implementation.HelperFns

object LinMin {
  val debug = false
  
  class Bracket(f:Double=>Double,step:Double) {
    var m = 0.0
    var u = m + step
    var l = m - step
    var fm = f(m)
    var fu = f(u)
    var fl = f(l)
    
    // probe for an upper bound on right and shift bracket to the right
    // fu<=fm, p>u
    def insertU(p:Double) = {
      val fp = f(p)
      if(fl<=fm) {
        l = m
        fl = fm
      }
      m = u
      fm = fu
      u = p
      fu = fp
    }
    
    // probe for an upper bound on left and shift bracket to the left
    // fl<=fm, p<l
    def insertL(p:Double) = {
      val fp = f(p)
      if(fu<=fm) {
        u = m
        fu = fm
      }
      m = l
      fm = fl
      l = p
      fl = fp
    }
    
    // maintain bracket
    // starting invariant
    // l<m<u f(m)<=f(l), f(m)<=f(u)
    // l<p<u, p!=m
    // ending invariant the same with smaller bracket
    def insertMid(p:Double):Boolean = {
      val fp = f(p)
      var improvedMin:Boolean = false
      if(p>m) {
        if(fp<fm) {
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
        if(fp<fm) {
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
      improvedMin
    }
    
    override def toString = "(" + l + "(" + fl + ")" + "\t" + m + "(" + fm + ")" + "\t" + u + "(" + fu + ")"
  }
  
  def parabolaMin(x1:Double, fx1:Double, x2:Double, fx2:Double, x3:Double, fx3:Double) = {
    val num = fx1*(x3-x2)*(x2+x3) + fx2*(x1-x3)*(x1+x3) + fx3*(x2-x1)*(x1+x2)
    val den = 2*(fx1*(x3-x2) + fx2*(x1-x3) + fx3*(x2-x1))
    num/den
  }
  
  def linMin(f:Double=>Double,maxEvals:Int):(Double,Double) = { // return: x, f(x)
    var step:Double = 5.0
    val bracket = new Bracket(f,step)
    var evals:Int = 3
    // first: try to bracket a minimum
    while((bracket.fu<=bracket.fm)&&(evals<maxEvals/3)) {
      bracket.insertU(bracket.u+step)
      evals += 1
      step = (step+1)*2
    }
    while((bracket.fl<=bracket.fm)&&(evals<(2*maxEvals)/3)) {
      bracket.insertL(bracket.l-step)
      evals += 1
      step = (step+1)*2
    }
    if((bracket.fm<bracket.fl)&&(bracket.fm<bracket.fu)) {
      // now have l<m<u f(m) < min(f(l),f(u))
      if(debug) {
        println("\tlinmin initial bracket(" + evals +"): " + bracket) 
      }
      val c = 0.5*(scala.math.sqrt(5.0)-1) // about 0.618, the Fibonacci ratio
      val tol = 1.0e-6
      var phaseNum:Int = 0
      var lastWasParabola:Boolean = false
      while(((bracket.u - bracket.l)>tol)&&(evals<maxEvals)) {
        var parabolaStepTaken = false
        if((phaseNum%5)==0) {
          lastWasParabola = true
          // try a parabola step
          val p = parabolaMin(bracket.l,bracket.fl,bracket.m,bracket.fm,bracket.u,bracket.fu)
          if((p>bracket.l)&&(p<bracket.u)) {
            parabolaStepTaken = true
            evals += 1
            val improvedMin = bracket.insertMid(p)
            if(improvedMin) {  // try to end search
              if(p+0.4*tol<bracket.u) {
                evals += 1
                bracket.insertMid(p+0.4*tol)
              }
              if(p-0.4*tol>bracket.l) {
                evals += 1
                bracket.insertMid(p-0.4*tol)
              }
            }
          }
        }
        if(!parabolaStepTaken) {
          lastWasParabola = false
          evals += 1
          if((bracket.u - bracket.m)>=(bracket.m - bracket.l)) {
            val p = (1.0-c)*bracket.m + c*bracket.u
            bracket.insertMid(p)
          } else {
            val p = c*bracket.l + (1.0-c)*bracket.m
            bracket.insertMid(p)
          }
        }
        phaseNum += 1
      }
      // always try to polish with a final parabola step
      if((evals<maxEvals)&&(!lastWasParabola)) {
        val p = parabolaMin(bracket.l,bracket.fl,bracket.m,bracket.fm,bracket.u,bracket.fu)
        if((p>bracket.l)&&(p<bracket.u)) {
          evals += 1
          bracket.insertMid(p)
        }
      }
      if(debug) {
        println("\tlinmin final bracket(" + evals + "): " + bracket)
      }
    } else {
      // never established initial bracket
      if(bracket.fl<bracket.fm) {
        bracket.m = bracket.l
        bracket.fm = bracket.fl
      }
      if(bracket.fu<bracket.fm) {
        bracket.m = bracket.u
        bracket.fm = bracket.fu
      }
      if(debug) {
        println("\tfailed to establish bracket(" + evals + "): " + bracket)
      }
    }
    (bracket.m,bracket.fm)
  }

  // return xmin f(xmin)
  def lineMinV(f:Array[Double]=>Double,xm:Array[Double],di:Array[Double]):(Array[Double],Double) = {
    def pf(g:Double) = HelperFns.addD(xm,g,di)
    def fi(x:Double):Double = f(pf(x))
    val (xMin,fXMin) = LinMin.linMin(fi,300)
    (pf(xMin),fXMin)
  }

}
