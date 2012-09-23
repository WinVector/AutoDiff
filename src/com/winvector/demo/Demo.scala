package com.winvector.demo

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */


import com.winvector.definition.NumberBase
import com.winvector.definition.VectorFN
import com.winvector.implementation.MDouble
import com.winvector.implementation.FDouble
import com.winvector.implementation.HelperFns._
import com.winvector.opt.CG
import com.winvector.implementation.FwdDiff
import com.winvector.implementation.NumDiff
import com.winvector.reva.RevDiff
import com.winvector.reva.RevDiffQ


object Demo {
  def main(args : Array[String]) : Unit = {

    var dat:Array[Array[Double]] = Array(
      Array( 20, 0.0),
      Array( -1.0, 1.0),
      Array( -1.0, -1.0)
    )
    
    def fx(p:Array[Double]):Double = {
      val dim = p.length
      val npoint = dat.length
      var total = 0.0
      for(k <- 0 to (npoint-1)) {
        var term = 0.0
        for(i <- 0 to (dim-1)) {
          val diff = p(i) - dat(k)(i)
          term = term + diff*diff 
        }
        total = total + scala.math.sqrt(term)
      }
      total
    }
    
    
    def smoothSQRT[Y <: NumberBase[Y]](x:Y) = {
      val field = x.field
      val tau = field.inject(1.0e-4)
      (x+tau).pow(0.5)
    }
    

    val p0:Array[Double] = mean(dat)

    val genericFx = new VectorFN {
      def dim = { p0.length }
      def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
        val field = p(0).field
        val dim = p.length
        val npoint = dat.length
        var total = field.zero
        for(k <- 0 to (npoint-1)) {
          var term = field.zero
          for(i <- 0 to (dim-1)) {
            val diff = p(i) - field.inject(dat(k)(i))
            term = term + diff*diff 
          }
          total = total + smoothSQRT(term)
        }
        total
      }
    }
    
    

    println("points:")
    printM(dat)
    println()
    print("p0:\t")
    printD(p0);
    println("std eval: " + fx(p0))
    print("numeric gradient:\t")
    printPD((new NumDiff(fx)).gradEval(p0))
    print("ideal fwd gradient:\t")
    printPD((new FwdDiff(genericFx)).gradEval(p0))
    print("ideal rev gradient:\t")
    printPD((new RevDiff(genericFx)).gradEval(p0))
    println()
    val (pF,fpF) = CG.minimize(new FwdDiff(genericFx),p0)
    print("pF:\t")
    printD(pF);
    println()
    print("numeric gradient:\t")
    printPD((new NumDiff(fx)).gradEval(pF))
    print("ideal fwd gradient:\t")
    printPD((new FwdDiff(genericFx)).gradEval(pF))
    print("ideal rev gradient:\t")
    printPD((new RevDiff(genericFx)).gradEval(pF))
    println()
    println("done")
  }
}
