package com.winvector.demo

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
import com.winvector.opt.CG

/**
 * demonstrate the conversion of a summable function into a general function with
 * gradient and then use of CG to minimize this function.
 */
object CGExample {

  def main(args: Array[String]): Unit = {

    // square distance of p from dk
    def sqDist(pdim: Int): SummableFN = {
      new SummableFN {
        def dim = { pdim }
        def apply[Y <: NumberBase[Y]](p: Array[Y], dk: Array[Y]): Y = {
          val field = p(0).field
          val dim = p.length
          var term = field.zero
          for (i <- 0 until dim) {
            val diff = p(i) - dk(i)
            term = term + diff * diff
          }
          term
        }
      }
    }

    val n: Int = 5
    val m: Int = 10
    val rand = new Random(532523)

    val dat: Array[Array[Double]] = new Array[Array[Double]](m)
    for (i <- 0 until m) {
      dat(i) = new Array[Double](n)
      for (j <- 0 until n) {
        dat(i)(j) = scala.math.abs(rand.nextDouble)
      }
    }
    val p0:Array[Double] = new Array[Double](n)
    // compute the mean the "hard way" by minimizing total square error
    val gRSum = RevDiffS.convertToGFunction(sqDist(dat(0).length),dat)
     
    val mv: Array[Double] = mean(dat)
    
    val (pF,fpF) = CG.minimize(gRSum,p0)
    print("pF:\t")
    printD(pF);
    print("mv:\t")
    printD(mv);
  }
}