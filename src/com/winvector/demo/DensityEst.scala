package com.winvector.demo

import java.util.Date

import com.winvector.definition.NumberBase
import com.winvector.definition.VectorFN
import com.winvector.implementation.FDouble
import com.winvector.implementation.HelperFns
import com.winvector.opt.CG
import com.winvector.implementation.FwdDiff
import com.winvector.reva.CaptureNumber
import com.winvector.reva.RevDiffQ

object DensityEst {
  def normalDensityLog[Y <: NumberBase[Y]](mu:Y,sigma:Y,x:Y):Y = {
    val two = mu.field.inject(2.0)
    val twopi = mu.field.inject(2.0*scala.math.Pi)
    val sigmasq = sigma.sq + mu.field.inject(0.01)
    -((x-mu).sq)/(two*sigmasq) - ((twopi*sigmasq).log/two)
  }
  
  def wtFn[Y <: NumberBase[Y]](x:Y):Y = {
    val z = x.sq
    x.field.inject(2.0)*z/(x.field.one + z) + x.field.one
  }

  // return log(sum(v[i].exp)) (shift to avoid overflow/underflow)
  def logSumExp[Y <: NumberBase[Y]](v:Array[Y]):Y = {
    val n = v.length     
    var max = v(0)
    for(i <- 1 until n) {
      max = max.max(v(i))
    }
    var massi = (v(0)-max).exp
    for(i <- 1 until n) {
      massi = massi + (v(i)-max).exp
    }
    massi.log + max
  }
  
  def densityFN(pts:Array[Double]):VectorFN = {
    new VectorFN {
      def dim = { 3*pts.length }
      def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
        val field = p(0).field
        var penalty = field.zero
        val two = field.inject(2.0)
        val twopi = two*field.inject(scala.math.Pi)
        val npt = pts.length
        val nkernel = p.length/3
        val wts:Array[Y] = field.array(nkernel)
        var totalWt = field.zero
        for(j <- 0 until nkernel) {
          wts(j) = wtFn(p(3*j))
          totalWt += wts(j)
        }
        for(j <- 0 until nkernel) {
          wts(j) = wts(j)/totalWt
        }
        val vi:Array[Y] = field.array(nkernel)
        for(i <- 0 until npt) {
          for(j <- 0 until nkernel) {
            val wt = wts(j)
            val muj = p(3*j + 1)
            val sigmaj = p(3*j + 2)
            val logpij = normalDensityLog(muj,sigmaj,field.inject(pts(i)))
            vi(j) = wt.log + logpij
          }
          val penaltyi = logSumExp(vi)
          penalty +=  penaltyi
        }
        -penalty
      }
    }
  }
  
  class DensityEst(val wt:Double, val mean:Double, val stddev:Double) {
    override def toString:String = { "(wt: " + wt + ", mean: " + mean + ", stddev: " + stddev + ")" }
  }
  
  def decodeWts(pF:Array[Double]):Array[DensityEst] = {
    val k = pF.length/3
    val wts:Array[Double] = new Array[Double](k)
    var totalWt:Double = 0.0
    for(j <- 0 until k) {
      wts(j) = wtFn(FDouble.inject(pF(3*j))).project
      totalWt += wts(j)
    }
    for(j <- 0 until k) {
      wts(j) = wts(j)/totalWt
    }
    val ret:Array[DensityEst] = new Array[DensityEst](k)
    for(j <- 0 until k) {
      ret(j) = new DensityEst(wts(j),pF(3*j+1),scala.math.abs(pF(3*j+2)))
    }
    ret
  }


  def solveDensity(pts:Array[Double],k:Int):Array[DensityEst] = {
    val x2 = HelperFns.copy(pts)
    scala.util.Sorting.quickSort(x2)
    val npts = pts.length
    val params = new Array[Double](3*k)
    for(i <- 0 until k) {
      params(3*i) = 1.0
      params(3*i+1) = x2(((i+1)*pts.length)/(k+1))
      params(3*i+2) = 1.0
    }
    val f = densityFN(pts)
    println("finitial: " + f(HelperFns.injectV(FDouble,params)).project)
    //val (pF,fpF) = CG.minimize(new RevDiffQ(f.apply),params)
    val (pF,fpF) = CG.minimize(new FwdDiff(f),params)
    println("ffinal: " + fpF)
    decodeWts(pF)
  }

  def main(args : Array[String]) : Unit = {
       val pts:Array[Double] = Array(63, 67, 67, 37, 41, 56, 62, 57, 63, 53, 57, 56, 56, 44, 52, 57, 48, 54, 48, 49, 64, 58, 58, 58, 60, 50, 58, 66, 43, 40, 69, 60, 64, 59, 44, 42, 43, 57, 55, 61, 65, 40, 71, 59, 61, 58, 51, 50, 65, 53, 41, 65, 44, 44, 60, 54, 50, 41, 54, 51, 51, 46, 58, 54, 54, 60, 60, 54, 59, 46, 65, 67, 62, 65, 44, 65, 60, 51, 48, 58, 45, 53, 39, 68, 52, 44, 47, 53, 53, 51, 66, 62, 62, 44, 63, 52, 59, 60, 52, 48, 45, 34, 57, 71, 49, 54, 59, 57, 61, 39, 61, 56, 52, 43, 62, 41, 58, 35, 63, 65, 48, 63, 51, 55, 65, 45, 56, 54, 44, 62, 54, 51, 29, 51, 43, 55, 70, 62, 35, 51, 59, 59, 52, 64, 58, 47, 57, 41, 45, 60, 52, 42, 67, 55, 64, 70, 51, 58, 60, 68, 46, 77, 54, 58, 48, 57, 52, 54, 35, 45, 70, 53, 59, 62, 64, 57, 52, 56, 43, 53, 48, 56, 42, 59, 60, 63, 42, 66, 54, 69, 50, 51, 43, 62, 68, 67, 69, 45, 50, 59, 50, 64, 57, 64, 43, 45, 58, 50, 55, 62, 37, 38, 41, 66, 52, 56, 46, 46, 64, 59, 41, 54, 39, 53, 63, 34, 47, 67, 54, 66, 52, 55, 49, 74, 54, 54, 56, 46, 49, 42, 41, 41, 49, 61, 60, 67, 58, 47, 52, 62, 57, 58, 64, 51, 43, 42, 67, 76, 70, 57, 44, 58, 60, 44, 61, 42, 52, 59, 40, 42, 61, 66, 46, 71, 59, 64, 66, 39, 57, 58, 57, 47, 55, 35, 61, 58, 58, 58, 56, 56, 67, 55, 44, 63, 63, 41, 59, 57, 45, 68, 57, 57, 38) // cleveland heart data
    HelperFns.printD(pts)
    for(k <- 1 to 5) {
      println("start: " + new Date())
      val soln = solveDensity(pts,k)
      println("done: " + new Date())
      for(sj <- soln) {
        println(sj)
      }
      var j:Int = 0
      for(sj <- soln) {
        println("f" + j + " <- function(x) { " + sj.wt + "*dnorm(x,mean=" + sj.mean + ",sd=" + sj.stddev + ")}")
        j += 1
      }
    }
  }
}
