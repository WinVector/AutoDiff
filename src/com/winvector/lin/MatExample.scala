package com.winvector.lin

import com.winvector.implementation.MDouble
import com.winvector.implementation.FDouble
import com.winvector.definition.NumberBase
import com.winvector.definition.VectorFN
import com.winvector.implementation.HelperFns._
import com.winvector.opt.CG
import com.winvector.implementation.FwdDiff
import com.winvector.implementation.NumDiff
import com.winvector.implementation.MDiff
import com.winvector.reva.RevDiff
import com.winvector.reva.RevDiffQ

import scala.util.Random

object MatExample {

  def main(args: Array[String]): Unit = {
    //val m = Matrix(FDouble,Array(Array(3, 2), Array(1, 1)))
    val field = FDouble
    val m = field.injectM(Array(Array(3, 2), Array(1, 1)))
    println(m.field)
    println("m:")
    println(m)
    val orig = field.injectA(Array(3,5))
    println("origv: " + orig(0) + " " + orig(1))
    val v = m*orig
    println("prodv: " + v(0) + " " + v(1))
    val inv = m.identity(m.rows)/m
    println("minv:")
    println(inv)
    println("m*minv:")
    println(m*inv)
    val vo = inv*v
    println("recovinv: " +vo(0) + " " + vo(1))
    val va = m.solve(v)
    println("recovsolve: " + va(0) + " " + va(1))
    
    val stride:Int = 3
    
    // implements the first Newton step of solving a logistic regression starting at zero
    // on data of dim-stride (including the first dimension set to 1 and weights of the form w*w + 1)
    val genericFx = new VectorFN {
      def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
        val field = p(0).field
        val oneFourth = field.inject(0.25)
        val oneHalf = field.inject(0.5)
        val logHalf = oneHalf.log
        val m = p.length
        val a = field.matrix(stride,stride)
        val b = field.array(stride)
        val epsilon = field.inject(1.0e-3)
        var nullP = field.zero
        var wTot = field.zero
        for(s <- 0 until m/stride) {
          val ws = p(stride*s).sq + field.one
          wTot = wTot + ws
          val ys = if((s&1)==0) field.one else field.zero 
          nullP = nullP + ws*logHalf
          for(i <- 0 until stride) {
            val xsi = if (i<=0) field.one else p(i+stride*s)
            b(i) = b(i) + ws*(ys-oneHalf)*xsi
            for(j <- 0 until stride) {
              val xsj = if (j<=0) field.one else p(j+stride*s)
              a.set(i,j,a.get(i,j)+ws*oneFourth*xsi*xsj)
            }
          }
        }
        val x = a.solve(b)
        var oneP = field.zero
        for(s <- 0 until m/stride) {
          val ws = p(stride*s).sq + field.one
          val ys = (s&1)==0
          var dot = field.zero
          for(i <- 0 until stride) {
            val xsi = if (i<=0) field.one else p(i+stride*s)
            dot = dot + x(i)*xsi
          }
          val prob = dot.sigmoid
          if(ys) {
            oneP = oneP + ws*prob.log
          } else {
            oneP = oneP + ws*((field.one-prob).log)
          }
        }
        nullP/oneP + epsilon*wTot // minimize this
      }
    }
    
    //val p0:Array[Double] = Array(1.0,2.0,3.0,1.0,-1.0,-1.0,1.0,0.0,2.5,1.0,1.0,1.0,1.0,-1.0,0.5)
    val rand = new Random(25253262L);
    val n:Int = stride*20
    
    for(rep <- 0 until 10) {
    val p0 = new Array[Double](n)
    print("p0:")
    for(i <- 0 until n) {
      p0(i) = rand.nextGaussian
      print("\t" + p0(i))
    }
    println()
    
    print("numeric gradient:\t")  
    printPD((new MDiff(genericFx)).gradEval(p0))
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
    printPD((new MDiff(genericFx)).gradEval(pF))
    print("ideal fwd gradient:\t")
    printPD((new FwdDiff(genericFx)).gradEval(pF))
    print("ideal rev gradient:\t")
    printPD((new RevDiff(genericFx)).gradEval(pF))
    println()
    }
    println("done")
  }

}