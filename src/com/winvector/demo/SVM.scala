package com.winvector.demo

import java.io.File
import com.winvector.definition.NumberBase
import com.winvector.definition.VectorFN
import com.winvector.implementation.HelperFns._
import com.winvector.opt.CG
import com.winvector.implementation.FDouble
import com.winvector.reva.RevDiffQ
import java.util.Arrays
import com.winvector.definition.SummableFN
import com.winvector.definition.Field

/**
 * page 215 hard margin SVM from Kernel Methods for Pattern Analysis
 */
object SVM {
  
  // from: Rex Kerr answer on http://stackoverflow.com/questions/4604237/how-to-write-to-a-file-in-scala
  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) = {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }
  
  
  // Gaussian radial kernel
  def gkernel(kernelWidth:Double):(Array[Double],Array[Double])=>Double = {
    new Function2[Array[Double],Array[Double],Double] {
      def apply(x1:Array[Double],x2:Array[Double]):Double = {
        val dim = x1.length
        var total = 0.0
        for(k <- 0 until dim) {
          val diff = x1(k) - x2(k)
          total = total + diff*diff 
        }
        scala.math.exp(-1.0*kernelWidth*total)
      }
    }
  }
  // cosine kernel
  val kernelC:(Array[Double],Array[Double])=>Double = new Function2[Array[Double],Array[Double],Double] {
    def apply(x1:Array[Double],x2:Array[Double]):Double = {
      val dim = x1.length
      val depth = 1.0
      var x1Dx1 = depth
      var x2Dx2 = depth
      var x1Dx2 = depth
      for(k <- 0 until dim) {
    	x1Dx1 += x1(k)*x1(k)
    	x2Dx2 += x2(k)*x2(k)
    	x1Dx2 += x1(k)*x2(k)
      }
      x1Dx2/scala.math.sqrt(x1Dx1*x2Dx2)
    }
  }

  // (1 + x.y)^2
  val kernelS:(Array[Double],Array[Double])=>Double = new Function2[Array[Double],Array[Double],Double] {
    def apply(x1:Array[Double],x2:Array[Double]):Double = {
      val dim = x1.length
      val depth = 2.0/(1.0+scala.math.sqrt(5.0))
      var x1Dx2 = depth
      for(k <- 0 until dim) {
    	x1Dx2 += x1(k)*x2(k)
      }
      x1Dx2*x1Dx2
    }
  }
  
  
  class SVNScore(kernel:(Array[Double],Array[Double])=>Double,datx:Array[Array[Double]],daty:Array[Double]) extends VectorFN {

    /**
    * enforce p dot y = 0 and p dot 1 = 1
    * 
    */
    def encode[Y <: NumberBase[Y]](p:Array[Y]):Array[Y] = {
      val field = p(0).field
      val npoint = daty.length
      val q = field.array(npoint)
      var sumPlus = field.zero
      var sumMinus = field.zero
      for(i <- 0 until npoint) {
        q(i) = p(i)*p(i) + field.inject(1.e-10)
        if(daty(i)>0) {
          sumPlus = sumPlus + q(i)
        } else if(daty(i)<0) {
          sumMinus = sumMinus + q(i)
        }
      }
      val adjPlus = field.one/(field.inject(2)*sumPlus)
      val adjMinus = field.one/(field.inject(2)*sumMinus)
      for(i <- 0 until npoint) {
        if(daty(i)>0) {
          q(i) = q(i)*adjPlus
        } else if(daty(i)<0) {
          q(i) = q(i)*adjMinus
        }
      }
      q
    }

    def dim = { daty.length }
    
    def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
      val field = p(0).field
      val npoint = datx.length
      val q = encode(p)
      var total = field.zero
      for(i <- 0 until npoint) {
        for(j <- 0 until npoint) {
          val kij = kernel(datx(i),datx(j))
          total = total + q(i)*q(j)*field.inject(kij*daty(i)*daty(j))
        }
      }
      total
    }
    
    def calcB(alpha:Array[Double],W:Double):Double = {
      val deltaSq = scala.math.abs(W)
      var maxI:Int = 0
      for( i <- 1 until datx.length) {
        if(alpha(i)>alpha(maxI)) {
          maxI = i
        }
      }
      var b = daty(maxI)*deltaSq
      for( j <- 0 until datx.length) {
        b = b - alpha(j)*daty(j)*kernel(datx(maxI),datx(j))
      }
      b
    }
    
    def model(alpha:Array[Double],b:Double,x:Array[Double]):Double = {
      var v = b
      for( j <- 0 until datx.length) {
        v = v + alpha(j)*daty(j)*kernel(x,datx(j))
      }
      v
    }
    
    def printTable(plotPoints:Int,concept:Array[Double]=>Double,alpha:Array[Double],b:Double,f:File):Unit = {
      var range:Double = 1
      for(i <- 0 until datx.length) {
        for(xi <- datx(i)) {
          val v = scala.math.ceil(scala.math.abs(xi))
          range = scala.math.max(range,v)
        }
      }
      printToFile(f)( p => {
        p.println("" + "x" +"\t" + "y" + "\t" + "v" + "\t" + "d" + "\t" + "cat")
        for(xi <- -plotPoints to plotPoints) {
          val x = (range*xi)/plotPoints
          for(yi <- -plotPoints to plotPoints) {
            val y = (range*yi)/plotPoints
            val v = model(alpha,b,Array[Double](x,y))
            var d = 0
            if(v>0) {
              d = 1
            } else if(v<0) {
              d = -1
            }
            val cat = concept(Array[Double](x,y))
            p.println("" + x +"\t" + y + "\t" + v + "\t" + d + "\t" + cat)
          }
        }
      })
    }
    
    def printExampleConcept(plotPoints:Int,concept:Array[Double]=>Double,j:Int,f:File):Unit = {
      val alpha = new Array[Double](datx.length)
      alpha(j) = 1
      var total = 0.0
      for(i <- 0 until datx.length) {
        val v = model(alpha,0,datx(i))
        total += v
      }
      val b = -total/(0.0+datx.length)
      printTable(plotPoints,concept,alpha,b,f)
    }
    
  }
  
  def main1D(args : Array[String]) : Unit = {
    def concept(x:Array[Double]):Double = {
      var ret = 0
      if(x(0)>=0) {
        ret = 1
      } else {
        ret = -1
      }
      ret
    }
    
    val kernel = gkernel(3)
    println("" + "points" + "\t" + "margin" + "\t" + "nSupport" + "\t" + "margin*points")

    for(trial <- 1 until 40) {
      val daty:Array[Double] = new Array[Double](2*trial)
      val datx:Array[Array[Double]] = new Array[Array[Double]](daty.length)
      
      for(i <- 0 until datx.length) {
        datx(i) = Array[Double](i/(datx.length-1.0)-0.5) // -0.5 to 0.5
        daty(i) = concept(datx(i))
      }
      
      val rand: scala.util.Random = new scala.util.Random(5325)
      val svnSystem = new SVNScore(kernel,datx,daty)
      val fn = new RevDiffQ(svnSystem)
      // plot null model
      val alpha0 = new Array[Double](datx.length)
      for(i <- 0 until datx.length) {
        alpha0(i) = 1
      }
      val p0:Array[Double] = new Array[Double](daty.length)
      //print("zero gradient:\t")
      //printPD(fn.gradEval(p0))
      // Need a random start as our encoding adds way too many symmetries at the origin
      for(i <- 0 until datx.length) {
        p0(i) = rand.nextGaussian()
      }
      //println()
      //print("initial gradient:\t")
      //printPD(fn.gradEval(p0))
      val (pF,fpF) = CG.minimize(fn,p0)
      //print("pF:\t")
      //printD(pF)
      val alpha = projectV(svnSystem.encode(injectV(FDouble,pF)))
      val b = svnSystem.calcB(alpha,fpF)
      //print("soln: ")
      //printD(alpha)
      //println("b: " + b)
      var nSupport = 0
      for(i <- 0 until pF.length) {
        if(pF(i)>1.0e-4) {
          nSupport += 1
        }
      }
      val margin = scala.math.sqrt(fn.apply(pF));
      println("" + daty.length + "\t" + margin + "\t" + nSupport + "\t" + (margin*daty.length))
      var sumY = 0.0
      var sumV = 0.0
      for(i <- 0 until datx.length) {
        sumV += alpha(i)
        sumY += daty(i)*alpha(i)
      }
      //println("check: sumV=" + sumV + ", sumY=" + sumY)
      //print("final gradient:\t")
      //printPD(fn.gradEval(pF))
    }
    println("done 1d")
  }

  
  
  
  
  def main(args : Array[String]) : Unit = {
    
    
    val rand: scala.util.Random = new scala.util.Random(5325)

    val plotPoints = 200
    val daty:Array[Double] = new Array[Double](20)
    val datx:Array[Array[Double]] = new Array[Array[Double]](daty.length)
    
    def concept(x:Array[Double]):Double = {
      val diff = x(0)*x(0) - x(1)
      var ret = 0
      if(scala.math.abs(diff)>0.1) {
        if(diff>0) {
          ret = 1
        } else {
          ret = -1
        }
      }
      ret
    }

    for(i <- 0 until datx.length) {
      var got = false
      while(!got) {
    	val x = rand.nextGaussian()
    	val y = rand.nextGaussian()
    	val v = Array[Double](x,y)
    	val cat = concept(v)
    	if(scala.math.abs(cat)>0.1) {
    	  datx(i) = v
    	  daty(i) = cat
    	  got = true
    	}
      }
    }
    
    
    

    
    
    
    
    
    
    
    def distSq(x1:Array[Double],x2:Array[Double]):Double = {
      val n = x1.length
      var dsq = 0.0
      for(i <- 0 until n) {
        val diff = x1(i)-x2(i)
        dsq += diff*diff
      }
      dsq
    }

    def printKNN(kernel:(Array[Double],Array[Double])=>Double,k:Int,f:File):Unit = {
      var range:Double = 1
      for(i <- 0 until datx.length) {
        for(xi <- datx(i)) {
          val v = scala.math.ceil(scala.math.abs(xi))
          range = scala.math.max(range,v)
        }
      }
      printToFile(f)( p => {
        p.println("" + "x" +"\t" + "y" + "\t" + "v" + "\t" + "d" + "\t" + "cat")
        val dists:Array[Double] = new Array[Double](datx.length)
        for(xi <- -plotPoints to plotPoints) {
          val x = (range*xi)/plotPoints
          for(yi <- -plotPoints to plotPoints) {
            val y = (range*yi)/plotPoints
            val pt = Array[Double](x,y)
            for(i <- 0 until datx.length) {
              val dsq = distSq(datx(i),pt)
              dists(i) = dsq
            }
            Arrays.sort(dists)
            val bound = dists(k-1)
            var sumY = 0.0
            var nY = 1.0e-6
            for(i <- 0 until datx.length) {
              val dsq = distSq(datx(i),pt)
              if(dsq<=bound) {
                var wt = 1.0
                if(null!=kernel) {
                  wt = kernel(pt,datx(i))
                }
                sumY += daty(i)*wt
                nY += wt
              }
            }
            val v = sumY/nY
            var d = 0
            if(v>0) {
              d = 1
            } else if(v<0) {
              d = -1
            }
            val cat = concept(Array[Double](x,y))
            p.println("" + x +"\t" + y + "\t" + v + "\t" + d + "\t" + cat)
          }
        }
      })
    }
    
    
    
    
    
    
    val pWidth = new VectorFN {
      // return prob x in class given parameters
      def px[Y<:NumberBase[Y]](parameterArg:Array[Y],x:Array[Double]):Y = {
        val field = parameterArg(0).field
        var num = field.zero
        var den = field.zero
        val m = datx.length
        val dim = datx(0).length
        for( i <- 0 until m) {
          if(daty(i)!=0) {
            var dxi = 0.0
            for( j <- 0 until dim) {
              val diff = x(j) - datx(i)(j)
              dxi = dxi + diff*diff
            }
            val vi = (-parameterArg(i)*parameterArg(i)*field.inject(dxi)).exp
            den = den + vi
            if(daty(i)>0) {
              num = num + vi
            }
          }
        }
        num/den
      }
      
      def dim = {datx(0).length}
      // return loss over training given parameters (varyingArg is the point we are evaluating, last coordinate is ground truth)
      def apply[Y<:NumberBase[Y]](parameterArg:Array[Y]):Y = {
        val field = parameterArg(0).field
        val m = datx.length
        val dim = datx(0).length
        var loss = field.zero
        // loss term
        for(i <- 0 until m) {
          val p = px(parameterArg,datx(i))
          if(daty(i)>0) {
            loss = loss - p.log
          } else if(daty(i)<0) {
            loss = loss - (field.one-p).log
          }
        }
        loss = loss/field.inject(m)
        // regularization term
        var reg = field.zero
        for(i <- 0 until dim) {
          reg = reg + parameterArg(i).sq
        }
        reg = reg/field.inject(dim)
        loss + reg
      }
      
      def printTable(parameterArg:Array[Double],f:File):Unit = {
        var range:Double = 1
        for(i <- 0 until datx.length) {
          for(xi <- datx(i)) {
            val v = scala.math.ceil(scala.math.abs(xi))
            range = scala.math.max(range,v)
          }
        }
        val param = injectV(FDouble,parameterArg)
        printToFile(f)( p => {
          p.println("" + "x" +"\t" + "y" + "\t" + "v" + "\t" + "d" + "\t" + "cat")
          for(xi <- -plotPoints to plotPoints) {
            val x = (range*xi)/plotPoints
            for(yi <- -plotPoints to plotPoints) {
              val y = (range*yi)/plotPoints
              val v:Double = px(param,Array[Double](x,y)).project - 0.5
              var d = 0
              if(v>0) {
                d = 1
              } else if(v<0) {
                d = -1
              }
              val cat = concept(Array[Double](x,y))
              p.println("" + x +"\t" + y + "\t" + v + "\t" + d + "\t" + cat)
            }
          }
        })
      }
    }
    
    

    val gRSum = new RevDiffQ(pWidth)
    val w0 = new Array[Double](datx.length)
    print("zero gradient:\t")
    printPD(gRSum.gradEval(w0))
    for(i <- 0 until w0.length) {
      w0(i) = rand.nextGaussian()
    }
    print("initial gradient:\t")
    printPD(gRSum.gradEval(w0))
    val (wF,fwF) = CG.minimize(gRSum,w0)
    print("wF: ")
    printD(wF)
    print("final gradient:\t")
    printPD(gRSum.gradEval(wF))
    pWidth.printTable(wF,new File("BandWidthModel.tsv"))
    

    
    printKNN(null,1,new File("knn1.tsv"))
    printKNN(null,2,new File("knn2.tsv"))
    printKNN(null,3,new File("knn3.tsv"))

    val kernels = Map("GaussianKernel0.25" -> gkernel(0.25), "GaussianKernel3" -> gkernel(3), "GaussianKernel20" -> gkernel(20), 
                      "CosineKernel" -> kernelC, "SquareKernel"->kernelS)
    for((kernelName,kernel) <- kernels.toSeq) { 
      println()
      println("work with " + kernelName)
      printKNN(kernel,5,new File("knn5" + kernelName + ".tsv"))
      val svnSystem = new SVNScore(kernel,datx,daty)
      val fn = new RevDiffQ(svnSystem)
      // plot null model
      val alpha0 = new Array[Double](datx.length)
      for(i <- 0 until datx.length) {
        alpha0(i) = 1
      }
      svnSystem.printTable(plotPoints,concept,alpha0,0.0,new File(kernelName + "initialModel.tsv"))    
      val p0:Array[Double] = new Array[Double](daty.length)
      print("zero gradient:\t")
      printPD(fn.gradEval(p0))
      // Need a random start as our encoding adds way too many symmetries at the origin
      for(i <- 0 until datx.length) {
        p0(i) = rand.nextGaussian()
      }
      // println("points:")
      //printM(datx)
      //printD(daty)
      println()
      print("initial gradient:\t")
      printPD(fn.gradEval(p0))
      val (pF,fpF) = CG.minimize(fn,p0)
      print("pF:\t")
      printD(pF)
      val alpha = projectV(svnSystem.encode(injectV(FDouble,pF)))
      val b = svnSystem.calcB(alpha,fpF)
      printD(alpha)
      println("b: " + b)
      var sumY = 0.0
      var sumV = 0.0
      for(i <- 0 until datx.length) {
        sumV += alpha(i)
        sumY += daty(i)*alpha(i)
      }
      println("check: sumV=" + sumV + ", sumY=" + sumY)
      print("final gradient:\t")
      printPD(fn.gradEval(pF))
      println()
      svnSystem.printTable(plotPoints,concept,alpha,b,new File(kernelName + "finalModel.tsv"))
      println()
      svnSystem.printExampleConcept(plotPoints,concept,1,new File(kernelName + "exampleConcept.tsv"))
      printToFile(new File(kernelName + "trainingData.tsv"))( p => {
        p.println("x" + "\t" + "y" + "\t" + "class" + "\t" + "weight")
        for( i <- 0 until datx.length) {
          p.println("" + datx(i)(0) + "\t" + datx(i)(1) + "\t" + daty(i) + "\t" + alpha(i))
        }
      })
      println("done with " + kernelName)
      println()
    }
    println()
    println("done")
  }
}
