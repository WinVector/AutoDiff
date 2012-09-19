package com.winvector.lin

import com.winvector.definition.NumberBase
import com.winvector.definition.Field
import scala.collection.mutable.ArrayBuffer

object Matrix {
  def apply[NUMBERTYPE <: NumberBase[NUMBERTYPE]](field:Field[NUMBERTYPE],v:Array[Array[Double]]):Matrix[NUMBERTYPE] = {
    val rows = v.length
    val columns = v(0).length
    val m = new Matrix[NUMBERTYPE](field,rows,columns)
    for(i <- 0 until rows) {
      for(j <- 0 until columns) {
        m.set(i,j,field.inject(v(i)(j)))
      }
    }
    m
  }

  def zero[NUMBERTYPE <: NumberBase[NUMBERTYPE]](field:Field[NUMBERTYPE],rows:Int,columns:Int):Matrix[NUMBERTYPE] = {
    new Matrix[NUMBERTYPE](field,rows,columns)
  }

  def identity[NUMBERTYPE <: NumberBase[NUMBERTYPE]](field:Field[NUMBERTYPE],n:Int):Matrix[NUMBERTYPE] = {
    val m = new Matrix[NUMBERTYPE](field,n,n)
    for(i <- 0 until n) {
      m.set(i,i,field.one)
    }
    m
  }
}

class Matrix[NUMBERTYPE <: NumberBase[NUMBERTYPE]](val field:Field[NUMBERTYPE], val rows:Int, val columns:Int) {
  require(rows>0);
  require(columns>0);
  val d = ArrayBuffer.fill[NUMBERTYPE](rows,columns)(field.zero)
  
  // get/set
  def get(i:Int,j:Int):NUMBERTYPE = {
	  d(i)(j)
  }
  
  def set(i:Int,j:Int,v:NUMBERTYPE):Unit = {
	  d(i)(j) = v
  }
  
  def copy():Matrix[NUMBERTYPE] = {
    val c = new Matrix[NUMBERTYPE](field,rows,columns)
    for(i <- 0 until rows) {
      for(j <- 0 until columns) {
        c.set(i,j,get(i,j))
      }
    }
    c
  }

  
   def * (that: Matrix[NUMBERTYPE]):Matrix[NUMBERTYPE] = {
     require(columns==that.rows)
     val p = new Matrix[NUMBERTYPE](field,rows,that.columns)
     for(i <- 0 until rows) {
       for(j <- 0 until that.columns) {
         var sum = field.zero
         for(k <- 0 until columns) {
           sum = sum + get(i,k)*that.get(k,j)
         }
         p.set(i,j,sum)
       }
     }
     p
   }
   
   def * (that: Array[NUMBERTYPE]):Array[NUMBERTYPE] = {
     require(columns==that.length)
     val p = field.array(rows)
     for(i <- 0 until rows) {
         var sum = field.zero
         for(k <- 0 until columns) {
           sum = sum + get(i,k)*that(k)
         }
         p(i) = sum
     }
     p
   }
   
   /**
    * swap rows a and b
    */
   private def rowswap(a:Int,b:Int):Unit = {
	  if(a!=b) {}
	  for(j <- 0 until columns) {
	    val oa = get(a,j)
	    val ob = get(b,j)
	    set(a,j,ob)
	    set(b,j,oa)
     }
   }

   private def rowscale(a:Int,s:NUMBERTYPE):Unit = {
     for(j <- 0 until columns) {
       val v = get(a,j)
       if(v.nonZero) {
    	  set(a,j,v*s)
       }
     }     
   }
   
   /**
    * row[b,] = row[b,] + row[a,]
    * a!=b
    */
   private def rowop(a:Int,b:Int,s:NUMBERTYPE):Unit = {
     require(a!=b)
     for(j <- 0 until columns) {
       set(b,j,get(b,j) + s*get(a,j))
     }
   }


   
  /**
   * this.rows==this.columns
   * b.rows==this.columns
   * return x s.t. this * x = b
   */
   def / (b:Matrix[NUMBERTYPE]):Matrix[NUMBERTYPE] = {
     require(rows==columns)
     require(columns==b.rows)
     val x = this.copy
     val a = b.copy
     // upper triangularize a with parallel operations on x
     for(j <- 0 until columns) {
       var bestI = j
       for(i <- j+1 until rows) {
         if(a.get(i,j).abs>a.get(bestI,j).abs) {
           bestI = i
         }
       }
       if(a.get(bestI,j).nonZero) {
         a.rowswap(j,bestI)
         x.rowswap(j,bestI)
         val us = field.one/a.get(j,j)
         a.rowscale(j,us)
         x.rowscale(j,us)
         for(i <- j+1 until rows) {
        	 val s = -a.get(i,j)
        	 a.rowop(j,i,s);
        	 x.rowop(j,i,s);
         }
       }
     }
     // complete diagonalization
     for(jj <- 0 until columns) {
       val j = (columns - 1) - jj
       if(a.get(j,j).nonZero) {
         for(i <- 0 until j) {
        	 val s = -a.get(i,j)
        	 a.rowop(j,i,s);
        	 x.rowop(j,i,s);
         }
       }
     }
     x
   }

  
  // print
  override def toString() = {
    val m = rows
    val n = columns
    var maxsz = 0
    for(i <- 0 until m) {
      for(j <- 0 until n) {
        val x = "" + get(i,j)
        val sz = x.length()
        if(sz>maxsz) {
          maxsz = sz
        }
      }
    }
    val b = new StringBuilder()
    b.append(field.toString + " matrix (" + m + "," + n + ")\n")
    for(i <- 0 until m) {
      b.append("[");
      for(j <- 0 until n) {
        b.append("\t")
        val x = "" + get(i,j)
        val sz = x.length()
        if(sz<maxsz) {
           b.append(" "*(maxsz-sz))
        }
        b.append(x)
      }
      b.append("\t]\n")
    }
    b.toString()
  }
  
  
  // build more matrices of the same type
  
  def inject(v:Array[Array[Double]]):Matrix[NUMBERTYPE] = {
    Matrix(field,v)
  }

  def zero(rows:Int,columns:Int):Matrix[NUMBERTYPE] = {
    Matrix.zero(field,rows,columns)
  }

  def identity(n:Int):Matrix[NUMBERTYPE] = {
    Matrix.identity(field,n)
  }
}