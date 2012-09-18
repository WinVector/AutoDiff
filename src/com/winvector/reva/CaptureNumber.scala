package com.winvector.reva

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */

import scala.collection.mutable.Stack

import com.winvector.definition.NumberBase
import com.winvector.definition.Field

// implementation class
object FCapture extends Field[CaptureNumber] {
  val idSource = new IDSource()
  private val z = new CaptureNumber(0.0)
  private val o = new CaptureNumber(1.0)
  def zero = z
  def one = o
  def inject(v:Double) = new CaptureNumber(v)             // constant
  def variable(v:Double,id:Int) = new CaptureNumber(v,id) // variable
  override def toString = "FCapture"
  def array(n:Int) = { 
    val a = new Array[CaptureNumber](n)
    for(i <- 0 until n) {
      a(i) = zero
    }
    a
  }
  def representationNorm(v:CaptureNumber):Double = { scala.math.abs(v.v) }
  
  // define basic ops
  class Op(val name:String,
           val function:(List[CaptureNumber],Map[Int,Int],Array[Double])=>Double,
           val grad:(CaptureNumber,Map[Int,Int],Array[Double])=>List[Double]
         ) {
    override def toString = name
  }
  
  def sq(x:Double):Double = { x*x }

  val plusOp = new Op( "+" ,
                      (i:List[CaptureNumber],p:Map[Int,Int],v:Array[Double]) => v(p(i(0).nodeId)) + v(p(i(1).nodeId)),
                      (i:CaptureNumber,p:Map[Int,Int],v:Array[Double]) => List(1.0,1.0)
                    )

  val minusOp = new Op( "-" , 
                       (i:List[CaptureNumber],p:Map[Int,Int],v:Array[Double]) => v(p(i(0).nodeId)) - v(p(i(1).nodeId)),
                       (i:CaptureNumber,p:Map[Int,Int],v:Array[Double]) => List(1.0,-1.0)
                     )
  
  val negOp = new Op( "neg" , 
                     (i:List[CaptureNumber],p:Map[Int,Int],v:Array[Double]) => -v(p(i(0).nodeId)),
                     (i:CaptureNumber,p:Map[Int,Int],v:Array[Double]) => List(-1.0)
                   )

  val timesOp = new Op( "*" , 
                       (i:List[CaptureNumber],p:Map[Int,Int],v:Array[Double]) => v(p(i(0).nodeId)) * v(p(i(1).nodeId)),
                       (i:CaptureNumber,p:Map[Int,Int],v:Array[Double]) => List(v(p(i.inputs(1).nodeId)),v(p(i.inputs(0).nodeId)))
                     )
  
  val divOp = new Op( "/" , 
                     (i:List[CaptureNumber],p:Map[Int,Int],v:Array[Double]) => v(p(i(0).nodeId)) / v(p(i(1).nodeId)),
                     (i:CaptureNumber,p:Map[Int,Int],v:Array[Double]) => List(1.0/v(p(i.inputs(1).nodeId)),-v(p(i.inputs(0).nodeId))/sq(v(p(i.inputs(1).nodeId))))
                   )
  
  val expOp = new Op( "exp" ,
                     (i:List[CaptureNumber],p:Map[Int,Int],v:Array[Double]) => scala.math.exp(v(p(i(0).nodeId))),
                     (i:CaptureNumber,p:Map[Int,Int],v:Array[Double]) => List(v(p(i.nodeId)))
                   )

  def powOp(exp:Double) = new Op( "pow(" + exp + ")" ,
                               (i:List[CaptureNumber],p:Map[Int,Int],v:Array[Double]) => scala.math.pow(v(p(i(0).nodeId)),exp),
                               (i:CaptureNumber,p:Map[Int,Int],v:Array[Double]) => List(exp*v(p(i.nodeId))/v(p(i.inputs(0).nodeId)))
                             )

  val logOp = new Op( "log" ,
                     (i:List[CaptureNumber],p:Map[Int,Int],v:Array[Double]) => scala.math.log(v(p(i(0).nodeId))),
                     (i:CaptureNumber,p:Map[Int,Int],v:Array[Double]) => List(1.0/(v(p(i.inputs(0).nodeId))))
                   )

  val absOp = new Op( "abs" ,
                     (i:List[CaptureNumber],p:Map[Int,Int],v:Array[Double]) => scala.math.abs(v(p(i(0).nodeId))),
                     (i:CaptureNumber,p:Map[Int,Int],v:Array[Double]) => {
                       var d:Double = 1.0
                       if(v(p(i.inputs(0).nodeId))<0) {
                         d = -1.0
                       }
                       List(d)
                     }
                   )

  val maxOp = new Op( "max" ,
                     (i:List[CaptureNumber],p:Map[Int,Int],v:Array[Double]) => scala.math.max(v(p(i(0).nodeId)),v(p(i(1).nodeId))),
                     (i:CaptureNumber,p:Map[Int,Int],v:Array[Double]) => {
                       if(v(p(i.inputs(0).nodeId))>=v(p(i.inputs(1).nodeId))) {
                         List(1.0,0.0)
                       } else {
                         List(0.0,1.0)
                       }
                     }
                   )

  val minOp = new Op( "min" ,
                     (i:List[CaptureNumber],p:Map[Int,Int],v:Array[Double]) => scala.math.min(v(p(i(0).nodeId)),v(p(i(1).nodeId))),
                     (i:CaptureNumber,p:Map[Int,Int],v:Array[Double]) => {
                       if(v(p(i.inputs(0).nodeId))<=v(p(i.inputs(1).nodeId))) {
                         List(1.0,0.0)
                       } else {
                         List(0.0,1.0)
                       }
                     }
                   )
  
  val sinOp = new Op( "sin" ,
                     (i:List[CaptureNumber],p:Map[Int,Int],v:Array[Double]) => scala.math.sin(v(p(i(0).nodeId))),
                     (i:CaptureNumber,p:Map[Int,Int],v:Array[Double]) => List(scala.math.cos(v(p(i.inputs(0).nodeId))))
                   )

  val cosOp = new Op( "cos" ,
                     (i:List[CaptureNumber],p:Map[Int,Int],v:Array[Double]) => scala.math.cos(v(p(i(0).nodeId))),
                     (i:CaptureNumber,p:Map[Int,Int],v:Array[Double]) => List(-scala.math.sin(v(p(i.inputs(0).nodeId))))
                   )

  val tanOp = new Op( "tan" ,
                     (i:List[CaptureNumber],p:Map[Int,Int],v:Array[Double]) => scala.math.tan(v(p(i(0).nodeId))),
                     (i:CaptureNumber,p:Map[Int,Int],v:Array[Double]) => List(1.0/sq(scala.math.cos(v(p(i.inputs(0).nodeId)))))
                   )

  val asinOp = new Op( "asin" ,
                      (i:List[CaptureNumber],p:Map[Int,Int],v:Array[Double]) => scala.math.asin(v(p(i(0).nodeId))),
                      (i:CaptureNumber,p:Map[Int,Int],v:Array[Double]) => List(1.0/scala.math.sqrt(1.0-sq(v(p(i.inputs(0).nodeId)))))
                    )

  val acosOp = new Op( "acos" ,
                      (i:List[CaptureNumber],p:Map[Int,Int],v:Array[Double]) => scala.math.acos(v(p(i(0).nodeId))),
                      (i:CaptureNumber,p:Map[Int,Int],v:Array[Double]) => List(-1.0/scala.math.sqrt(1.0-sq(v(p(i.inputs(0).nodeId)))))
                    )

  val atanOp = new Op( "atan" ,
                      (i:List[CaptureNumber],p:Map[Int,Int],v:Array[Double]) => scala.math.atan(v(p(i(0).nodeId))),
                      (i:CaptureNumber,p:Map[Int,Int],v:Array[Double]) => List(1.0/(1.0+sq(v(p(i.inputs(0).nodeId)))))
                    )

  val sinhOp = new Op( "sinh" ,
                      (i:List[CaptureNumber],p:Map[Int,Int],v:Array[Double]) => scala.math.sinh(v(p(i(0).nodeId))),
                      (i:CaptureNumber,p:Map[Int,Int],v:Array[Double]) => List(scala.math.cosh(v(p(i.inputs(0).nodeId))))
                    )

  val coshOp = new Op( "cosh" ,
                      (i:List[CaptureNumber],p:Map[Int,Int],v:Array[Double]) => scala.math.cosh(v(p(i(0).nodeId))),
                      (i:CaptureNumber,p:Map[Int,Int],v:Array[Double]) => List(scala.math.sinh(v(p(i.inputs(0).nodeId))))
                    )

  val tanhOp = new Op( "tanh" ,
                      (i:List[CaptureNumber],p:Map[Int,Int],v:Array[Double]) => scala.math.tanh(v(p(i(0).nodeId))),
                      (i:CaptureNumber,p:Map[Int,Int],v:Array[Double]) => List(1.0/sq(scala.math.cosh(v(p(i.inputs(0).nodeId)))))
                    )

  def constRes(inputs:List[CaptureNumber]):Boolean = {
     if(inputs!=null) {
        for(ni <- inputs) {
          if(!ni.isConst) {
             return false
          }
        }
     }
     return true
  }
}


class CaptureNumber private (private[reva] val v:Double, private[reva] val isConst:Boolean, private[reva] val op:FCapture.Op, private[reva] val inputs:List[CaptureNumber], private[reva] val nodeId:Int, private[reva] val externalId:Int) extends NumberBase[CaptureNumber] {
  private[reva] def this(v:Double) = this(v,true,null,null,FCapture.idSource.nextID,-1)                                        // constant
  private[reva] def this(v:Double,externalID:Int) = this(v,false,null,null,FCapture.idSource.nextID,externalID)                // variable
  private def this(v:Double,op:FCapture.Op,inputs:List[CaptureNumber]) = this(v,FCapture.constRes(inputs),op,inputs,FCapture.idSource.nextID,-1) // operation result
  // basic arithmetic
  def + (that: CaptureNumber) = new CaptureNumber(v + that.v,FCapture.plusOp,List(this,that))
  def - (that: CaptureNumber) = new CaptureNumber(v - that.v,FCapture.minusOp,List(this,that))
  def unary_- = new CaptureNumber(-v,FCapture.negOp,List(this)) 
  def * (that: CaptureNumber) = new CaptureNumber(v * that.v,FCapture.timesOp,List(this,that))
  def / (that: CaptureNumber) = {
    if(that.v==0.0) {
      throw new IllegalArgumentException("Tried to divide by zero")
    }
    new CaptureNumber(v / that.v,FCapture.divOp,List(this,that))
  }
  
  def project = v
  
  // more complicated
  def pow(p:Double) = {
    if(v<0.0) {
      throw new IllegalStateException("Tried to pow() negative number")
    }
    if(exp==0.0) {
      FCapture.one
    } else {
      new CaptureNumber(scala.math.pow(v,p),FCapture.powOp(p),List(this))
    }
  }
  
  def exp = new CaptureNumber(scala.math.exp(v),FCapture.expOp,List(this))
  
  def log = {
    if(v<=0.0) {
      throw new IllegalStateException("Tried to log() non-positive number: " + v)
    }
    new CaptureNumber(scala.math.log(v),FCapture.logOp,List(this))
  }
  
  def sqrt = { pow(0.5) }
  override def abs = new CaptureNumber(scala.math.abs(v),FCapture.absOp,List(this))
  override def max(o:CaptureNumber) = new CaptureNumber(scala.math.max(v,o.v),FCapture.maxOp,List(this,o))
  override def min(o:CaptureNumber) = new CaptureNumber(scala.math.min(v,o.v),FCapture.minOp,List(this,o))
  
  // more special fns
  def sin = new CaptureNumber(scala.math.sin(v),FCapture.sinOp,List(this))
  def cos = new CaptureNumber(scala.math.cos(v),FCapture.cosOp,List(this))
  def tan = new CaptureNumber(scala.math.tan(v),FCapture.tanOp,List(this))
  def asin = new CaptureNumber(scala.math.asin(v),FCapture.asinOp,List(this))
  def acos = new CaptureNumber(scala.math.acos(v),FCapture.acosOp,List(this))
  def atan = new CaptureNumber(scala.math.atan(v),FCapture.atanOp,List(this))
  def sinh = new CaptureNumber(scala.math.sinh(v),FCapture.sinhOp,List(this))
  def cosh = new CaptureNumber(scala.math.cosh(v),FCapture.coshOp,List(this))
  def tanh = new CaptureNumber(scala.math.tanh(v),FCapture.tanhOp,List(this))


  // utility
  def self = { this }
  def field:Field[CaptureNumber] = FCapture
  override def toString = "" + v
  
  def longString:String = {
    val str = new StringBuilder()
    str.append("nd" + nodeId)
    if(op==null) {
      if(externalId>=0) {
        str.append(" = v" + externalId)
      }
    } else {
      str.append(" = (" + op.name)
      for(vi <- inputs) {
        str.append(" nd" + vi.nodeId)     
      }
      str.append(")")
    }
    str.append(" = " + v)
    if(isConst) {
       str.append(" constant")
    }
    str.toString
  }
  
  
  // get all the nodes involved in calculating this number
  // return (orderedSteps:Array[CaptureNumber],varIdToStepId:Map[Int,Int])
  // which is the set of operations in order and a map from variable ids to step numbers
  def varMap:(Array[CaptureNumber],Map[Int,Int]) = {
    val vars = new scala.collection.mutable.HashMap[Int,CaptureNumber]()
    // recurse by hand so we don't blow out call stack
    var ndStack = new Stack[CaptureNumber]
    ndStack.push(this)
    while(!ndStack.isEmpty) {
        val nd = ndStack.pop()
        vars.put(nd.nodeId,nd)
        if(nd.inputs!=null) {
          for(ci <- nd.inputs) {
            ndStack.push(ci)
          }
        }      
    }
    val stepToVarId = vars.keySet.toArray
    scala.util.Sorting.quickSort(stepToVarId)
    val nkeys = stepToVarId.length
    val orderedSteps = new Array[CaptureNumber](nkeys)
    val densing = new scala.collection.mutable.HashMap[Int,Int]()
    for(i <- 0 until nkeys) {
       val vi = stepToVarId(i) 
       densing(vi) = i
       orderedSteps(i) = vars(vi)
    }
    val varIdToStepId = Map.empty ++ densing
    (orderedSteps,varIdToStepId)
  }
  
  // print out the straight-line program this number represents
  def slpString:String = { 
    val str = new StringBuilder()
    val (orderedSteps,varIdToStepId) = varMap
    for(si <- orderedSteps) {
      str.append(si.longString + "\n")
    }
    str.toString()
  }
}

