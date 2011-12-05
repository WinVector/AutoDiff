package com.winvector.reva

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */


// implementation class (used by RevDiff, RevDiffQ and RevDiffS
class ReverseDiff(capturedResult:CaptureNumber) {
  // get map from nodeId to node of all nodes involved in calculation
  private val (orderedSteps,varIdToStepId) = capturedResult.varMap
  // get forward references (stepid -> list of (stepids,positions)
  private val forwardRefs:Array[List[(Int,Int)]] = rForwardRefs
  // get maximum externalId
  private val maxExternalId:Int = calcMaxExternalId
  
  override def toString:String = {
    val b = new StringBuilder()
    b.append("ReverseDiff\n")
    val nk = orderedSteps.length
    for(idx <- 0 until nk) {
      val v = orderedSteps(idx)
      b.append("\tstep(" + idx + ")= " + v.longString + "\n")
      for((fstep,fpos) <- forwardRefs(idx)) {
    	  b.append("\t\tforward ref in step: " + fstep + ", position: " + fpos + "\n")
      }
    }
    val vids = varIdToStepId.keySet.toArray
    scala.util.Sorting.quickSort(vids)
    for(varid <- vids) {
      b.append("\tmap nd" + varid + " -> step " + varIdToStepId(varid) + "\n")
    }
    b.append("\tmaxExternal variable id: v" + maxExternalId)
    b.toString()
  }

  private def calcMaxExternalId:Int = {
    var maxEId:Int = -1
    for( v <- orderedSteps) {
      if(v.externalId>=0) {
        maxEId = scala.math.max(maxEId,v.externalId) 
      }
    }
    maxEId
  }
  
  // map stepid to list of forward refs (encoded as list of (stepid,posn)
  private def rForwardRefs:Array[List[(Int,Int)]] = {
    // get inverted lists of references and backwards set of keys
    val nk = orderedSteps.length
    val forwardRefs = new Array[List[(Int,Int)]](nk)
    for(idx <- 0 until nk) {
      forwardRefs(idx) = List[(Int,Int)]()
    }
    for(idx <- 0 until nk) {
      val v = orderedSteps(idx)
      val refs = v.inputs
      if(refs!=null) {
        val nrefs = refs.length
        for( pos <- 0 until nrefs) {
          val fref = varIdToStepId(refs(pos).nodeId)
          forwardRefs(fref) = (idx,pos) :: forwardRefs(fref)
        }
      }
    }
    forwardRefs
  }
  
  private def initCalc(inx:Array[Double]):Array[Double] = {
    // evaluate straight-line program and store node function values in a vector
    val nk = orderedSteps.length
    val x = new Array[Double](nk)
    if(inx==null) {
      for(idx <- 0 until nk) {
        val v = orderedSteps(idx)
        x(idx) =  v.v
      }
    } else {
      for(idx <- 0 until nk) {
        val v = orderedSteps(idx)
        if(v.externalId>=0) {
          x(idx) = inx(v.externalId)                       // variable
        } else if(v.isConst) {
          x(idx) = v.v                                     // constant
        } else if(v.op!=null) {
          x(idx) = v.op.function(v.inputs,varIdToStepId,x) // derived value
        } else {
          x(idx) = v.v                                     // constant
        }
      }
    }
    x
  }
  
  def eval(inx:Array[Double]):Double = {
    val x = initCalc(inx)
    x(x.length-1)
  }
  
  def gradient(inx:Array[Double],minExternalID:Int):(Double,Array[Double]) = {
    val x = initCalc(inx)
    // store node derivatives
    val nk = x.length; 
    val df = new Array[List[Double]](nk)
    for(idx <- 0 until nk) {
      val v = orderedSteps(idx)
      if((v.op!=null)&&(!v.isConst)) {
        df(idx) = v.op.grad(v,varIdToStepId,x)
      }
    }
    // start building result
    val r = new Array[Double](maxExternalId+1)
    val stepIdToD = new Array[Double](nk)
    // walk through the steps in reverse order
    var done:Boolean = false
    var k = nk - 1
    while((!done)&&(k>=0)) {
      val ndK = orderedSteps(k)
      if(!ndK.isConst) {
        var dydk:Double = 0.0
        if(k>=nk-1) {
          dydk = 1.0
        } else {
          val frK = forwardRefs(k)
          //println("fwd nd" + k + ": " + frK)
          for((i,pos) <- frK) {
            val dfi = df(i)
            if(dfi!=null) {
              val dfip:Double = dfi(pos)
              val dydi:Double = stepIdToD(i)
              dydk += dydi*dfip
            }
          }
        }
        //println("dy/dstep" + k + ": " + dydk + " (ext:" + ndK.externalId + ")")
        stepIdToD(k) = dydk
        if(ndK.externalId>=0) {
          r(ndK.externalId) = dydk
          if(ndK.externalId<=minExternalID) {
            done = true
          }
        }
      }
      // prepare for next pass through loop
      k -= 1
    }
    (x(nk-1),r)
  }

  def gradient(inx:Array[Double]):(Double,Array[Double]) = gradient(inx,0)

}
