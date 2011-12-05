package com.winvector.demo

/**
 * Copyright 2010 John Mount / Win-Vector LLC
 * Released under GNUv3 GPLv3 License (see http://www.gnu.org/licenses/gpl.html)
 * For details/instructions see "Automatic Differentiation with Scala" from www.win-vector.com
 */

import com.winvector.reva.FCapture
import com.winvector.reva.CaptureNumber
import com.winvector.reva.ReverseDiff
import com.winvector.implementation.HelperFns


object ReverseDiffExample {
  def main(args: Array[String]):Unit = {
    val v1 = FCapture.variable(0.1,0)
    val v2 = v1 + v1.field.one
    val r = FCapture.inject(3.0)*v1*(v2*v2);
    println(r.slpString)
    val rd = new ReverseDiff(r)
    val x:Double = 0.15
    val (fx,gx) = rd.gradient(Array(x))
    System.out.println("f(" + x + ")=\t" + fx + "\t")
    HelperFns.printD(gx)
    //value = 2.2
    //r.refresh()
    //val g2 = LFactory.gradient(gradStructs)
    //System.out.println("f(" + value + ")=\t" + r.std + "\t" + g2)
  }
}
