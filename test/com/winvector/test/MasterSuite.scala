package com.winvector.test

import org.scalatest.Suite

import com.winvector.lin.TestMat
import com.winvector.opt.TestLinMin
import com.winvector.opt.TestCG


object MasterSuite {
   def main(args : Array[String]) : Unit = {
     val specs = List(
        classOf[TestBasic],
        classOf[TestArithOps],
        classOf[TestAlgebraRelns],
        classOf[TestDemo],
        classOf[TestDiff],
        classOf[TestLinMin],
        classOf[TestCG],
        classOf[TestMat]
     )
     for(ci <- specs) {
       ci.newInstance().execute()
     }
   }
}