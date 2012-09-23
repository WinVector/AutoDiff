package com.winvector.lin

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test

import com.winvector.definition.NumberBase
import com.winvector.definition.VectorFN
import com.winvector.implementation.FwdDiff
import com.winvector.implementation.MDouble
import com.winvector.implementation.FDouble




class TestMat extends AssertionsForJUnit {

  @Test def testMat1:Unit = {
        //val m = Matrix(FDouble,Array(Array(3, 2), Array(1, 1)))
        val m = FDouble.injectM(Array(Array(3, 2), Array(1, 1)))
        val inv = m.identity(m.rows)/m
        assertTrue(inv.rows==m.rows)
        assertTrue(inv.rows==inv.columns)
        val check = m*inv
        assertTrue(check.rows==m.rows)
        assertTrue(check.rows==check.columns)
        for(i <- 0 until check.rows) {
          for(j <- 0 until check.columns) {
            if(i!=j) {
              assertTrue(scala.math.abs(check.get(i,j).project)<1.0e-5)
            } else {
              assertTrue(scala.math.abs(check.get(i,j).project-1.0)<1.0e-5)
            }
          }
        }
   }
}