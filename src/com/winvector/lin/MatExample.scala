package com.winvector.lin

import com.winvector.implementation.MDouble
import com.winvector.implementation.FDouble

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
  }

}