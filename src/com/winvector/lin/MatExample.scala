package com.winvector.lin

import com.winvector.implementation.MDouble
import com.winvector.implementation.FDouble

object MatExample {

  def main(args: Array[String]): Unit = {
    val m = Matrix(FDouble,Array(Array(3, 2), Array(1, 1)))
    println(m.field)
    println(m)
    val inv = m.identity(m.rows)/m
    println(inv)
    println(m*inv)
  }

}