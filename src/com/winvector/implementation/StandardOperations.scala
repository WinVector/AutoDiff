package com.winvector.implementation

import com.winvector.definition.NumberBase
import com.winvector.definition.VectorFN

/**
 * all the standard operations from NumberBase encoded as small VectorFns (for testing)
 */
object StandardOperations {
	val fns = Map(
	    "*" -> new VectorFN {
	    	def dim = 2
	        def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
               p(0) * p(1)
            }
	    },
	    "+" -> new VectorFN {
	    	def dim = 2
	        def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
               p(0) + p(1)
            }
	    },
	    "-" -> new VectorFN {
	    	def dim = 2
	        def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
               p(0) - p(1)
            }
	    },
	    "/" -> new VectorFN {
	    	def dim = 2
	        def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
               p(0) / p(1)
            }
	    },

	    "abs" -> new VectorFN {
	    	def dim = 1
	        def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
               p(0).abs
            }
	    },
	    "acos" -> new VectorFN {
	    	def dim = 1
	        def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
               p(0).acos
            }
	    },
	    "asin" -> new VectorFN {
	    	def dim = 1
	        def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
               p(0).asin
            }
	    },
	    "atan" -> new VectorFN {
	    	def dim = 1
	        def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
               p(0).atan
            }
	    },
	    "cos" -> new VectorFN {
	    	def dim = 1
	        def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
               p(0).cos
            }
	    },
	    "cosh" -> new VectorFN {
	    	def dim = 1
	        def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
               p(0).cosh
            }
	    },
	    "exp" -> new VectorFN {
	    	def dim = 1
	        def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
               p(0).exp
            }
	    },
	    "log" -> new VectorFN {
	    	def dim = 1
	        def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
               p(0).log
            }
	    },
	    "max" -> new VectorFN {
	    	def dim = 2
	        def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
               p(0).max(p(1))
            }
	    },
	    "min" -> new VectorFN {
	    	def dim = 2
	        def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
               p(0).min(p(1))
            }
	    },
	    "neg" -> new VectorFN {
	    	def dim = 1
	        def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
               p(0).neg
            }
	    },
	    "unary_-" -> new VectorFN {
	    	def dim = 1
	        def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
               -p(0)
            }
	    },
	    "sigmoid" -> new VectorFN {
	    	def dim = 1
	        def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
               p(0).sigmoid
            }
	    },
	    "sin" -> new VectorFN {
	    	def dim = 1
	        def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
               p(0).sin
            }
	    },
	    "sinh" -> new VectorFN {
	    	def dim = 1
	        def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
               p(0).sinh
            }
	    },
	    "sq" -> new VectorFN {
	    	def dim = 1
	        def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
               p(0).sq
            }
	    },
	    "sqrt" -> new VectorFN {
	    	def dim = 1
	        def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
               p(0).sqrt
            }
	    },
	    "tan" -> new VectorFN {
	    	def dim = 1
	        def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
               p(0).tan
            }
	    },
	    "tanh" -> new VectorFN {
	    	def dim = 1
	        def apply[Y <: NumberBase[Y]](p:Array[Y]):Y = {
               p(0).tanh
            }
	    }
	 )
	 
	 abstract trait VectorTest {
        def dim:Int
        def apply[Y<:NumberBase[Y]](x:Array[Y]):Boolean
	 }
	
	 val tests = Map(
	    "!=" -> new VectorTest {
	    	def dim = 2
	        def apply[Y <: NumberBase[Y]](p:Array[Y]):Boolean = {
               p(0) != p(1)
            }
	    },
	    "<" -> new VectorTest {
	    	def dim = 2
	        def apply[Y <: NumberBase[Y]](p:Array[Y]):Boolean = {
               p(0) < p(1)
            }
	    },
	    "<=" -> new VectorTest {
	    	def dim = 2
	        def apply[Y <: NumberBase[Y]](p:Array[Y]):Boolean = {
               p(0) <= p(1)
            }
	    },
	    "==" -> new VectorTest {
	    	def dim = 2
	        def apply[Y <: NumberBase[Y]](p:Array[Y]):Boolean = {
               p(0) == p(1)
            }
	    },
	    ">" -> new VectorTest {
	    	def dim = 2
	        def apply[Y <: NumberBase[Y]](p:Array[Y]):Boolean = {
               p(0) > p(1)
            }
	    },
	    ">=" -> new VectorTest {
	    	def dim = 2
	        def apply[Y <: NumberBase[Y]](p:Array[Y]):Boolean = {
               p(0) >= p(1)
            }
	    },
	    "nonZero" -> new VectorTest {
	    	def dim = 1
	        def apply[Y <: NumberBase[Y]](p:Array[Y]):Boolean = {
               p(0).nonZero
            }
	    }
	 )
	 
	 abstract trait ParamOp {
        def apply[Y<:NumberBase[Y]](x:Y,p:Double):Y
	 }
	 
	 val paramOps = Map(
	     "pospow" -> new ParamOp {
	        def apply[Y<:NumberBase[Y]](x:Y,p:Double):Y = {
               x.pospow(p)
            }	       
	     }
	 )
}