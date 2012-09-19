package com.winvector.marketchain

import scala.collection.mutable.ListBuffer

import com.winvector.definition.Field
import com.winvector.definition.NumberBase
import com.winvector.definition.VectorFN
import com.winvector.implementation.MDouble
import com.winvector.implementation.FDouble
import com.winvector.implementation.DualNumber
import com.winvector.implementation.FDualNumber
import com.winvector.implementation.HelperFns._
import com.winvector.opt.CG
import com.winvector.implementation.FwdDiff
import com.winvector.implementation.NumDiff
import com.winvector.reva.RevDiff
import com.winvector.reva.RevDiffQ
import com.winvector.lin.Matrix
import com.winvector.opt.CG

import com.winvector.marketchain.ValueChain.MktState
import com.winvector.marketchain.ValueChain.HiddenState

object ComputeStrat {
	var verbose = false
	var printAtAll = true
  
	def convolute[X <: NumberBase[X]](a:Map[Int,X], b:Map[Int,X]):Map[Int,X]  = {
 		var r = Map[Int,X]()
		if(a.isEmpty) {
		    for(mb <- b) {
		    	r += mb
            }
		} else if(b.isEmpty) {
			for(ma <- a) {
			   r += ma
			}
		} else {
			for((ia,pa) <-a) {
				for((ib,pb) <- b) {
					val index = ia + ib
					val delta = pa*pb
					val ov = r.get(index)
                    ov match {
                      case None => r += (index -> delta)
                      case Some(x) => r += (index -> (x+delta))
                    }
				}
			}
		}
        r
    }
 
 	def neg[X <: NumberBase[X]](a:Map[Int, X]):Map[Int,X] = {
 	    var r = Map[Int,X]()
        for((ia,pa) <-a) {
          r +=  ((-ia) -> pa)
        }
        r
	}
  
    /**
	 * solve transitions * x = x, 1.x = 1
	 * @param transitions
	 * @return
	 */
	 def solveForStationary[X  <: NumberBase[X]](transitions:Matrix[X]):Matrix[X] = {
	    val factory = transitions.field
        val one = factory.one
		val n = transitions.rows
		val a = factory.matrix(n+1,n)
		val b = factory.matrix(n+1,1)
		b.set(n,0,one)
		for(i <- 0 to n-1) {
			for(j <- 0 to n-1) {
				if(i==j) {
					a.set(i,j,transitions.get(i,j)-one)
				} else {
					a.set(i,j,transitions.get(i,j))
				}
			}
			a.set(n,i,one)
		}
		// solve a x = b, least squares 
		val at = a.transpose
		val atb = at*b
		val ata = at*a
		val soln = atb/ata
		soln
	}
  

	def compareLM(o1:List[MktState], o2:List[MktState]):Int = {
		val n = o1.size
		if(n!=o2.size) {
			if(n>=o2.size) {
				return 1
			} else {
				return -1
			}
		}
		for(i <- 0 to n-1) {
			val cmp = o1(i).compareTo(o2(i))
			if(cmp!=0) {
				return cmp
			}
        }
	    0
    }
 
 	class DetailedState(val mkt:List[MktState], val h:HiddenState)  extends Comparable[DetailedState]  {

		override def compareTo(o: DetailedState):Int = {
			val cmp = compareLM(mkt,o.mkt)
			if(cmp!=0) {
				return cmp
			} else {
				return h.compareTo(o.h)
            }
		}
  
		override def hashCode():Int = {
		  var v = mkt.size + 3*h.ordinal()
		  var i = 1
		  for( mi <- mkt ) {
		    v = (2*i+1)*v + mi.ordinal()
		  }
		  v
		}

		override def equals(that: Any):Boolean = {
		  that match { 
			case other: DetailedState => compareTo(other)==0
			case _ => false
		  }
        }
  
		override def toString():String = {
			val b = new StringBuilder()
			b.append("[")
			for(mi <- mkt) {
				b.append(mi)
			}
			b.append("](" + h + ")")
			b.toString()
		}
		
		def successors():List[DetailedState] = {
			val suc = new ListBuffer[DetailedState]()
			val k = mkt.size
			for(m <- MktState.values()) {
				val v = new ListBuffer[MktState]()
				for(i <- 1 to k-1) {
					v += mkt(i)
				}
				v += m
				val vl = v.toList
				for(h <- HiddenState.values()) {
					val d = new DetailedState(vl,h)
					suc += d
				}
			}
			if(verbose) {
				println()
				println("succ " + this + "\t" + suc)
				println()
			}
			suc.toList
		}
	}

  	def kVectors(k:Int):List[List[MktState]] = {
		var r = new ListBuffer[List[MktState]]()
		if(k<=0) {
			r += List[MktState]()
		} else if(k<=1) {
			for(m <- MktState.values()) {
				val vi = List(m)
				r += vi
			}
		} else {
			val sub = kVectors(k-1)
			for(si <- sub) {
				for(m <- MktState.values()) {
				    val vi = m :: si
					r += vi
				}
			}
		}
		r.toList
	}


  	def encodeF1Strategy[X  <: NumberBase[X]](k:Int, followProb:X):Map[List[MktState],Map[Int,X]]  = {
  	    val one = followProb.field.one
		var strategy = Map[List[MktState],Map[Int,X]]()
		val vecs = kVectors(k)
		for(vi <- vecs) {
			val last = vi(k-1)
			last match {
				case MktState.P => strategy += (vi -> Map(1 -> followProb, 0 -> (one-followProb)))
				case MktState.Z => strategy += (vi -> Map(0 -> one))
				case MktState.M => strategy += (vi -> Map(-1 -> followProb, 0 -> (one-followProb)))
			}
		}
        strategy
	}


   	class MarketParams {
		var k:Int = 1
		var n:Int = 2
		var m:Int = 2
		var p:Double = 0.8
		var q:Double = 0.9
	}

    class Market[X  <: NumberBase[X]](val factory:Field[X], val params:MarketParams, val strategy:Map[List[MktState],Map[Int,X]]) {
		var states = Map[DetailedState,Int]()

        {
			val vecs = kVectors(params.k)
			for(mseq <- vecs) {
				for(hi <- HiddenState.values()) {
					val di = new DetailedState(mseq,hi)
					states += (di -> states.size)
				}
			}
			if(verbose) {
			   println("states:")
			   println(states)
			   println()
			}
        }
        
		def computeTransitionMatrix(tradeForced:Boolean, forcedTrade:Int):Matrix[X] = {
			val nstates = states.size
			val transitions = factory.matrix(nstates,nstates)
			val zero = factory.zero
			val one = factory.one
			val qv = factory.inject(params.q)
   			val pv = factory.inject(params.p)
			for((sa,ia) <- states) {
				val traderOdds = strategy(sa.mkt)
				val negTraderOdds = neg(traderOdds)
				var tOdds = Map[HiddenState,Map[MktState,X]]()
				for(hi <- HiddenState.values()) {
					var type2Odds = Map[Int,X]()
					if(hi.equals(HiddenState.P)) {
						type2Odds = Map( 1 -> qv, -1 -> (one-qv))
					} else {
					    type2Odds = Map( 1-> (one-qv), -1 -> qv)
					}
					var odds = Map[Int,X]()
					if(tradeForced) {
					    odds = Map( (-forcedTrade) -> one ) // outcome is opposite of first half of trade
					} else {
					    odds = negTraderOdds
					}
					for(i <- 1 to params.m) {
						odds = convolute(odds,type2Odds)
					}
					for( i <- 1 to (params.n - 1)) {
						odds = convolute(odds,negTraderOdds)
					}
					var sodds = Map[MktState,X]()
					for(mi <- MktState.values()) {
						sodds += (mi -> zero)
					}
					for((count,prob) <- odds) {
					    var key = MktState.Z
                        if(count>0) {
                          key = MktState.P
                        } else if(count<0) {
                          key = MktState.M
                        }
						val ov = sodds(key)
						sodds += (key -> (ov+prob))
					}
					tOdds += (hi -> sodds )
				}
				for(sb <- sa.successors()) {
				  	//println("states: " + states)
				    //println("sb: " + sb + "\t" + sb.compareTo(sb))
					val ib = states(sb)
					//println("idx: " + ib)
                    var phTransition = zero
                    if(sa.h.equals(sb.h)) {
                      phTransition = pv
                    } else {
                      phTransition = one - pv
                    }
					val pMarket = tOdds(sb.h)(sb.mkt(params.k-1))  // TODO: check this
					val prob = phTransition*pMarket
					if(verbose) {
					  println("p("  + ib + "," + ia + ")= " + prob + "\t|\t" + phTransition + "\t" + pMarket)
					}
					transitions.set(ib,ia,prob)
				}
			}
			transitions
		}
	}
    

    def eval[X  <: NumberBase[X]](factory:Field[X], params:MarketParams, followProb:X):X = {
		val strategy = encodeF1Strategy(params.k,followProb)
		if(verbose) {
		  println("strategy:")
          println(strategy)
          println()
		}
		val market = new Market[X](factory,params,strategy)

        val transitions = market.computeTransitionMatrix(false,0)
		if(verbose) {
		    println("transitions:")
			println(transitions)
			println()
		}
		val v = solveForStationary[X](transitions)
		if(verbose) {
			println("stationary dist:")
			println(v)
			val check = transitions * v
			println("check:")
			println(check)
			println()
		}

		var conditionedTransitions = Map[Int,Matrix[X]]()
		for(trade <- -1 to 1) {
			val tf = market.computeTransitionMatrix(true,trade)
			conditionedTransitions +=  (trade -> tf)
		}
        val zero = factory.zero
		var total = zero
		for((sa,ia) <- market.states) {
			val stateProb = v.get(ia,0)
			val traderOdds = strategy(sa.mkt)
			if(verbose) {
				println(sa + "\t" + ia + "\t" + stateProb)
			}
			for(sb <- sa.successors()) {
				val ib = market.states(sb)
				for((trade,tradeProb) <- traderOdds) {
					val tfo = conditionedTransitions.get(trade)
					tfo match {
					  case Some(tf) =>
						val transitionProb:X = tf.get(ib,ia)
						val eventProb = stateProb*transitionProb*tradeProb
						val outcome = sb.mkt(params.k-1)
						var eventValue:X = zero
						outcome match {
							case MktState.P => eventValue = factory.inject(trade)
							case MktState.M => eventValue = factory.inject(-trade)
							case _ =>
						}
						if(verbose) {
							//if((eventProb>0.0)&&(eventValue!=0.0)) {
								println("\t" + (-trade) + "\t" + eventValue + "\t" + eventProb + "\t" + sb)
							//}
						}
						total += eventProb*eventValue
   					case _ =>
					}
				}
			}
		}
		if(verbose) {
			println("strategy value: " + total)
		} else {
		  if(printAtAll) {
			println("" + followProb + "\t" + total)
		  }
		}
		total
    }


	def main(args: Array[String]):Unit = {
		println("hello")
		verbose = false
		printAtAll = true
		val params = new MarketParams()
		
		val emf = FDouble
  
		val regPattern = 100
  		if(!verbose) {
			println("followProb" + "\t" + "strategyValue")			
		}
		//val followProb = alg.inject(0.2)
		for(divi <- 0 to regPattern) {
			val followProb = emf.inject(divi)/emf.inject(regPattern)
			val v = eval(emf,params,followProb);
		}
		//println("f(" + followProb + ")= " + v)
  
		
		println()
		printAtAll = false
  
        val h = new VectorFN {
           def apply[X <: NumberBase[X]](x:Array[X]):X = {
			  val x0 = x(0)
			  val factory = x0.field
			  val v = eval(factory,params,x0)
		  	  v.neg
		   }
        }
        
        val x0 = Array(1.0)
        println("x0: " + x0(0))
        println("f(x0): " + h.apply(x0))
        CG.debug = true
     	val (xMin2,fX2) = CG.minimize(new FwdDiff(h),x0)
		println()
		println("xmin2: " + xMin2(0))
		println("f(xmin2): " + fX2)

		println("all done")
	}

}
