package edu.brown.cs.netlogo

import burlap.behavior.statehashing.StateHashFactory
import burlap.oomdp.core.State
import burlap.behavior.statehashing.StateHashTuple

class NLHashFactory extends StateHashFactory{
	def hashState(s:State):StateHashTuple = {
	  new NLStateHashTuple(new NLState(s))
	}
}

class NLStateHashTuple(state:NLState) extends StateHashTuple(state) {
  def computeHashCode() = {
    s.hashCode()
  }
}