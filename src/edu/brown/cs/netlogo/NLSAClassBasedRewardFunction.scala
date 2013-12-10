package edu.brown.cs.netlogo

import burlap.oomdp.singleagent.classbased.ClassBasedRewardFunction
import burlap.oomdp.singleagent.RewardFunction
import burlap.oomdp.singleagent.GroundedAction
import burlap.oomdp.core.State

class NLSAClassBasedRewardFunction(ext:BURLAPExtension, initClasses:java.util.Map[String, RewardFunction] ) extends ClassBasedRewardFunction(initClasses) {

  override def reward(s:State, a:GroundedAction, sprime:State) = {
    val nls = new NLState(s)
    ext.versioner.restoreFromBurlapState(ext.contextStack.top, nls)
    super.reward(nls, a, null) // We only care about current actions
  }
  
}