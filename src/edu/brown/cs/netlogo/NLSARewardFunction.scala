package edu.brown.cs.netlogo

import burlap.oomdp.singleagent.RewardFunction
import burlap.oomdp.singleagent.GroundedAction
import burlap.oomdp.core.State
import org.nlogo.nvm.ExtensionContext
import org.nlogo.nvm.ReporterTask

class NLSARewardFunction(ext:BURLAPExtension, domain:String, classname:String, reporter:ReporterTask) extends RewardFunction {
  def reward(s:State, a:GroundedAction, sprime:State):Double = {
    ext.versioner.restoreFromBurlapState(ext.contextStack.top, sprime.asInstanceOf[NLState])
    val o = sprime.getObservableObjectAt(0)
    val agent = ext.versioner.getAgentFromString(ext.contextStack.top.workspace.world, o.getName, o.getObjectClass.name)
    
    val agentContext = new ExtensionContext(ext.contextStack.top.workspace, ext.versioner.makeContextForAgent(ext.contextStack.top.nvmContext,agent)) // innerAgentContext)
    ext.versioner.getReport(reporter, agentContext, Array()).asInstanceOf[Double]
  }

}