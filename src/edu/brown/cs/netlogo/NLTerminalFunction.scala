package edu.brown.cs.netlogo

import burlap.oomdp.core.TerminalFunction
import burlap.oomdp.core.Domain
import org.nlogo.api.ReporterTask
import burlap.oomdp.core.State
import org.nlogo.nvm.ExtensionContext

class NLTerminalFunction(ext:BURLAPExtension, domain:Domain, reporter:ReporterTask) extends TerminalFunction {

  def isTerminal(s:State):Boolean = {
    val agentContext = new ExtensionContext(ext.contextStack.top.workspace, ext.versioner.makeContextForAgent(ext.contextStack.top.nvmContext,ext.contextStack.top.workspace.world.observer)) // innerAgentContext)
    ext.versioner.getReport(reporter, agentContext, Array()).asInstanceOf[Boolean]
  }
  
}