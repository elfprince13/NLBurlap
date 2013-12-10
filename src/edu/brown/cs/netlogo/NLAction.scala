package edu.brown.cs.netlogo

import burlap.oomdp.singleagent.Action
import burlap.oomdp.core.Attribute
import burlap.oomdp.core.Domain
import burlap.oomdp.core.State

import org.nlogo.api.DefaultClassManager
import org.nlogo.api.LogoListBuilder
import org.nlogo.api.PrimitiveManager
import org.nlogo.api.Syntax
import org.nlogo.nvm.Context
import org.nlogo.api.Turtle
import org.nlogo.api.World
import org.nlogo.api.DefaultReporter
import org.nlogo.api.DefaultCommand
import org.nlogo.api.Argument
import org.nlogo.api.ExtensionException
import org.nlogo.api.LogoException
import org.nlogo.nvm.ExtensionContext
import org.nlogo.api.CommandTask


class NLAction(ext:BURLAPExtension, domain:Domain, name:String, action:CommandTask, classesList:Array[String]=Array[String]()) extends Action(name, domain, classesList) {

  protected  def performActionHelper(s: State, params: Array[String]): State = {
    val safeS = s match {
      case nls:NLState => nls
      case ss:State => new NLState(ss)
    }
    val callingContext = ext.contextStack.top
    
    // Probably need to be observer 
    //val innerObsContext = new Context(callingContext.nvmContext,callingContext.workspace.world.observer)
    //innerObsContext.agent = callingContext.workspace.world.observer
    //innerObsContext.myself = callingContext.workspace.world.observer
    //innerObsContext.agentBit = callingContext.workspace.world.observer.getAgentBit
    val ourContext = new ExtensionContext(callingContext.workspace, ext.versioner.makeContextForAgent(callingContext.nvmContext,callingContext.workspace.world.observer)) //innerObsContext)
    ext.versioner.restoreFromBurlapState(ourContext, safeS)
    
    ext.versioner.doCommands(action, ourContext, Array())
    
    val outstate = ext.versioner.copyIntoState(ourContext.workspace.world, domain)
    outstate.setFixerUp(safeS.fixerUp)
    outstate
  }

}