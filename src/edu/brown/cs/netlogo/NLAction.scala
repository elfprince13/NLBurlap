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

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

class NLAction(ext:BURLAPExtension, indomain:Domain, inname:String, action:CommandTask, classesList:Array[String]=Array[String]()) extends Action {
  domain = indomain
  name = inname
  parameterClasses = classesList
  parameterOrderGroup = (0 until parameterClasses.length).map(i => "%s.P%d".format(name,i)).toArray
  
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
    val agent = ext.versioner.getAgentFromString(callingContext.workspace.world, name)
    val ourContext = new ExtensionContext(callingContext.workspace, ext.versioner.makeContextForAgent(callingContext.nvmContext,agent)) //innerObsContext)
    ext.versioner.restoreFromBurlapState(ourContext, safeS)
    
    ext.versioner.doCommands(action, ourContext, params.map(p => ext.versioner.getAgentFromString(ourContext.workspace.world, p)))
    
    val outstate = ext.versioner.copyIntoState(ourContext.workspace.world, domain)
    outstate.setFixerUp(safeS.fixerUp)
    outstate
  }

}