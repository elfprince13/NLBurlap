package edu.brown.cs.netlogo

import burlap.oomdp.core.Attribute
import burlap.oomdp.core.Domain
import burlap.oomdp.core.Value
import org.nlogo.nvm.ExtensionContext
import org.nlogo.nvm.Context
import org.nlogo.agent.Agent
import org.nlogo.agent.AgentSet
import org.nlogo.api.ReporterTask
import org.nlogo.api.CommandTask
import java.util.{ArrayList,HashMap}
import scala.collection.Set
import scala.collection.JavaConverters._
import scala.collection.JavaConversions._

class NLAttribute(val ext:BURLAPExtension, domain:Domain, name:String, reporter:ReporterTask, setter:CommandTask, val attrType:Attribute.AttributeType) extends Attribute(domain,name,attrType) {
	
  override def copy(newDomain:Domain):NLAttribute = {
    val natt = new NLAttribute(ext, newDomain, name, reporter, setter, attrType)
    natt.lowerLim = this.lowerLim
    natt.upperLim = this.upperLim
    natt.discValues = new ArrayList[String](discValues)
    natt.discValuesHash = new HashMap[String, Integer](discValuesHash)
    natt.hidden = this.hidden
    natt
  }
  
  def makeStaticValue(v:NLValue):Value = {
    val sv = super.valueConstructor
    if (attrType == Attribute.AttributeType.DISC || attrType == Attribute.AttributeType.REAL || attrType == Attribute.AttributeType.REALUNBOUND ){
      sv.setValue(v.getNumericRepresentation)
    } else if (attrType == Attribute.AttributeType.RELATIONAL || attrType == Attribute.AttributeType.MULTITARGETRELATIONAL){
      v.getAllRelationalTargets.foreach{
        target => sv.addRelationalTarget(target)
      }
    } else {
      throw new IllegalArgumentException("Cannot make a static value from an NLValue of type NOTYPE");
    }
    sv
  }
  
  override def valueConstructor():NLValue = { new NLValue(this) }
  
  def evaluate(agentName:String,state:NLState):Any = {
    val o = state.getObject(agentName)
    
    val callingContext = ext.contextStack.top
    
    // Probably need to be observer 
    // val innerObsContext = new Context(callingContext.nvmContext,callingContext.workspace.world.observer)
    // innerObsContext.agent = callingContext.workspace.world.observer
    // innerObsContext.myself = callingContext.workspace.world.observer
    // innerObsContext.agentBit = callingContext.workspace.world.observer.getAgentBit
    val obsContext = new ExtensionContext(callingContext.workspace, ext.versioner.makeContextForAgent(callingContext.nvmContext,callingContext.workspace.world.observer)) // innerObsContext)
    ext.versioner.restoreFromBurlapState(obsContext, state)
    
    val agent = ext.versioner.getAgentFromString(obsContext.workspace.world, agentName, o.getObjectClass.name)
    
    // val innerAgentContext = new Context(obsContext.nvmContext,agent)
    // innerAgentContext.agent = agent
    // innerAgentContext.myself = agent
    // innerAgentContext.agentBit = agent.getAgentBit
    val agentContext = new ExtensionContext(obsContext.workspace, ext.versioner.makeContextForAgent(obsContext.nvmContext,agent)) // innerAgentContext)
    val seenValue = ext.versioner.getReport(reporter, agentContext, Array())
    attrType match {
      case Attribute.AttributeType.RELATIONAL => Set[String](seenValue.asInstanceOf[Agent].toString).asJava
      case Attribute.AttributeType.MULTITARGETRELATIONAL => seenValue.asInstanceOf[AgentSet].agents.map(_.toString).toSet[String].asJava
      case Attribute.AttributeType.REALUNBOUND => seenValue.asInstanceOf[Double]
      case Attribute.AttributeType.REAL => val out = Math.min(Math.max(seenValue.asInstanceOf[Double], this.lowerLim), this.upperLim)
      if (out != seenValue.asInstanceOf[Double]) {
        Console.err.printf("Warning: clamping applied to evaluation of attribute '%s'\n",name)
      }
      out
      case Attribute.AttributeType.DISC => if (discValuesHash.get(seenValue.asInstanceOf[String]) != null){
        discValuesHash.get(seenValue.asInstanceOf[String])
      } else {
        throw new IllegalArgumentException("reporter task for attribute '%s' generate a value (%s) which was not a String\n".format(name,seenValue.toString))
      }
      case Attribute.AttributeType.NOTYPE => seenValue
    }
  }
  
  def update(agentName:String,state:NLState,intype:Any):Unit = {
    val o = state.getObject(agentName)
    
    val callingContext = ext.contextStack.top
    
    // Probably need to be observer 
    //val innerObsContext = new Context(callingContext.nvmContext,callingContext.workspace.world.observer)
    //innerObsContext.agent = callingContext.workspace.world.observer
    //innerObsContext.myself = callingContext.workspace.world.observer
    //innerObsContext.agentBit = callingContext.workspace.world.observer.getAgentBit
    val obsContext = new ExtensionContext(callingContext.workspace,  ext.versioner.makeContextForAgent(callingContext.nvmContext,callingContext.workspace.world.observer)) // innerObsContext)
    if(!ext.versioner.isStreaming){
       ext.versioner.restoreFromBurlapState(obsContext, state) 
    }
    val agent = ext.versioner.getAgentFromString(obsContext.workspace.world, agentName, o.getObjectClass.name)
    
    //val innerAgentContext = new Context(obsContext.nvmContext,agent)
    //innerAgentContext.agent = agent
    //innerAgentContext.myself = agent
    //innerAgentContext.agentBit = agent.getAgentBit
    val agentContext = new ExtensionContext(obsContext.workspace, ext.versioner.makeContextForAgent(obsContext.nvmContext,agent)) // innerAgentContext)
    ext.versioner.doCommands(setter, agentContext, Array(attrType match {
      case Attribute.AttributeType.RELATIONAL => ext.versioner.getAgentFromString(obsContext.workspace.world, intype.asInstanceOf[String])
      case Attribute.AttributeType.MULTITARGETRELATIONAL => ext.versioner.getAgentSetFromStrings(obsContext.workspace.world, intype.asInstanceOf[java.util.Set[String]])
      case Attribute.AttributeType.REALUNBOUND => Double.box(intype.asInstanceOf[Double])
      case Attribute.AttributeType.REAL => val out = Math.min(Math.max(intype.asInstanceOf[Double], this.lowerLim), this.upperLim)
      if (out != intype.asInstanceOf[Double]) {
        Console.err.printf("Warning: clamping applied to evaluation of attribute '%s'\n",name)
      }
      Double.box(out)
      case Attribute.AttributeType.DISC => discValues.get(intype.asInstanceOf[Integer])
      case Attribute.AttributeType.NOTYPE => intype.asInstanceOf[AnyRef]
    }))
  }
  
}