package edu.brown.cs.netlogo

import burlap.oomdp.core.Attribute
import burlap.oomdp.core.Domain
import burlap.oomdp.core.Value
import org.nlogo.nvm.ExtensionContext
import org.nlogo.nvm.Context

import org.nlogo.agent.Agent
import org.nlogo.agent.AgentSet




import org.nlogo.api.ReporterTask

import java.util.{ArrayList,HashMap}
import scala.collection.Set
import scala.collection.JavaConverters._
import scala.collection.JavaConversions._

class NLAttribute(ext:BURLAPExtension, domain:Domain, name:String, reporter:ReporterTask, val attrType:Attribute.AttributeType) extends Attribute(domain,name,attrType) {
	
  override def copy(newDomain:Domain):NLAttribute = {
    val natt = new NLAttribute(ext, newDomain, name, reporter, attrType)
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
    val obsContext = new ExtensionContext(callingContext.workspace, new Context(callingContext.nvmContext,callingContext.workspace.world.observer))
    ext.versioner.restoreFromBurlapState(obsContext, state)
    
    val agent = ext.versioner.getAgentFromString(obsContext.workspace.world, agentName, o.getObjectClass.name)
    
    val agentContext = new ExtensionContext(obsContext.workspace, new Context(obsContext.nvmContext,agent))
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
  
}