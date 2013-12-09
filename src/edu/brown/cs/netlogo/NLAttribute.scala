package edu.brown.cs.netlogo

import burlap.oomdp.core.Attribute
import burlap.oomdp.core.Domain




import org.nlogo.api.ReporterTask

import java.util.{ArrayList,HashMap}

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
  
  override def valueConstructor():NLValue = { new NLValue(this) }
  
  def evaluate(agentName:String,state:NLState):Any = {
    
  }
  
}