package edu.brown.cs.netlogo

import burlap.oomdp.core.ObjectInstance
import burlap.oomdp.core.ObjectClass
import burlap.oomdp.core.Value
import java.util.ArrayList
import scala.collection.JavaConversions._

class NLObjectInstance(obClass:ObjectClass,name:String,private var boundTo:NLState) extends ObjectInstance(obClass,name) {
  
  def this(obInst:NLObjectInstance) = this(obInst.getObjectClass,obInst.getName,obInst.getState)
  
  override def initializeValueObjects() = {
    values = new ArrayList[Value](obClass.numAttributes)
    obClass.attributeList.foreach {
      attr => val nVal = attr.asInstanceOf[NLAttribute].valueConstructor
      nVal.setOwningAgent(name)
      nVal.bindState(boundTo)
      values.add(nVal)
    }
  }
  initializeValueObjects()
  
  def getState = boundTo
  def setState(newBind:NLState) = {
    boundTo = newBind
    values.foreach {
      value => value.asInstanceOf[NLValue].bindState(boundTo)
    }
  }
  
  override def copy() = new NLObjectInstance(this)
}