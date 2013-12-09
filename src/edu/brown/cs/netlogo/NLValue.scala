package edu.brown.cs.netlogo

import burlap.oomdp.core.Value
import burlap.oomdp.core.Attribute

import org.nlogo.api.DefaultClassManager
import org.nlogo.api.LogoListBuilder
import org.nlogo.api.PrimitiveManager
import org.nlogo.api.Syntax
import org.nlogo.api.Context
import org.nlogo.api.Turtle
import org.nlogo.api.World
import org.nlogo.api.DefaultReporter
import org.nlogo.api.DefaultCommand
import org.nlogo.api.Argument
import org.nlogo.api.ExtensionException
import org.nlogo.api.LogoException
import org.nlogo.nvm.ExtensionContext

class NLValue(val attr:NLAttribute) extends Value(attr) {
  
  def this(value:NLValue) = {
    this(value.attr)
    owningAgent=value.owningAgent
    boundState = value.boundState
  }
  
  var owningAgent:String = ""
  var boundState:NLState = null
  def copy(): Value = new NLValue(this)
  
  def setOwningAgent(s : String) = { owningAgent = s }
  def bindState(s : NLState) = { boundState = s }

  def setValue(v: String): Unit = { throw new ExtensionException("NLValues are read-only. Their state is evaluted from the NetLogo model") }
  def setValue(v: Double): Unit = { throw new ExtensionException("NLValues are read-only. Their state is evaluted from the NetLogo model") }
  def setValue(v: Int): Unit = { throw new ExtensionException("NLValues are read-only. Their state is evaluted from the NetLogo model") }

  def addRelationalTarget(t: String): Unit = { throw new ExtensionException("NLValues are read-only. Their state is evaluted from the NetLogo model") }

  def clearRelationTargets(): Unit = { throw new ExtensionException("NLValues are read-only. Their state is evaluted from the NetLogo model") }

  def removeRelationalTarget(target: String): Unit = { throw new ExtensionException("NLValues are read-only. Their state is evaluted from the NetLogo model") }

  def getDiscVal(): Int = { 
    if (attr.attrType == Attribute.AttributeType.DISC) {
      attr.evaluate(owningAgent,boundState).asInstanceOf[Int]
    } else {
      throw new ExtensionException("Non-DISC values cannot be evaluated discretely")
    }
  }

  def getRealVal(): Double = { 
    if (attr.attrType == Attribute.AttributeType.REAL ||attr.attrType == Attribute.AttributeType.REALUNBOUND) {
      attr.evaluate(owningAgent,boundState).asInstanceOf[Double]
    } else if (attr.attrType == Attribute.AttributeType.DISC) {
      attr.evaluate(owningAgent,boundState).asInstanceOf[Int].toDouble
    } else {
      throw new ExtensionException("Relational and unknown value types cannot be evaluated as reals")
    }
  }

  def getStringVal(): String = { null }

  def getAllRelationalTargets(): java.util.Set[String] = { null }

  def getNumericRepresentation(): Double = { 0.0d }

}