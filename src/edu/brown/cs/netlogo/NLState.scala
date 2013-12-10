package edu.brown.cs.netlogo

import burlap.oomdp.core.State
import burlap.oomdp.core.Attribute
import java.security.MessageDigest
import org.nlogo.api.CommandTask
import scala.collection._
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

class NLState extends State {
  
  def this(state:State) = {
    this()
    state match {
      case nls : NLState =>
            worldRep = nls.worldRep
            fixerUp = nls.fixerUp
            hashed = nls.hashed
            hashVal = nls.hashVal
            nls.getAllObjects.foreach {
            	obj => val nobj = obj.copy(); nobj.asInstanceOf[NLObjectInstance].setState(this); addObject(nobj)
            }
      case s : State =>
        	var ext = ExtensionPointer.ext
        	ext.versioner.prepForStreamingStates(this)
            s.getAllObjects.foreach {
              obj => val nobj = new NLObjectInstance(obj.getObjectClass, obj.getName, this)
              obj.getValues.foreach{
                v => v.getAttribute.`type` match {
                  case Attribute.AttributeType.DISC => nobj.setValue(v.attName,v.getDiscVal)
                  case Attribute.AttributeType.REAL => nobj.setValue(v.attName,v.getRealVal)
                  case Attribute.AttributeType.REALUNBOUND => nobj.setValue(v.attName,v.getRealVal)
                  case Attribute.AttributeType.RELATIONAL =>   nobj.setValue(v.attName,v.getStringVal)
                  case Attribute.AttributeType.MULTITARGETRELATIONAL =>   nobj.getValueForAttribute(v.attName).asInstanceOf[NLValue].addRelationalTargets(v.getAllRelationalTargets.toSet[String])
                  case _ => throw new UnsupportedOperationException("Don't know how to turn NOTYPE Values into NOTYPE NLValues")
                }
              }
              addObject(nobj)
            }
        	worldRep = ext.versioner.cloneWorld(ext.contextStack.top.workspace.world)
    }
    

  }
  
  var worldRep:String = ""
  var fixerUp:CommandTask = null
  private var hashed = false
  private var hashVal = 0
  private val digester = MessageDigest.getInstance("SHA")
  override def copy() = { new NLState(this) }
  
  def copyStatic() = {
    val outState = new State()
    outState.getAllObjects().foreach{
      o => o.asInstanceOf[NLObjectInstance].makeStatic()
    }
	outState
  }
	
	def setWorldRep(wr:String) = {worldRep = wr; hashed = false; hashVal = 0}
	def setFixerUp(fru:CommandTask) = {fixerUp = fru}
	
	override def hashCode:Int = {
	  if(!hashed){
	    digester.reset
	    hashVal = xorDigestBytes(digester.digest(worldRep.getBytes))
	  }
	  hashVal
	}
	
	private def xorDigestBytes(bytes:Array[Byte]):Int = {
	  (bytes.view.zipWithIndex.map(
	    bi => bi._1.toInt << (8 * (bi._2 % 4))
	  )).foldLeft(0)((a,b) => a ^ b)
	}
}