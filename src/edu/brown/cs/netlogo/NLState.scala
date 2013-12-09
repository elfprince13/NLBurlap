package edu.brown.cs.netlogo

import burlap.oomdp.core.State
import java.security.MessageDigest


import org.nlogo.api.CommandTask
import scala.collection._
import scala.collection.JavaConversions._

class NLState extends State {
  
  def this(state:NLState) = {
    this()
    worldRep = state.worldRep
    fixerUp = state.fixerUp
    hashed = state.hashed
    hashVal = state.hashVal
    state.getAllObjects.foreach {
      obj => val nobj = obj.copy(); nobj.asInstanceOf[NLObjectInstance].setState(this); addObject(nobj)
    }
  }
  
  var worldRep:String = ""
  var fixerUp:CommandTask = null
  private var hashed = false
  private var hashVal = 0
  private val digester = MessageDigest.getInstance("SHA")
  override def copy() = { new NLState(this) }
	
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