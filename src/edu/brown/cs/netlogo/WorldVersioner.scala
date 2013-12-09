package edu.brown.cs.netlogo

import org.nlogo.agent.World
import org.nlogo.api.Context

import org.nlogo.api.AgentSet

import org.nlogo.api.{ExtensionException,LogoException}
import org.nlogo.nvm.ExtensionContext
import org.nlogo.nvm.{CompilerInterface,FileManager}

import burlap.oomdp.core.Domain
import burlap.oomdp.core.ObjectInstance

import scala.collection._
import scala.collection.JavaConversions._

import java.io.{StringWriter,StringReader,PrintWriter,BufferedReader}
import java.util._


class WorldVersioner(bufferSize:Int) {

  val sw = new StringWriter(bufferSize)
  val pw = new PrintWriter(sw)
  
  def cloneWorld(oldWorld:World):String = {
    sw.getBuffer().setLength(0)
    
    oldWorld.exportWorld(pw, true)
    
    sw.toString()
    
  }
  
  def copyIntoState(world:World,domain:Domain) = {
    val state = new NLState()
    state.setWorldRep(cloneWorld(world))
    world.program.breedsSingular.foreach {
      kv => if (domain.getObjectClass(kv._1) != null) world.program.breeds(kv._2).asInstanceOf[AgentSet].agents.foreach {
        agent => state.addObject(new NLObjectInstance(domain.getObjectClass(kv._1),agent.toString,state))
      }
    }
    world.program.linkBreedsSingular.foreach {
      kv => if (domain.getObjectClass(kv._1) != null) world.program.linkBreeds(kv._2).asInstanceOf[AgentSet].agents.foreach {
        agent => state.addObject(new NLObjectInstance(domain.getObjectClass(kv._1),agent.toString,state))
      }
    }
    if (domain.getObjectClass("observer") != null) {
      state.addObject(new NLObjectInstance(domain.getObjectClass("observer"),"observer",state))
    }
    if (domain.getObjectClass("patches") != null) {
      world.patches.agents.foreach {
        patch => state.addObject(new NLObjectInstance(domain.getObjectClass("patches"),patch.toString,state))
      }
    }
    state
  }
  
  def restoreFromBurlapState(context:ExtensionContext,state:NLState):Unit = {
    context.workspace.world.importWorld(VersionerErrorHandler, context.workspace, new VersionerReader(context), new BufferedReader(new StringReader(state.worldRep)))
    if(state.fixerUp != null) {
      state.fixerUp.perform(context, Array())
    }
  }
  
  
  
}

object VersionerErrorHandler extends org.nlogo.agent.Importer.ErrorHandler{

    def showError(title:String, errorDetails:String, fatalError:Boolean):Boolean = {
      Console.out.printf("Error (fatal: %s): %s, %s\nWill attempt to continue? %s\n",""+fatalError, title,  errorDetails,""+(!fatalError))
      true
    }
}

class VersionerReader(context:ExtensionContext) extends org.nlogo.agent.Importer.StringReader {
      override def readFromString(s:String):AnyRef = {
        try {
          return context.workspace.compiler.readFromString(s, context.workspace.world, context.workspace.getExtensionManager, context.workspace.world.program.is3D)
        } catch {
          case ex : Exception => throw new org.nlogo.agent.Importer.StringReaderException(ex.getMessage)
        }
      }
}