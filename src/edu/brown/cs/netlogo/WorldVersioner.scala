package edu.brown.cs.netlogo

import org.nlogo.agent.World
import org.nlogo.api.Context
import org.nlogo.api.CommandTask
import org.nlogo.api.ReporterTask
import org.nlogo.agent.AgentSet
import org.nlogo.api.{ExtensionException,LogoException}
import org.nlogo.nvm.ExtensionContext
import org.nlogo.nvm.{CompilerInterface,FileManager}
import burlap.oomdp.core.Domain
import burlap.oomdp.core.ObjectInstance
import scala.collection._
import scala.collection.JavaConversions._
import java.io.{StringWriter,StringReader,PrintWriter,BufferedReader}
import burlap.oomdp.core.State
import org.nlogo.agent.Agent
import org.nlogo.agent.Turtle
import org.nlogo.agent.Observer
import org.nlogo.agent.Patch
import org.nlogo.agent.Link
import org.nlogo.agent.ArrayAgentSet


class WorldVersioner(bufferSize:Int) {

  val sw = new StringWriter(bufferSize)
  val pw = new PrintWriter(sw)
  
  var lastId = -1
  var cached = false
  
  private var streamingTo:State = null
  
  /*private var ignoreVersioningRequests = false
  
  def setChainActions(chainActions:Boolean) = {
    ignoreVersioningRequests = chainActions
  }*/
  
  def cloneWorld(oldWorld:World):String = {
    streamingTo = null
    sw.getBuffer().setLength(0)
    oldWorld.exportWorld(pw, true)
    sw.toString()
    
  }
  
  def makeContextForAgent(oldNvmContext:org.nlogo.nvm.Context, agent:Agent):org.nlogo.nvm.Context ={
    val innerAgentContext = new org.nlogo.nvm.Context(oldNvmContext,agent)
    innerAgentContext.agent = agent
    innerAgentContext.myself = agent
    innerAgentContext.agentBit = agent.getAgentBit
    innerAgentContext
  }
  
  def prepForStreamingStates(s:State) = { streamingTo = s }
  def isStreaming = { streamingTo != null }
  
  def getAgentFromString(world:World, agentName:String, inAgentClass:String = "") = {
    val nameComponents = agentName.split(" ")
    val agentClass = if (inAgentClass == ""){
      if(nameComponents(0) == "observer" || nameComponents(0) == "patches") {
        nameComponents(0)
      } else {
        nameComponents(0).toUpperCase
      }
    } else {
      inAgentClass
    }
    if(agentClass == "observer") {
      world.observer
    } else if (agentClass == "patches") {
      world.fastGetPatchAt(nameComponents(1).toInt, nameComponents(2).toInt)
    } else if (world.program.breedsSingular.get(agentClass) != null) {
      world.getTurtle(nameComponents(1).toLong)
    } else if (world.program.linkBreedsSingular.get(agentClass) != null) {
      world.linkManager.findLink(world.getTurtle(nameComponents(1).toLong), world.getTurtle(nameComponents(2).toLong), world.program.linkBreeds.get(world.program.linkBreedsSingular.get(agentClass)).asInstanceOf[AgentSet], false)
    } else {
      throw new IllegalArgumentException("agentName must be a valid agent belonging to a valid agentClass")
    }
  }
  
  def findCommonAncestor(ica:Class[_ <: Agent], icb:Class[_ <: Agent]):Class[_ <: Agent] = {
    if (ica.isInstance(icb)){
      icb
    } else if (icb.isInstance(ica)){
      ica 
    } else if (icb.isInstance(classOf[Turtle]) || icb.isInstance(classOf[Turtle])) {
      classOf[Turtle]
    } else if (icb.isInstance(classOf[Link]) || icb.isInstance(classOf[Link])) {
      classOf[Link]
    } else if (icb.isInstance(classOf[Patch]) || icb.isInstance(classOf[Patch])) {
      classOf[Patch]
    } else if (icb.isInstance(classOf[Observer]) || icb.isInstance(classOf[Observer])) {
      classOf[Observer]
    } else {
      classOf[Agent]
    }
  }
  
  def getAgentSetFromStrings(world:World, agentNames:Set[String]):AgentSet = {
    val agents = agentNames.map(an => getAgentFromString(world, an))
    var parentClass = agents.head.getAgentClass
    agents.foreach(agent => parentClass = findCommonAncestor(parentClass,agent.getAgentClass))
    new ArrayAgentSet(parentClass, agents.toArray, world)
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
    //if(!ignoreVersioningRequests) {
      streamingTo = null
      if(!cached || lastId != state.hashCode){
        context.workspace.world.importWorld(VersionerErrorHandler, context.workspace, new VersionerReader(context), new BufferedReader(new StringReader(state.worldRep)))
        if(state.fixerUp != null) {
          doCommands(state.fixerUp,context, Array())
        }
        cached = true
        lastId = state.hashCode
      }
    //}
  }
  
  // Assume commands are side-effectful
  def doCommands(commands:CommandTask,context:ExtensionContext, args:Array[AnyRef]):Unit = {
    cached = false
    lastId = -1
    commands.perform(context,args)
  }
  
  // Assume reporters are side-effect free! This is important, otherwise caching can't work.
  // Burden is on the end-user contract
  def getReport(reporter:ReporterTask,context:ExtensionContext, args:Array[AnyRef]) = {
    reporter.report(context,args)
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