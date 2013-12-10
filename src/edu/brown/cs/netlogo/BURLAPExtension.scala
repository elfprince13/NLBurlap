package edu.brown.cs.netlogo

import java.util.List
import scala.collection.Map
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
import org.nlogo.nvm.FileManager
import org.nlogo.nvm.Workspace
import org.nlogo.shape.editor.ManagerDialog
import burlap.oomdp.core._
import burlap.oomdp.singleagent._
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import java.io.{StringWriter,PrintWriter}
import org.nlogo.api.LogoList
import org.nlogo.nvm.CommandTask
import org.nlogo.nvm.ReporterTask

object ExtensionPointer {
  var ext:BURLAPExtension = null
  def setExt(next:BURLAPExtension) {
    if (ext != null) {
      Console.err.println("Warning: two or more BURLAPExtensions have been instantiated. Something is weird here.")
    }
    ext = next
  }
}

class BURLAPExtension extends DefaultClassManager {

  var domainMap = Map[String,Domain]()
  var rewardMap = Map[String,RewardFunction]()
  var terminalMap = Map[String,TerminalFunction]()
  var contextStack = new scala.collection.mutable.Stack[ExtensionContext]()
  val versioner = new WorldVersioner(30000000)
  ExtensionPointer.setExt(this)
  def load(primitiveManager: PrimitiveManager): Unit = {
		primitiveManager.addPrimitive("create-domain", new CreateSADomain(this,"create-domain"))
		primitiveManager.addPrimitive("destroy-domain", new DestroySADomain(this,"destroy-domain"))
		primitiveManager.addPrimitive("set-name-dependence", new SetNameDependentDomain(this,"set-name-dependence"))
		primitiveManager.addPrimitive("expose-agent-class", new MakeBURLAPClassForBreed(this,"expose-agent-class"))
		primitiveManager.addPrimitive("expose-own-attr", new ExposeOwn(this,"expose-own-attr"))
		primitiveManager.addPrimitive("capture-current-state",new CaptureCurrentState(this,"capture-current-state"))
		primitiveManager.addPrimitive("return-to-state",new ReturnToState(this,"return-to-state"))
		
		primitiveManager.addPrimitive("add-deterministic-composite-action",new DeterministicCompositeAction(this,"add-deterministic-composite-action"))
		primitiveManager.addPrimitive("set-class-based-rewards",new SetClassBasedRewardFunction(this,"set-class-based-rewards"))
		primitiveManager.addPrimitive("set-terminal-function",new SetTerminalFunction(this,"set-terminal-rewards"))
		
		primitiveManager.addPrimitive("evaluate-attribute-in-state",new EvaluateAttributeInState(this,"evaluate-attribute-in-state"))

		primitiveManager.addPrimitive("attr:NOTYPE", new Attributer(this,"attr:NOTYPE",Attribute.AttributeType.NOTYPE))
		primitiveManager.addPrimitive("attr:DISC", new Attributer(this,"attr:DISC",Attribute.AttributeType.DISC))
		primitiveManager.addPrimitive("attr:REAL", new Attributer(this,"attr:REAL",Attribute.AttributeType.REAL))
		primitiveManager.addPrimitive("attr:REALUNBOUND", new Attributer(this,"attr:REALUNBOUND",Attribute.AttributeType.REALUNBOUND))
		primitiveManager.addPrimitive("attr:RELATIONAL", new Attributer(this,"attr:RELATIONAL",Attribute.AttributeType.RELATIONAL))
		primitiveManager.addPrimitive("attr:MULTITARGETRELATIONAL", new Attributer(this,"attr:MULTITARGETRELATIONAL",Attribute.AttributeType.MULTITARGETRELATIONAL))
		
		primitiveManager.addPrimitive("set-attr-range:DISC", new SetBounds(this,"set-attr-range:DISC",Attribute.AttributeType.DISC))
		primitiveManager.addPrimitive("set-attr-range:REAL", new SetBounds(this,"set-attr-range:REAL",Attribute.AttributeType.REAL))
 
  }

}



class CreateSADomain(ext:BURLAPExtension, cmdName:String) extends DefaultCommand {
  override def getSyntax():Syntax = {
    	Syntax.commandSyntax(Array[Int](Syntax.StringType));
    }

    override def getAgentClassString():String = { "OTPL" }

    override def perform(args:Array[Argument], context:Context) = {
      try{
        ext.contextStack.push(context.asInstanceOf[ExtensionContext])
        val nd = (new SADomain())
        //val kAttr = new Attribute(nd,"SYS:KIND",Attribute.AttributeType.DISC)
        //kAttr.setDiscValues(Array[String]("O","T","P","L"))
        ext.domainMap = ext.domainMap + (args(0).getString -> nd)
      } catch {
        case e : Exception => val sw = new StringWriter
        e.printStackTrace( new PrintWriter(sw) )
        throw new ExtensionException("Error in %s: %s\n%s".format(cmdName,e.getMessage,sw.toString))
      } finally {
        if(ext.contextStack.nonEmpty && ext.contextStack.top == context) {
          ext.contextStack.pop
        }
      }
      
    }
}

class DestroySADomain(ext:BURLAPExtension, cmdName:String) extends DefaultCommand {
  override def getSyntax():Syntax = {
    	Syntax.commandSyntax(Array[Int](Syntax.StringType));
    }

    override def getAgentClassString():String = { "OTPL" }

    override def perform(args:Array[Argument], context:Context) = {
      try{
        ext.contextStack.push(context.asInstanceOf[ExtensionContext])
        ext.domainMap = ext.domainMap - (args(0).getString)
      } catch {
        case e : Exception => val sw = new StringWriter
        e.printStackTrace( new PrintWriter(sw) )
        throw new ExtensionException("Error in %s: %s\n%s".format(cmdName,e.getMessage,sw.toString))
      } finally {
        if(ext.contextStack.nonEmpty && ext.contextStack.top == context) {
          ext.contextStack.pop
        }
      }
      
    }
}

class SetNameDependentDomain(ext:BURLAPExtension, cmdName:String) extends DefaultCommand {
  override def getSyntax():Syntax = {
    	Syntax.commandSyntax(Array[Int](Syntax.StringType,Syntax.BooleanType));
    }

    override def getAgentClassString():String = { "OTPL" }

    override def perform(args:Array[Argument], context:Context) = {
      try{
        ext.contextStack.push(context.asInstanceOf[ExtensionContext])
        ext.domainMap(args(0).getString).setNameDependence(args(1).getBooleanValue)
      } catch {
        case e : Exception => val sw = new StringWriter
        e.printStackTrace( new PrintWriter(sw) )
        throw new ExtensionException("Error in %s: %s\n%s".format(cmdName,e.getMessage,sw.toString))
      } finally {
        if(ext.contextStack.nonEmpty && ext.contextStack.top == context) {
          ext.contextStack.pop
        }
      }
      
    }
}

class MakeBURLAPClassForBreed(ext:BURLAPExtension, cmdName:String) extends DefaultCommand {
  override def getSyntax():Syntax = {
    	Syntax.commandSyntax(Array[Int](Syntax.StringType,Syntax.StringType,Syntax.BooleanType));
    }

    override def getAgentClassString():String = { "OTPL" }

    override def perform(args:Array[Argument], context:Context) = {
      try{
        ext.contextStack.push(context.asInstanceOf[ExtensionContext])
        val breeds = context.asInstanceOf[ExtensionContext].workspace.world.program.breedsSingular
        val linkBreeds = context.asInstanceOf[ExtensionContext].workspace.world.program.linkBreedsSingular
        val domainName = args(0).getString
        val breedName = args(1).getString.toUpperCase
        val hidden = args(2).getBooleanValue
        val domain = ext.domainMap(domainName)
        val nclass = if (breedName == "patches" || breedName == "observer" || 
            breeds.get(breedName) != null || linkBreeds.get(breedName) != null ) {
          new ObjectClass(domain,breedName) 
        } else {
          throw new IllegalArgumentException("Error in %s: argument must be a breed name, a linkBreed name, 'observer', or 'patches'".format(cmdName))
        }
        nclass.hidden = hidden
      } catch {
        case e : Exception => val sw = new StringWriter
        e.printStackTrace( new PrintWriter(sw) )
        throw new ExtensionException("Error in %s: %s\n%s".format(cmdName,e.getMessage,sw.toString))
      } finally {
        if(ext.contextStack.nonEmpty && ext.contextStack.top == context) {
          ext.contextStack.pop
        }
      }
    }  
}

class ExposeOwn(ext:BURLAPExtension, cmdName:String) extends DefaultCommand {
  override def getSyntax():Syntax = {
    	Syntax.commandSyntax(Array[Int](Syntax.StringType,Syntax.StringType,Syntax.StringType,Syntax.ReporterTaskType,Syntax.CommandTaskType,Syntax.NumberType))
    }

    override def getAgentClassString():String = { "OTPL" }

    override def perform(args:Array[Argument], context:Context) = {
      try{
        ext.contextStack.push(context.asInstanceOf[ExtensionContext])
        val program = context.asInstanceOf[ExtensionContext].workspace.world.program 
        val breeds = program.breedsSingular
        val linkBreeds = program.linkBreedsSingular
        val domainName = args(0).getString
        val breedName = args(1).getString.toUpperCase
        val attrName = args(2).getString
        val attrTask = args(3).getReporterTask
        val attrSetter = args(4).getCommandTask
        val kind = Attribute.AttributeType.fromInt(args(5).getIntValue)
        val domain = ext.domainMap(domainName)
        val nclass = if (domain.getObjectClass(breedName) != null ) {
          domain.getObjectClass(breedName) 
        } else {
          throw new IllegalArgumentException("Error in %s: argument must be a breed name, a linkBreed name, 'observer', or 'patches'".format(cmdName))
        }
        
        val nattr = new NLAttribute(ext,domain,attrName,attrTask,attrSetter,kind)
        nclass.addAttribute(nattr)
        
      } catch {
        case e : Exception => val sw = new StringWriter
        e.printStackTrace( new PrintWriter(sw) )
        throw new ExtensionException("Error in %s: %s\n%s".format(cmdName,e.getMessage,sw.toString))
      } finally {
        if(ext.contextStack.nonEmpty && ext.contextStack.top == context) {
          ext.contextStack.pop
        }
      }
    }
}

class DeterministicCompositeAction(ext:BURLAPExtension, cmdName:String) extends DefaultCommand {
  override def getSyntax():Syntax = {
    	Syntax.commandSyntax(Array[Int](Syntax.StringType,Syntax.StringType,Syntax.ListType,Syntax.ListType,Syntax.ListType))
    }

    override def getAgentClassString():String = { "OTPL" }

    override def perform(args:Array[Argument], context:Context) = {
      try{
        ext.contextStack.push(context.asInstanceOf[ExtensionContext])
        val program = context.asInstanceOf[ExtensionContext].workspace.world.program 
        val domainName = args(0).getString
        val actionName = args(1).getString
        val tasksList = args(2).getList
        val argsKindList = args(3).getList
        val argsList = args(4).getList
        val domain = ext.domainMap(domainName)
        
        val argsListStr = argsList.toList.map(il => il.asInstanceOf[LogoList].toList.map(s => s.toString))
        val argsKindListStr = argsKindList.toList.map(s => s.toString)
        
        val actionsList = (tasksList.toList).zip(argsKindListStr).zipWithIndex.map{tri => 
          val counter = tri._2
          val task = tri._1._1.asInstanceOf[CommandTask]
          val paramKinds = tri._1._2
          new NLAction(ext,domain,"%s-%d".format(actionName,counter),task,
              if (domain.getObjectClass(paramKinds) == null) { Array[String]() } 
              else { Array[String](paramKinds) }
          )
        }
        
        domain.addAction(new NLCompositeAction(ext, actionName, domain, actionsList, argsListStr, new NLDeterministicCompositeActionModel))
        
      } catch {
        case e : Exception => val sw = new StringWriter
        e.printStackTrace( new PrintWriter(sw) )
        throw new ExtensionException("Error in %s: %s\n%s".format(cmdName,e.getMessage,sw.toString))
      } finally {
        if(ext.contextStack.nonEmpty && ext.contextStack.top == context) {
          ext.contextStack.pop
        }
      }
    }
}

class SetClassBasedRewardFunction(ext:BURLAPExtension, cmdName:String) extends DefaultCommand {
  override def getSyntax():Syntax = {
    	Syntax.commandSyntax(Array[Int](Syntax.StringType,Syntax.ListType,Syntax.ListType))
    }

    override def getAgentClassString():String = { "OTPL" }

    override def perform(args:Array[Argument], context:Context) = {
      try{
        ext.contextStack.push(context.asInstanceOf[ExtensionContext])
        val program = context.asInstanceOf[ExtensionContext].workspace.world.program 
        val domainName = args(0).getString
        val breedsList = args(2).getList
        val reportersList = args(3).getList
        val domain = ext.domainMap(domainName)
        if (domain != null){
          ext.rewardMap = ext.rewardMap + (domainName -> new NLSAClassBasedRewardFunction(ext, Map(breedsList.toList.zip(reportersList.toList).map(p => 
            Pair(p._1.asInstanceOf[String],
                (new NLSARewardFunction(ext, domainName, p._1.asInstanceOf[String], p._2.asInstanceOf[ReporterTask])).asInstanceOf[RewardFunction])
             ) :_*)))
        }
      } catch {
        case e : Exception => val sw = new StringWriter
        e.printStackTrace( new PrintWriter(sw) )
        throw new ExtensionException("Error in %s: %s\n%s".format(cmdName,e.getMessage,sw.toString))
      } finally {
        if(ext.contextStack.nonEmpty && ext.contextStack.top == context) {
          ext.contextStack.pop
        }
      }
    }
}

class SetTerminalFunction(ext:BURLAPExtension, cmdName:String) extends DefaultCommand {
  override def getSyntax():Syntax = {
    	Syntax.commandSyntax(Array[Int](Syntax.StringType,Syntax.ListType,Syntax.ListType))
    }

    override def getAgentClassString():String = { "OTPL" }

    override def perform(args:Array[Argument], context:Context) = {
      try{
        ext.contextStack.push(context.asInstanceOf[ExtensionContext])
        val program = context.asInstanceOf[ExtensionContext].workspace.world.program 
        val domainName = args(0).getString
        val breedsList = args(2).getList
        val reportersList = args(3).getList
        val domain = ext.domainMap(domainName)
        if (domain != null){
          ext.rewardMap = ext.rewardMap + (domainName -> new NLSAClassBasedRewardFunction(ext, Map(breedsList.toList.zip(reportersList.toList).map(p => 
            Pair(p._1.asInstanceOf[String],
                (new NLSARewardFunction(ext, domainName, p._1.asInstanceOf[String], p._2.asInstanceOf[ReporterTask])).asInstanceOf[RewardFunction])
             ) :_*)))
        }
      } catch {
        case e : Exception => val sw = new StringWriter
        e.printStackTrace( new PrintWriter(sw) )
        throw new ExtensionException("Error in %s: %s\n%s".format(cmdName,e.getMessage,sw.toString))
      } finally {
        if(ext.contextStack.nonEmpty && ext.contextStack.top == context) {
          ext.contextStack.pop
        }
      }
    }
}

class CaptureCurrentState(ext:BURLAPExtension, cmdName:String) extends DefaultReporter {
  override def getSyntax():Syntax = {
    	Syntax.reporterSyntax(Array[Int](Syntax.StringType),Syntax.WildcardType)
    }

    override def getAgentClassString():String = { "OTPL" }

    override def report(args:Array[Argument], context:Context) = {
      var outstate:NLState = null
      try{
        ext.contextStack.push(context.asInstanceOf[ExtensionContext])
        val world = context.asInstanceOf[ExtensionContext].workspace.world 
        val domainName = args(0).getString
        val domain = ext.domainMap(domainName)
        outstate = ext.versioner.copyIntoState(world,domain)
      } catch {
        case e : Exception => val sw = new StringWriter
        e.printStackTrace( new PrintWriter(sw) )
        throw new ExtensionException("Error in %s: %s\n%s".format(cmdName,e.getMessage,sw.toString))
      } finally {
        if(ext.contextStack.nonEmpty && ext.contextStack.top == context) {
          ext.contextStack.pop
        }
      }
      outstate
    }
}

class EvaluateAttributeInState(ext:BURLAPExtension, cmdName:String) extends DefaultReporter {
  override def getSyntax():Syntax = {
    	Syntax.reporterSyntax(Array[Int](Syntax.WildcardType,Syntax.StringType,Syntax.StringType),Syntax.WildcardType)
    }

    override def getAgentClassString():String = { "OTPL" }

    override def report(args:Array[Argument], context:Context) = {
      var outstr:String = ""
      try{
        ext.contextStack.push(context.asInstanceOf[ExtensionContext])
        val world = context.asInstanceOf[ExtensionContext].workspace.world 
        val instate = args(0).get.asInstanceOf[NLState]
        val agentName = args(1).getString
        val attrName = args(2).getString
        val agentObj = instate.getObject(agentName)
        val attr = agentObj.getValueForAttribute(attrName)
        outstr = attr.getStringVal()
      } catch {
        case e : Exception => val sw = new StringWriter
        e.printStackTrace( new PrintWriter(sw) )
        throw new ExtensionException("Error in %s: %s\n%s".format(cmdName,e.getMessage,sw.toString))
      } finally {
        if(ext.contextStack.nonEmpty && ext.contextStack.top == context) {
          ext.contextStack.pop
        }
      }
      outstr
    }
}

class ReturnToState(ext:BURLAPExtension, cmdName:String) extends DefaultCommand {
  override def getSyntax:Syntax = {
    Syntax.commandSyntax(Array[Int](Syntax.StringType,Syntax.WildcardType))
  }
  
  override def getAgentClassString():String = { "OTPL" }

  override def perform(args:Array[Argument], context:Context) = {
    try{
      ext.contextStack.push(context.asInstanceOf[ExtensionContext])
      val world = context.asInstanceOf[ExtensionContext].workspace.world 
      val domainName = args(0).getString
      val domain = ext.domainMap(domainName)
      if(domain != null){
        val state = args(1).get.asInstanceOf[NLState]     
        ext.versioner.restoreFromBurlapState(context.asInstanceOf[ExtensionContext],state)
      } else {
        throw new IllegalArgumentException("Domain must be a valid domain");
      }
    } catch {
      case e : Exception => val sw = new StringWriter
      e.printStackTrace( new PrintWriter(sw) )
      throw new ExtensionException("Error in %s: %s\n%s".format(cmdName,e.getMessage,sw.toString))
    } finally {
      if(ext.contextStack.nonEmpty && ext.contextStack.top == context) {
        ext.contextStack.pop
      }
    }
  }
}


class Attributer(ext:BURLAPExtension, cmdName:String, attrT:Attribute.AttributeType) extends DefaultReporter {
   override def getSyntax():Syntax = {
     Syntax.reporterSyntax(Array[Int](),Syntax.NumberType)
   }
   
   override def getAgentClassString():String = { "OTPL" }
   
   override def report(args:Array[Argument], context:Context): java.lang.Double = {
     Double.box(attrT.toInt)
   }
   
 }


class SetBounds(ext:BURLAPExtension, cmdName:String, attrT:Attribute.AttributeType) extends DefaultCommand{
  override def getSyntax():Syntax = {
    Syntax.commandSyntax(Array[Int](Syntax.StringType,Syntax.StringType,Syntax.ListType))
  }
   
  override def getAgentClassString():String = { "OTPL" }
  
  override def perform(args:Array[Argument], context:Context) = {
    try{
      ext.contextStack.push(context.asInstanceOf[ExtensionContext])
      val domainName = args(0).getString
      val domain = ext.domainMap(domainName)
      val attrName = args(1).getString
      val attr = domain.getAttribute(attrName)
      val inp = args(2).getList
      if (attrT == Attribute.AttributeType.DISC) {  
        attr.setDiscValues(inp.scalaIterator.map(_.toString).toArray[String])
      } else if (attrT == Attribute.AttributeType.REAL) {
        attr.setLims(inp.get(0).asInstanceOf[Double],inp.get(1).asInstanceOf[Double])
      }
    } catch {
      case e : Exception => val sw = new StringWriter
      e.printStackTrace( new PrintWriter(sw) )
      throw new ExtensionException("Error in %s: %s\n%s".format(cmdName,e.getMessage,sw.toString))
    } finally {
      if(ext.contextStack.nonEmpty && ext.contextStack.top == context) {
        ext.contextStack.pop
      }
    }
  }
   
}
