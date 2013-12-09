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

import java.io.{StringWriter,PrintWriter}

class BURLAPExtension extends DefaultClassManager {

  var domainMap = Map[String,Domain]()
  var contextStack = new scala.collection.mutable.Stack[ExtensionContext]()
  val versioner = new WorldVersioner(30000000)
  
  def load(primitiveManager: PrimitiveManager): Unit = {
		primitiveManager.addPrimitive("create-domain", new CreateSADomain(this,"create-domain"))
		primitiveManager.addPrimitive("destroy-domain", new DestroySADomain(this,"destroy-domain"))
		primitiveManager.addPrimitive("set-name-dependence", new SetNameDependentDomain(this,"set-name-dependence"))
		primitiveManager.addPrimitive("expose-agent-class", new MakeBURLAPClassForBreed(this,"expose-agent-class"))
		primitiveManager.addPrimitive("expose-own-attr", new ExposeOwn(this,"expose-own-attr"))
		primitiveManager.addPrimitive("capture-current-state",new CaptureCurrentState(this,"capture-current-state"))
		

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
    	Syntax.commandSyntax(Array[Int](Syntax.StringType,Syntax.StringType,Syntax.StringType,Syntax.ReporterTaskType,Syntax.NumberType))
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
        val kind = Attribute.AttributeType.fromInt(args(4).getIntValue)
        val domain = ext.domainMap(domainName)
        val nclass = if (domain.getObjectClass(breedName) != null ) {
          domain.getObjectClass(breedName) 
        } else {
          throw new IllegalArgumentException("Error in %s: argument must be a breed name, a linkBreed name, 'observer', or 'patches'".format(cmdName))
        }
        
        val nattr = new NLAttribute(ext,domain,attrName,attrTask,kind)
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

class CaptureCurrentState(ext:BURLAPExtension, cmdName:String) extends DefaultReporter {
  override def getSyntax():Syntax = {
    	Syntax.reporterSyntax(Array[Int](Syntax.StringType),Syntax.WildcardType)
    }

    override def getAgentClassString():String = { "OTPL" }

    override def report(args:Array[Argument], context:Context) = {
      try{
        ext.contextStack.push(context.asInstanceOf[ExtensionContext])
        val world = context.asInstanceOf[ExtensionContext].workspace.world 
        val domainName = args(0).getString
        val domain = ext.domainMap(domainName)
        ext.versioner.copyIntoState(world,domain)
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
