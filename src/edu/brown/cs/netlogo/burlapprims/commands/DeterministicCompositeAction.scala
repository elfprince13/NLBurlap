package edu.brown.cs.netlogo.burlapprims.commands
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
import edu.brown.cs.netlogo._

class DeterministicCompositeAction(ext:BURLAPExtension, cmdName:String) extends DefaultCommand {
  override def getSyntax():Syntax = {
    	Syntax.commandSyntax(Array[Int](Syntax.StringType,Syntax.StringType,Syntax.ListType,Syntax.ListType,Syntax.ListType,Syntax.ListType))
    }

    override def getAgentClassString():String = { "OTPL" }

    override def perform(args:Array[Argument], context:Context) = {
      try{
        ext.contextStack.push(context.asInstanceOf[ExtensionContext])
        val program = context.asInstanceOf[ExtensionContext].workspace.world.program 
        val domainName = args(0).getString
        val actionName = args(1).getString
        val actorsList = args(2).getList
        val tasksList = args(3).getList
        val argsKindList = args(4).getList
        val argsList = args(5).getList
        val domain = ext.domainMap(domainName)
        
        val actorsListStr = actorsList.toList.map(s => s.toString).toList
        val argsListStr = argsList.toList.map(il => il.asInstanceOf[LogoList].toList.map(s => s.toString).toList).toList
        val argsKindListStr = argsKindList.toList.map(s => s.toString).toList
        
        val actionsList = ((tasksList.toList).zip(argsKindListStr)).zip(actorsListStr.zipWithIndex).map{tri => 
          val counter = tri._2._2
          val actor = tri._2._1
          val task = tri._1._1.asInstanceOf[CommandTask]
          val paramKinds = tri._1._2.toUpperCase()
          new NLAction(ext,domain,"%s %s-%d".format(actor,actionName,counter),task,
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