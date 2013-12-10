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