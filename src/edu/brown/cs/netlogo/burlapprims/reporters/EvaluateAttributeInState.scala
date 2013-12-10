package edu.brown.cs.netlogo.burlapprims.reporters
import org.nlogo.api.Syntax
import org.nlogo.api.Context
import org.nlogo.api.DefaultReporter
import org.nlogo.api.Argument
import org.nlogo.api.ExtensionException
import org.nlogo.nvm.ExtensionContext
import burlap.oomdp.core._
import burlap.oomdp.singleagent._
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import java.io.{StringWriter,PrintWriter}
import edu.brown.cs.netlogo.BURLAPExtension
import edu.brown.cs.netlogo.NLState

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