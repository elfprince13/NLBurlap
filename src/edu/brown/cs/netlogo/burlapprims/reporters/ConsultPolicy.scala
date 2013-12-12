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
import edu.brown.cs.netlogo._
import burlap.behavior.singleagent.Policy

class ConsultPolicy(ext:BURLAPExtension, cmdName:String) extends DefaultReporter {
  override def getSyntax:Syntax = {
    Syntax.reporterSyntax(Array[Int](Syntax.StringType,Syntax.WildcardType,Syntax.WildcardType,Syntax.BooleanType),Syntax.WildcardType)
  }
  
  override def getAgentClassString():String = { "OTPL" }

  override def report(args:Array[Argument], context:Context) = {
    var outState:NLState = null
    try{
      ext.contextStack.push(context.asInstanceOf[ExtensionContext])
      val world = context.asInstanceOf[ExtensionContext].workspace.world 
      val domainName = args(0).getString
      val domain = ext.domainMap(domainName)
      if(domain != null){
        val policy = args(1).get.asInstanceOf[Policy]
        val state = args(2).get.asInstanceOf[NLState]     
        
        val assumeCurrent = args(3).getBooleanValue
        if(!assumeCurrent){
          ext.versioner.restoreFromBurlapState(context.asInstanceOf[ExtensionContext],state)
        }
        
        val cleanState = state.copyStatic
        val hypotheticalGA = policy.getAction(cleanState)
        //ext.versioner.setChainActions(true)
        hypotheticalGA.executeIn(cleanState)
        //ext.versioner.setChainActions(false)
        outState = new NLState(cleanState)
        
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
    outState
  }
}