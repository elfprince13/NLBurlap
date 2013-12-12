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
import edu.brown.cs.netlogo.NLHashFactory
import burlap.behavior.statehashing.StateHashFactory
import burlap.behavior.statehashing.DiscreteStateHashFactory
import burlap.behavior.singleagent.planning.stochastic.valueiteration.ValueIteration
import burlap.behavior.singleagent.planning.commonpolicies.GreedyQPolicy
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import burlap.behavior.singleagent.Policy

class ModeledZealotPolicy(ext:BURLAPExtension, cmdName:String) extends DefaultReporter {
  override def getSyntax():Syntax = {
    	Syntax.reporterSyntax(Array[Int](Syntax.NumberType,Syntax.NumberType,Syntax.NumberType),Syntax.WildcardType)
    }

    override def getAgentClassString():String = { "OTPL" }

    override def report(args:Array[Argument], context:Context) = {
      var outpolicy:Policy = null
      try{
        ext.contextStack.push(context.asInstanceOf[ExtensionContext])

        val numguys = args(0).getIntValue
        val minhealth = args(1).getIntValue
        val maxhealth = args(2).getIntValue
        
        
        val generator = new burlap.domain.singleagent.composite.zealots.ZealotsDomainGenerator(numguys,minhealth,maxhealth)
        val modelDomain = generator.generateDomain()
        
        if(cmdName == "get-modeled-greedy"){
          outpolicy = generator.getPolicyByVI(modelDomain, 0.95, 0.01, 1000)
        } else if(cmdName == "get-modeled-factoredsolution") {
          outpolicy = generator.getPolicyFromFactoredSolution(modelDomain, generator.getFactoredSolution(modelDomain, 0.95))
        } else {
          throw new IllegalArgumentException("unknown command name passed to ModeledZealotPolicy")
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
      outpolicy
    }
}