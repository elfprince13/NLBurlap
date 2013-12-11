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

class TryQLearning(ext:BURLAPExtension, cmdName:String) extends DefaultReporter {
  override def getSyntax():Syntax = {
    	Syntax.reporterSyntax(Array[Int](Syntax.StringType, Syntax.NumberType, Syntax.NumberType, Syntax.NumberType, Syntax.WildcardType),Syntax.WildcardType)
    }

    override def getAgentClassString():String = { "OTPL" }

    override def report(args:Array[Argument], context:Context) = {
      var outstate:NLState = null
      try{
        ext.contextStack.push(context.asInstanceOf[ExtensionContext])
        val world = context.asInstanceOf[ExtensionContext].workspace.world 
        val domainName = args(0).getString
        
        val gamma = args(1).getDoubleValue
        val tol = args(2).getDoubleValue
        val maxIterations = args(3).getIntValue
        
        val state = args(4).get.asInstanceOf[NLState]
        val domain = ext.domainMap(domainName)
        
        val reward = ext.rewardMap(domainName)
        val terminus = ext.terminalMap(domainName)
        
        
        val shf:StateHashFactory = new DiscreteStateHashFactory(
            Map(
                domain.getObjectClasses.map(c => Pair(c.name , c.attributeList.filter(_.asInstanceOf[Attribute].`type` == Attribute.AttributeType.DISC).toList.asJava)) :_*
                ).asJava
            )
       
        val vi = new ValueIteration(domain, reward, terminus, gamma, shf, tol, maxIterations)
        
        vi.planFromState(state)
        Console.out.println("Found initial state to have value %f".format(vi.value(state)))
        
        new GreedyQPolicy(vi)
        
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