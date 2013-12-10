package edu.brown.cs.netlogo.burlapprims.reporters
import org.nlogo.api.Syntax
import org.nlogo.api.Context
import org.nlogo.api.DefaultReporter
import org.nlogo.api.Argument
import burlap.oomdp.core._
import burlap.oomdp.singleagent._
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import edu.brown.cs.netlogo.burlapprims.commands._
import edu.brown.cs.netlogo.BURLAPExtension

class Attributer(ext:BURLAPExtension, cmdName:String, attrT:Attribute.AttributeType) extends DefaultReporter {
   override def getSyntax():Syntax = {
     Syntax.reporterSyntax(Array[Int](),Syntax.NumberType)
   }
   
   override def getAgentClassString():String = { "OTPL" }
   
   override def report(args:Array[Argument], context:Context): java.lang.Double = {
     Double.box(attrT.toInt)
   }
   
 }
