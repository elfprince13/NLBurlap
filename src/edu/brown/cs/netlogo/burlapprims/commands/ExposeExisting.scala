package edu.brown.cs.netlogo.burlapprims.commands
import org.nlogo.api.Syntax
import org.nlogo.api.Context
import org.nlogo.api.DefaultCommand
import org.nlogo.api.Argument
import org.nlogo.api.ExtensionException
import org.nlogo.nvm.ExtensionContext
import burlap.oomdp.core._
import burlap.oomdp.singleagent._
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import java.io.{StringWriter,PrintWriter}
import edu.brown.cs.netlogo.burlapprims.commands._
import edu.brown.cs.netlogo.burlapprims.reporters._

import edu.brown.cs.netlogo.BURLAPExtension
import edu.brown.cs.netlogo.NLAttribute

class ExposeExisting(ext:BURLAPExtension, cmdName:String) extends DefaultCommand {
  override def getSyntax():Syntax = {
    	Syntax.commandSyntax(Array[Int](Syntax.StringType,Syntax.StringType,Syntax.StringType))
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
        
        val domain = ext.domainMap(domainName)
        val nclass = if (domain.getObjectClass(breedName) != null ) {
          domain.getObjectClass(breedName) 
        } else {
          throw new IllegalArgumentException("Error in %s: argument must be a breed name, a linkBreed name, 'observer', or 'patches'".format(cmdName))
        }
        
        val nattr = domain.getAttribute(attrName)
        if(nattr != null){
        	nclass.addAttribute(nattr)
        } else {
          throw new IllegalArgumentException("Cannot assign a non-existent attribute name")
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
