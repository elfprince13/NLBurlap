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
import edu.brown.cs.netlogo.BURLAPExtension


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