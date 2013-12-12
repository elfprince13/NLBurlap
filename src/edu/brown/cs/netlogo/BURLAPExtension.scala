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
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import java.io.{StringWriter,PrintWriter}
import org.nlogo.api.LogoList
import org.nlogo.nvm.CommandTask
import org.nlogo.nvm.ReporterTask
import edu.brown.cs.netlogo.burlapprims.reporters._
import edu.brown.cs.netlogo.burlapprims.commands._

object ExtensionPointer {
  var ext:BURLAPExtension = null
  def setExt(next:BURLAPExtension) {
    if (ext != null) {
      Console.err.println("Warning: two or more BURLAPExtensions have been instantiated. Something is weird here.")
    }
    ext = next
  }
}

class BURLAPExtension extends DefaultClassManager {

  var domainMap = Map[String,Domain]()
  var rewardMap = Map[String,RewardFunction]()
  var terminalMap = Map[String,TerminalFunction]()
  var contextStack = new scala.collection.mutable.Stack[ExtensionContext]()
  val versioner = new WorldVersioner(30000000)
  ExtensionPointer.setExt(this)
  def load(primitiveManager: PrimitiveManager): Unit = {
		primitiveManager.addPrimitive("create-domain", new CreateSADomain(this,"create-domain"))
		primitiveManager.addPrimitive("destroy-domain", new DestroySADomain(this,"destroy-domain"))
		primitiveManager.addPrimitive("set-name-dependence", new SetNameDependentDomain(this,"set-name-dependence"))
		primitiveManager.addPrimitive("expose-agent-class", new MakeBURLAPClassForBreed(this,"expose-agent-class"))
		primitiveManager.addPrimitive("expose-own-attr", new ExposeOwn(this,"expose-own-attr"))
		primitiveManager.addPrimitive("expose-shared-attr", new ExposeExisting(this,"shared-attr"))
		primitiveManager.addPrimitive("capture-current-state",new CaptureCurrentState(this,"capture-current-state"))
		primitiveManager.addPrimitive("return-to-state",new ReturnToState(this,"return-to-state"))
		
		primitiveManager.addPrimitive("add-deterministic-composite-action",new DeterministicCompositeAction(this,"add-deterministic-composite-action"))
		primitiveManager.addPrimitive("set-class-based-rewards",new SetClassBasedRewardFunction(this,"set-class-based-rewards"))
		primitiveManager.addPrimitive("set-terminal-function",new SetTerminalFunction(this,"set-terminal-rewards"))
		
		primitiveManager.addPrimitive("try-qlearning", new TryQLearning(this, "try-qlearning"))
		primitiveManager.addPrimitive("get-modeled-greedyq", new ModeledZealotPolicy(this, "get-modeled-greedyq"))
		primitiveManager.addPrimitive("get-modeled-factoredsolution", new ModeledZealotPolicy(this, "get-modeled-factoredsolution"))
		
		primitiveManager.addPrimitive("consult-policy", new ConsultPolicy(this, "consult-policy"))
		
		primitiveManager.addPrimitive("evaluate-attribute-in-state",new EvaluateAttributeInState(this,"evaluate-attribute-in-state"))

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
