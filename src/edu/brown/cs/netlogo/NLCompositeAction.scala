package edu.brown.cs.netlogo

import burlap.oomdp.singleagent.classbased.{CompositeAction,CompositeActionModel}
import burlap.oomdp.core.{State,Domain}
import burlap.oomdp.singleagent.Action
import burlap.oomdp.singleagent.GroundedAction
import scala.collection._
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

class NLCompositeAction(ext:BURLAPExtension, name:String, domain:Domain, subactions:List[Action], params:List[List[String]], model:CompositeActionModel) extends CompositeAction(name, domain, subactions.asJava, model) {

  domain.getActions().remove(subactions)
  
  def getAllGroundings(s: State): java.util.List[GroundedAction] = {
    val indices = Stream.continually(0).take(params.size).toList
    val maxes = params.map(l => l.size)
    val count = maxes.fold(1)(_*_)
    getNextGrounding(List[GroundedAction](),indices,maxes,count)
  }
  
  def getNextGrounding(known:List[GroundedAction],indices:List[Int],maxes:List[Int],count:Int): List[GroundedAction] = {
    if (count > 0){ getNextGrounding((new GroundedAction(this,params.zip(indices).map(p => (p._1)(p._2)).toArray)) :: known, getNextIndices(indices,maxes),maxes,count-1) }
    else { known }
  }
  
  def getNextIndices(indices:List[Int], maxes:List[Int]) = {
    var carry = false
    indices.zip(maxes).map({pair => val i = pair._1
      val m = pair._2
      if(i+1==m){
        carry = true 
      } else {
        carry = false 
      }
      (i+1)%m
    })
  }

}