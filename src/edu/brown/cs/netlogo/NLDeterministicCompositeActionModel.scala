package edu.brown.cs.netlogo

import burlap.oomdp.singleagent.classbased.CompositeActionModel
import burlap.oomdp.core.{State,TransitionProbability}
import burlap.oomdp.singleagent.GroundedAction

import scala.collection._
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

class NLDeterministicCompositeActionModel extends CompositeActionModel {

  def transitionProbsFor(s: State, ja: java.util.List[GroundedAction]): java.util.List[TransitionProbability] = deterministicTransitionProbsFor(s, ja).toList

  protected  def actionHelper(s: State, ja: java.util.List[GroundedAction]): State = { ja.foldLeft(s)((is,ija) => ija.executeIn(is)) }

}