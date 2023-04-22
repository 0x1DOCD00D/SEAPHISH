/*
 * Copyright (c) 2023 Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package GapModelAnalyzer

import GapGraphAlgebraDefs.GapModelAlgebra.{ALLTC, MAXPATHLENGTHTC, UNTILCYCLETC, graphWalkTerminationPolicy, maxWalkPathLengthCoeff}
import GapGraphAlgebraDefs.{Action, GapGraph, GapGraphComponent, GuiObject, GuiStateMachine, TerminalAction, TerminalNode}
import cats.Monad
import cats.data.State
import cats.syntax.all.catsSyntaxMonad

import scala.collection.immutable.Nil

trait TerminationPolicy
case object UntilCycleIsFound extends TerminationPolicy
case object MaxPathLengthIsReached extends TerminationPolicy
case object AllTerminationConditions extends TerminationPolicy

type STEPRESULT = Tuple2[GapGraphComponent, GapGraphComponent]
type PATHRESULT = List[STEPRESULT]
type GRAPHSTATE = GuiObject
type WALKSTATE = State[GRAPHSTATE, PATHRESULT]
class RandomWalker(private val gg: GapGraph, private val condition: TerminationPolicy) {
  require(gg != null, "GapGraph cannot be null")
  val maxWalkPathLength:Int = scala.math.floor(maxWalkPathLengthCoeff * gg.sm.nodes().size).toInt
  private def step(node: GRAPHSTATE): Option[STEPRESULT] =
    require(node != null, "Gap object cannot be null")
    gg.getRandomConnectedNode(node)

//  this function makes one step of the random walk where the state is the current node and the path is the list of steps
  private def makeOneStep(path: PATHRESULT): WALKSTATE = State { state =>
    step(state) match {
      case Some((node:GuiObject, action:Action)) => (node, (node, action) :: path)
      case None => (state, (TerminalNode, TerminalAction)::path)
      case _ => (state, (TerminalNode, TerminalAction)::path)
    }
  }

//  this function checks for termination conditions of the random walk
  def check4Termination(currValue:PATHRESULT): Option[PATHRESULT] =
    condition match
      case UntilCycleIsFound =>
        if currValue.isEmpty then None
        else
          val lastNode = currValue.head._1
          val lastAction = currValue.head._2
          val pathWithoutLastNode = currValue.tail
          if pathWithoutLastNode.exists(x => x._1 == lastNode && x._2 == lastAction) then Some(currValue)
          else None
      case MaxPathLengthIsReached =>
        if currValue.size > 10 then Some(currValue)
        else None
      case AllTerminationConditions =>
        if currValue.isEmpty then None
        else
          val lastNode = currValue.head._1
          val lastAction = currValue.head._2
          val pathWithoutLastNode = currValue.tail
          if pathWithoutLastNode.exists(x => x._1 == lastNode && x._2 == lastAction) then Some(currValue)
          else if currValue.size > 10 then Some(currValue)
          else None
    Some(currValue)

//  this is the main driver of the random walk
  def WalkTheWalk(currState: WALKSTATE): WALKSTATE =
    currState map check4Termination flatMap { step =>
      if step.isDefined then WalkTheWalk(makeOneStep(step.get))
      else currState
    }
}
object RandomWalker:
  def apply(gg: GapGraph): PATHRESULT =
    val terminationPolicy:TerminationPolicy = graphWalkTerminationPolicy.trim.toLowerCase match
      case UNTILCYCLETC => UntilCycleIsFound
      case MAXPATHLENGTHTC => MaxPathLengthIsReached
      case ALLTC => AllTerminationConditions
    val rw = new RandomWalker(gg, terminationPolicy)
    val walkedPath = rw.WalkTheWalk(rw.makeOneStep(List.empty)).run(gg.initState).value
    walkedPath(1)
  @main def runRandomWalker(args: String*): Unit =
    println("File /Users/drmark/Library/CloudStorage/OneDrive-UniversityofIllinoisChicago/Github/SeaPhish/GapModelGenerator/src/main/scala/GapModelAnalyzer/RandomWalker.scala created at time 12:46 PM")