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
import Utilz.CreateLogger
import scala.collection.immutable.Nil

trait TerminationPolicy
case object UntilCycleIsFound extends TerminationPolicy
case object MaxPathLengthIsReached extends TerminationPolicy
case object AllTerminationConditions extends TerminationPolicy

type STEPRESULT = Tuple2[GapGraphComponent, GapGraphComponent]
type PATHRESULT = List[STEPRESULT]
type GRAPHSTATE = GuiObject
type WALKSTATE = State[GRAPHSTATE, PATHRESULT]
val logger = CreateLogger(classOf[RandomWalker])
class RandomWalker(private val gg: GapGraph, private val condition: TerminationPolicy) {
  require(gg != null, "GapGraph cannot be null")
  val maxWalkPathLength:Int = scala.math.floor(maxWalkPathLengthCoeff * gg.sm.nodes().size).toInt
  logger.info(s"Max walk path length: $maxWalkPathLength and the termination condition is $condition")

  private def step(node: GRAPHSTATE): Option[STEPRESULT] =
    require(node != null, "Gap object cannot be null")
    gg.getRandomConnectedNode(node)

  private def initialState: WALKSTATE = State { state =>
    (state, List[STEPRESULT]())
  }
//  this function makes one step of the random walk where the state is the current node and the path is the list of steps
  private def makeOneStep(path: PATHRESULT, i: Int): WALKSTATE = State { state =>
    if path.nonEmpty && path.headOption.get._1 == TerminalNode then (state, path)
    else step(state) match {
      case Some((node:GuiObject, action:Action)) =>
        (node, (node, action) :: path)
      case None => (state, (TerminalNode, TerminalAction)::path)
      case _ => (state, (TerminalNode, TerminalAction)::path)
    }
  }

//  this function checks for termination conditions of the random walk
  private def check4Termination(currValue:PATHRESULT): PATHRESULT =
    def check4Cycle: PATHRESULT =
      if currValue.groupBy(x => x._1).filter(x => x._2.size > 1).keys.toList.nonEmpty then (TerminalNode, TerminalAction)::currValue.tail else currValue
    end check4Cycle

    if currValue.nonEmpty && currValue.headOption.get._1 == TerminalNode then currValue
    else
      condition match
        case UntilCycleIsFound => check4Cycle
        case MaxPathLengthIsReached =>
          if currValue.size <= maxWalkPathLength then currValue else (TerminalNode, TerminalAction)::currValue
        case AllTerminationConditions => if currValue.size > maxWalkPathLength then (TerminalNode, TerminalAction)::currValue else check4Cycle
  end check4Termination


//  this is the main driver of the random walk
  def WalkTheWalk(currState: WALKSTATE, i:Int): WALKSTATE =
    currState map check4Termination flatMap { step =>
      if step.isEmpty || (step.nonEmpty && step.headOption.get._1 != TerminalNode) then
        WalkTheWalk(makeOneStep(step, i), i+1)
      else
        State.pure[GRAPHSTATE, PATHRESULT](step)
    }
  end WalkTheWalk

  def walk(howManyWalks:Int = 1): List[PATHRESULT] =
    require(howManyWalks > 0, "Number of walks must be positive")
    def trimPath(walkOne:PATHRESULT): PATHRESULT =
      if walkOne.nonEmpty && walkOne.head._1 == TerminalNode then walkOne.tail.reverse else walkOne.reverse
    end trimPath

    val result: List[PATHRESULT] = (1 to howManyWalks).map(_ => WalkTheWalk(initialState,0).run(gg.initState).value(1)).toList
    result.map(trimPath)
}

object RandomWalker:
  /*
    val MAXPATHLENGTHTC:String = "maxpathlength"
    val UNTILCYCLETC:String = "untilcycle"
    val ALLTC:String = "all"
  * */
  def apply(gg: GapGraph, schwarz: Option[String] = None): RandomWalker =
    require(gg != null, "GapGraph cannot be null")
    val partialFunction4TerminationPolicy: PartialFunction[String, TerminationPolicy] = {
      case UNTILCYCLETC => UntilCycleIsFound
      case MAXPATHLENGTHTC => MaxPathLengthIsReached
      case ALLTC => AllTerminationConditions
    }
    val terminationPolicy:TerminationPolicy = if schwarz.isDefined then partialFunction4TerminationPolicy(schwarz.get.trim.toLowerCase)
    else partialFunction4TerminationPolicy(graphWalkTerminationPolicy.trim.toLowerCase)
    logger.info(s"Termination policy: ${terminationPolicy.toString}")
    new RandomWalker(gg, terminationPolicy)

  @main def runRandomWalker(args: String*): Unit =
    logger.info("File /Users/drmark/Library/CloudStorage/OneDrive-UniversityofIllinoisChicago/Github/SeaPhish/GapModelGenerator/src/main/scala/GapModelAnalyzer/RandomWalker.scala created at time 12:46 PM")

    val node1: GuiObject = GuiObject(id = 1, children = 5, props = 10, propValueRange = 20, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10)
    val node2: GuiObject = GuiObject(id = 2, children = 5, props = 10, propValueRange = 20, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10)
    val node3: GuiObject = GuiObject(id = 3, children = 5, props = 10, propValueRange = 20, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10)
    val edge12: Action = Action(actionType = 1, fromId = 1, toId = 2, resultingValue = Some(12), cost = 0.12)
    val edge23: Action = Action(actionType = 2, fromId = 2, toId = 3, resultingValue = Some(23), cost = 0.23)
    val lst = List((node1, edge12), (node2, edge23), (node3, TerminalAction), (TerminalNode, TerminalAction), (node2, edge23), (node1, edge23))
    logger.info(s"List of entries with cycles: ${lst.mkString(", ")}")
    val cycles = lst.groupBy(x => x._1).filter(x => x._2.size > 1).keys.toList
    logger.info(s"List of cycles: ${cycles.mkString(", ")}")