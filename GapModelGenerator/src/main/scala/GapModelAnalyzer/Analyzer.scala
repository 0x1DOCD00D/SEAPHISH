/*
 * Copyright (c) 2023 Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package GapModelAnalyzer

import GapGraphAlgebraDefs.GapModelAlgebra.{mapAppBudget, targetAppScore}
import GapGraphAlgebraDefs.{GapGraph, GapModelAlgebra, GraphPerturbationAlgebra, GuiObject}
import GapGraphAlgebraDefs.GraphPerturbationAlgebra.{ModificationRecord, ModificationRecordInverse, inverseMR}
import GapModelAnalyzer.Budget.{MalAppBudget, TargetAppScore}

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*

object Analyzer:
  def apply(graph: GapGraph, mr: ModificationRecordInverse, numberOfWalks:Int): COSTTUPLE =
    @tailrec def computeTotalCostsRewards(allWalks: List[PATHRESULT], cr: COSTTUPLE): COSTTUPLE =
      allWalks match
        case Nil => cr
        case head :: tail =>
          val ucr:COSTTUPLE = CostRewardCalculator(head, mr)(cr)
          computeTotalCostsRewards(tail, ucr)
    end computeTotalCostsRewards

    val walker = RandomWalker(graph)
    val walks: List[PATHRESULT] = walker.walk(numberOfWalks)
    computeTotalCostsRewards(walks, (MalAppBudget(mapAppBudget), TargetAppScore(targetAppScore)))

  def computeWalksStats(graph: GapGraph, walks: List[PATHRESULT]): Unit =
    walks.foreach(walk => logger.info(s"Walks: ${graph.initState.id :: walk}"))
    val stats = new WalkingStats(graph, walks)
    val sorted = stats.graphCoverage().toSeq.filter(e => e._1.isInstanceOf[GuiObject] && e._2 > 0).sortBy(_._2).map(_._1.asInstanceOf[GuiObject].id).toSet
    val all = graph.sm.nodes().asScala.map(_.id).toSet
    logger.info(s"Sorted walk 5: $sorted")
    logger.info(s"Uncovered nodes: ${all -- sorted}")
    val pe = PathsEstimator(graph)
    val estimate: List[SLICEOFCOMPONENTPIE] = pe.exploreThesePaths(walks, 3)
    estimate.flatten.filter(e => e.isInstanceOf[GuiObject]).map(e => e.asInstanceOf[GuiObject].id).toSet
    val remaining =  all -- sorted
    logger.info(s"Estimate: $estimate")

  @main def runAnalyzer(args: String*): Unit =
    logger.info("File /Users/drmark/Library/CloudStorage/OneDrive-UniversityofIllinoisChicago/Github/SeaPhish/GapModelGenerator/src/main/scala/GapModelAnalyzer/Analyzer.scala created at time 7:08 PM")
    logger.info(GapModelAlgebra.getFields.mkString(", ") )
    val graph: GapGraph = GapModelAlgebra()
    val algebra = new GraphPerturbationAlgebra(graph)
    val pms: List[GraphPerturbationAlgebra#GraphPerturbationTuple] = algebra.perturbModel(10)
    val allCosts: List[COSTTUPLE] = pms.map(pm => Analyzer(pm._1, inverseMR(pm._2), 50))
    allCosts.foreach(c => logger.info(s"Costs: $c"))

