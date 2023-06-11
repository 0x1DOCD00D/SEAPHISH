/*
 * Copyright (c) 2023 Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package GapModelAnalyzer

import GapGraphAlgebraDefs.GapModelAlgebra.{dopplegangers, mapAppBudget, targetAppScore}
import GapGraphAlgebraDefs.{GapGraph, GapGraphComponent, GapModelAlgebra, GraphPerturbationAlgebra, GuiObject}
import GapGraphAlgebraDefs.GraphPerturbationAlgebra.{ModificationRecord, ModificationRecordInverse, inverseMR}
import GapModelAnalyzer.Budget.{MalAppBudget, TargetAppScore}

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*

object Analyzer:
  def apply(graph: GapGraph, mr: ModificationRecordInverse, numberOfWalks:Int, appName: String): (COSTTUPLE, DetectedModifiedComponents) =
    @tailrec def computeTotalCostsRewards(allWalks: List[PATHRESULT], cr: COSTTUPLE, dc: DetectedModifiedComponents): (COSTTUPLE, DetectedModifiedComponents) =
      allWalks match
        case Nil => (cr,dc)
        case head :: tail =>
          val ucr:(COSTTUPLE, DetectedModifiedComponents) = CostRewardCalculator(head, mr, dc)(cr)
          computeTotalCostsRewards(tail, ucr._1, ucr._2)
    end computeTotalCostsRewards

    logger.info(s"Analyzing the model of the $appName app: ${mr.size} modifications out of which ${mr.keySet.count(_.isInstanceOf[GuiObject])} are GUI objects")
    val walker = RandomWalker(graph)
    val walks: List[PATHRESULT] = walker.walk(numberOfWalks)
    computeTotalCostsRewards(walks, (MalAppBudget(0.0d), TargetAppScore(0.0d)), List())
  end apply

  def computeWalksStats(graph: GapGraph, walks: List[PATHRESULT]): Unit =
    walks.foreach(walk => logger.info(s"Walks: ${graph.initState.id :: walk}"))
    val stats = new WalkingStats(graph, walks)
    val sorted = stats.graphCoverage().toSeq.filter(e => e._1.isInstanceOf[GuiObject] && e._2 > 0).sortBy(_._2).map(_._1.asInstanceOf[GuiObject].id).toSet
    val all = graph.sm.nodes().asScala.map(_.id).toSet
    logger.info(s"Sorted walks: $sorted")
    logger.info(s"Uncovered nodes: ${all -- sorted}")
    val pe = PathsEstimator(graph)
    val estimate: List[SLICEOFCOMPONENTPIE] = pe.exploreThesePaths(walks, 3)
    estimate.flatten.filter(e => e.isInstanceOf[GuiObject]).map(e => e.asInstanceOf[GuiObject].id).toSet
    val remaining =  all -- sorted
    logger.info(s"Estimate: $estimate")

  @main def runAnalyzer(args: String*): Unit =
    import com.google.common.graph.{Graphs, MutableValueGraph, ValueGraphBuilder}
    def debug(g: GapGraph, g1:GapGraph):Unit =
      val distances = g.distances().toSeq.filter(_._2 < Double.PositiveInfinity).sortBy(_._2).toMap
      val distances1 = g1.distances().toSeq.filter(_._2 < Double.PositiveInfinity).sortBy(_._2).toMap
      if distances != distances1 then logger.error(s"Distances are different: ${distances.toSet -- distances1.toSet}")
      val (minDistance, maxDistance) = (distances.minBy(_._2)._2, distances.maxBy(_._2)._2)
      val (minDistance1, maxDistance1) = (distances.minBy(_._2)._2, distances.maxBy(_._2)._2)
      if minDistance != minDistance1 then logger.error(s"Min distances are different: $minDistance and $minDistance1")
      if maxDistance != maxDistance1 then logger.error(s"Max distances are different: $maxDistance and $maxDistance1")
      if maxDistance * GapModelAlgebra.distanceCoeff != maxDistance1 * GapModelAlgebra.distanceCoeff then logger.error(s"perturb distances are different")
    end debug

    logger.info(GapModelAlgebra.getFields.mkString(", ") )
    val graph: GapGraph = GapModelAlgebra()
    logger.info("Perturbing target app model")
    val tappModel: GraphPerturbationAlgebra#GraphPerturbationTuple = GraphPerturbationAlgebra(graph.copy, true)

    val pms: List[GraphPerturbationAlgebra#GraphPerturbationTuple] = (1 to dopplegangers).toList.map(num =>
      logger.info(s"Perturbing doppleganger app model $num")
      val res = GraphPerturbationAlgebra(graph.copy)
      if graph != res._1 then logger.info(s"Models are different for $num") else logger.info(s"Models are the same for $num")
      res
    )
    logger.info("Successfully created perturbed models")
    val tappCosts: (COSTTUPLE, DetectedModifiedComponents) = Analyzer(tappModel._1, inverseMR(tappModel._2), GapModelAlgebra.numberOfWalks, "Target")
    val allCosts: List[(COSTTUPLE, DetectedModifiedComponents)] = pms.zipWithIndex.map {
      case (pm, index) => Analyzer(pm._1, inverseMR(pm._2), GapModelAlgebra.numberOfWalks, s"Doppleganger_$index")
    }
    logger.info(s"Target app costs: $tappCosts with the ratio ${tappCosts._1._2.toDouble/tappCosts._1._1.toDouble}")
    logger.info(s"Cost/score normalization between zero and one")
    val listOfCosts: List[COSTTUPLE] = allCosts.map(_._1)
    val maxCost: Double = listOfCosts.map(_._1.toDouble).max
    val maxScore: Double = listOfCosts.map(_._2.toDouble).max
    val normalizedCosts: List[(Double,Double, Double, Double)] = listOfCosts.map(c => (c._1.toDouble/maxCost, c._2.toDouble/maxScore, c._1.toDouble, c._2.toDouble))

    normalizedCosts.zipWithIndex.foreach(
      (c, index) => logger.info(f"Doppleganger app $index costs: ${c._1}%1.3f with the score ${c._2}%1.3f, abs values (${c._3}%1.3f, ${c._4}%1.3f) and its harmonic score ${2 * c._2 * c._1 / (c._2 + c._1)}%1.3f and its geometric score ${scala.math.sqrt(c._2 * c._1)}%1.3f")
    )

    val appScores: List[(Double, Int)] = normalizedCosts.zipWithIndex.map((c, index)=>(scala.math.sqrt(c._2*c._1), index)).sortBy(_._1)
    appScores.foreach(
      (c,index) => logger.info(f"Doppleganger app $index scores $c%1.3f")
    )
    logger.info(f"The winner is app ${appScores.head._2} with the score ${appScores.head._1}%1.3f")
