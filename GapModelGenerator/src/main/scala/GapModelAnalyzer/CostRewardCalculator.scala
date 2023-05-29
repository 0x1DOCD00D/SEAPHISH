/*
 * Copyright (c) 2023 Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package GapModelAnalyzer

import GapGraphAlgebraDefs.Action
import GapGraphAlgebraDefs.GraphPerturbationAlgebra.{ModificationRecord, ModificationRecordInverse}
import GapModelAnalyzer.Budget.*
import Randomizer.SupplierOfRandomness
import Utilz.{CreateLogger, SPSConstants}
import cats.data.State

/*
* The simulator computes random walks through the GAP graph thus simulating the behavior of the smartphone user.
* Every time a random walk is performed the simulator produces an instance of PATHRESULT that contains the
* list of nodes visited during the random walk and the list of edges traversed during the random walk.
* In addition, ModificationRecordInverse contains the list of modifications that were performed on the GAP graph
* so that it is possible for each node/edge in PATHRESULT to determine whether it was modified or not.
* While detecting some delta two key variables are updated: the target app score and the budget of the malapp.
* Each detected deviation from the "correct" GAP graph results in the decrease of target app score,
* and at the same time each step decreases the budget of the malapp. However, the PATHRESULT comes with the
* service reward that designates the usefulness of the malapp to the user. If the malapp is a true helpful app
* then it's useful fort the majority of the random walks earning high service award that compensates for the
* cost of detection.
* Service award should be proportional to the weights of the edges in a random walk. The higher the weight
* the more complexity there is to navigate the app and the more service should be provided to the user. Therefore,
* we computer the service award as the average of the weights of the edges in the random walk multiplied by the service award constant.
* This computed service award will be applied using the accept/reject mechanism of the service award probability.
* */
type COSTTUPLE = (MalAppBudget, TargetAppScore)
type CostRewardFunction = (PATHRESULT, ModificationRecordInverse) => COSTTUPLE => COSTTUPLE

object CostRewardCalculator extends CostRewardFunction:
  override def apply(v1: PATHRESULT, v2: ModificationRecordInverse): COSTTUPLE => COSTTUPLE =
    (costs:COSTTUPLE) => {
      import GapGraphAlgebraDefs.GapModelAlgebra.*
      val pathLength = v1.size.toDouble
      val avgWeight: Double = v1.map(_._2.asInstanceOf[Action].cost).sum / pathLength

      logger.info(s"Malapp budget: ${costs._1} and the target app score is ${costs._2}")
      val appScore = v1.foldLeft(costs._2)((appScore, entry) =>
        val as1 = if v2.contains(entry._1) then appScore.penalty(v2(entry._1)) else appScore
        val as2 = if v2.contains(entry._2) then appScore.penalty(v2(entry._2)) else appScore
        logger.info(s"Cost reward calculator detected modification: ${entry.toString()} and applied penalty resulting in the app score $as2")
        as2
      )
      val mab:MalAppBudget = costs._1.cost(pathLength)
      logger.info(s"Malappbudget: $mab changed from ${costs._1}, target app score: $appScore changed from ${costs._2}, the ration is ${mab.toDouble/appScore.toDouble}")
      (if SupplierOfRandomness.`YesOrNo?`(serviceRewardProbability) then mab.reward(avgWeight) else mab.penalty(avgWeight), appScore)
    }

  @main def runCostRewardCalculator(args: String*): Unit =
    import GapGraphAlgebraDefs.GapModelAlgebra.*
    val logger = CreateLogger(classOf[CostRewardCalculator.type])
    logger.info("File /Users/drmark/Library/CloudStorage/OneDrive-UniversityofIllinoisChicago/Github/SeaPhish/GapModelGenerator/src/main/scala/GapModelAnalyzer/CostRewardCalculator.scala created at time 7:06 PM")
    logger.info(s"malAppBudget: $mapAppBudget, costOfDetection: $costOfDetection, serviceReward: $serviceReward, servicePenalty: $servicePenalty, targetAppScore: $targetAppScore, " +
      s"targetAppPenalty: $targetAppLowPenalty, targetAppHighPenalty: $targetAppHighPenalty, serviceRewardProbability: $serviceRewardProbability")
