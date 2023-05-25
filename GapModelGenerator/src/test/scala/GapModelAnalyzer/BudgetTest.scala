/*
 * Copyright (c) 2023 Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package GapModelAnalyzer

import GapGraphAlgebraDefs.GapModelAlgebra.{mapAppBudget, targetAppScore}
import GapModelAnalyzer.Budget.{MalAppBudget, TargetAppScore}
import Randomizer.SupplierOfRandomness
import Utilz.ConfigReader.getConfigEntry
import Utilz.CreateLogger
import Utilz.SPSConstants.*

import scala.jdk.CollectionConverters.*
import com.google.common.graph.{MutableValueGraph, ValueGraphBuilder}
import org.mockito.Mockito.{mock, when}
import org.scalatest.PrivateMethodTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar
import org.slf4j.Logger

class BudgetTest extends AnyFlatSpec with Matchers with MockitoSugar with PrivateMethodTester {
  val logger: Logger = CreateLogger(this.getClass)

  behavior of "cost/reward computation types"

//  malAppBudget: 110.0, costOfDetection: 0.5, serviceReward: 1.5, servicePenalty: 2.5, targetAppScore: 200.0, targetAppPenalty: 0.3
  it should "create a malapp budget and take a cost of one and two steps" in {
    val malappbudget = MalAppBudget(mapAppBudget)
    malappbudget.cost(1) shouldEqual 109.5d
    malappbudget.cost(2) shouldEqual 109d
  }

  it should "create a malapp budget and take a cost and a reward" in {
    val malappbudget = MalAppBudget(mapAppBudget)
//    110-0.5+1.5 => 111
    malappbudget.cost(1).reward(1.0) shouldEqual 111d
    malappbudget.cost(1).reward(3.0) shouldEqual 114d
  }

  it should "create a malapp budget and take a cost and a reward and penalty" in {
    val malappbudget = MalAppBudget(mapAppBudget)
    //    110-0.5+1.5*2-2.5*1 => 110
    malappbudget.cost(1).reward(2.0).penalty(1.0) shouldEqual 110d
  }

  it should "create a target app score and take three penalties" in {
    val tappScore = TargetAppScore(targetAppScore)
//    200-0.3 => 199.7
    tappScore.penalty.penalty.penalty.toDouble - 199.1d should be < 0.01d
  }
}
