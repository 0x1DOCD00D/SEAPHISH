/*
 * Copyright (c) 2023 Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package GapModelAnalyzer

import GapGraphAlgebraDefs.GraphPerturbationAlgebra.{ModificationRecord, ModificationRecordInverse}
import GapModelAnalyzer.Budget.RemainingBudget
import cats.data.State

type BUDGETSTATE = State[GRAPHSTATE, RemainingBudget]
type CostRewardFunction = (PATHRESULT, ModificationRecordInverse, RemainingBudget) => RemainingBudget

object CostRewardCalculator extends CostRewardFunction:
  override def apply(v1: PATHRESULT, v2: ModificationRecordInverse, v3: RemainingBudget): RemainingBudget = ???
/*
  def detectedModifications(mr: ModificationRecordInverse): Double
  def costOfServiceProvider: Double
  def serviceReward: Double*/
