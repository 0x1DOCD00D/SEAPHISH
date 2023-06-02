/*
 * Copyright (c) 2023 Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package GapModelAnalyzer

import GapGraphAlgebraDefs.GapModelAlgebra.{costOfDetection, servicePenalty, serviceReward, targetAppLowPenalty}

object Budget:
  opaque type MalAppBudget = Double
  opaque type TargetAppScore = Double


  object MalAppBudget:
    def apply(bgt: Double): MalAppBudget = bgt

    extension (bgt: MalAppBudget)
      def toDouble: Double = bgt
      def cost(steps: Double = 1d): MalAppBudget = MalAppBudget(bgt - costOfDetection*steps)
      def reward(v: Double): MalAppBudget = MalAppBudget(bgt + v*serviceReward)
      def penalty(v: Double): MalAppBudget = MalAppBudget(bgt - v*servicePenalty)

  object TargetAppScore:
    def apply(as: Double): TargetAppScore = as

    extension (as: TargetAppScore)
      def toDouble: Double = as
      def penalty(p: Double): TargetAppScore = TargetAppScore(as - p)