/**
 *    Copyright 2014 KU Leuven Research and Developement - iMinds - Distrinet
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 *
 *    Administrative Contact: dnet-project-office@cs.kuleuven.be
 *    Technical Contact: maarten.decat@cs.kuleuven.be
 *    Author: maarten.decat@cs.kuleuven.be
 */
package stapl.core

import stapl.core.pdp.EvaluationCtx

/**
 * *************************
 * WRAPPERS
 *
 * We separate the keywords from the implementation so that these can be overridden
 * at runtime by passing them to the evaluation context.
 */
sealed trait CombinationAlgorithm {

  def combine(policies: List[AbstractPolicy], ctx: EvaluationCtx): Result =
    ctx.getCombinationAlgorithmImplementation(this).combine(policies, ctx)
}

case object PermitOverrides extends CombinationAlgorithm
case object DenyOverrides extends CombinationAlgorithm
case object FirstApplicable extends CombinationAlgorithm

/**
 * **********************
 * IMPLEMENTATIONS
 */
trait CombinationAlgorithmImplementation {

  def combine(policies: List[AbstractPolicy], ctx: EvaluationCtx): Result = {

    var tmpResult = Result(NotApplicable)

    for (policy <- policies) {
      val Result(decision, obligationActions, _) = policy.evaluate(ctx)
      // If a subpolicy returns smth from wantedObjects: return this result with its obligations,
      //	do not evaluate the rest for other obligations.
      // TODO is this correct?
      // If all subpolicies return smth from updateTmpObjects: combine all their obligations and return
      // 	them with Deny
      // If all subpolicies return NotApplicable: return NotApplicable without obligations
      // See XACML2 specs, Section 7.14
      if (wantedObjects.contains(decision)) {
        return Result(decision, obligationActions)
      } else if (updateTmpObjects.contains(decision)) {
        tmpResult = Result(decision, tmpResult.obligationActions ::: obligationActions)
      }
    }
    tmpResult
  }

  protected val updateTmpObjects: List[Effect]
  protected val wantedObjects: List[Effect]
}

trait CombinationAlgorithmImplementationBundle {
  def PermitOverrides: CombinationAlgorithmImplementation
  def DenyOverrides: CombinationAlgorithmImplementation
  def FirstApplicable: CombinationAlgorithmImplementation
}

/**
 * The bundle of simple implementations: sequential evaluation.
 */
object SimpleCombinationAlgorithmImplementationBundle extends CombinationAlgorithmImplementationBundle {

  object PermitOverrides extends CombinationAlgorithmImplementation {

    override protected val updateTmpObjects: List[Effect] = List(Deny)
    override protected val wantedObjects: List[Effect] = List(Permit)
  }

  object DenyOverrides extends CombinationAlgorithmImplementation {

    override protected val updateTmpObjects: List[Effect] = List(Permit)
    override protected val wantedObjects: List[Effect] = List(Deny)
  }

  object FirstApplicable extends CombinationAlgorithmImplementation {

    override protected val updateTmpObjects: List[Effect] = List.empty
    override protected val wantedObjects: List[Effect] = List(Permit, Deny)
  }
}