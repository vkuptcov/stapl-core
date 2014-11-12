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

import java.util.Date
import stapl.core.pdp.EvaluationCtx
import scala.concurrent.Future
import concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}

abstract class Expression {
  def evaluate(implicit ctx: EvaluationCtx): Boolean
  def evaluateAsync(implicit ctx: EvaluationCtx): Future[Try[Boolean]]

  final def &(that: Expression): Expression = And(this, that)

  final def |(that: Expression): Expression = Or(this, that)

  final def unary_!(): Expression = Not(this)
}

case object AlwaysTrue extends Expression {
  override def evaluate(implicit ctx: EvaluationCtx): Boolean = true
  override def evaluateAsync(implicit ctx: EvaluationCtx): Future[Try[Boolean]] = Future successful Success(true)
}
case object AlwaysFalse extends Expression {
  override def evaluate(implicit ctx: EvaluationCtx): Boolean = false
  override def evaluateAsync(implicit ctx: EvaluationCtx): Future[Try[Boolean]] = Future successful Success(false)
}
case class GreaterThanValue(value1: Value, value2: Value) extends Expression {
  override def evaluate(implicit ctx: EvaluationCtx): Boolean = {
    val c1 = value1.getConcreteValue(ctx)
    val c2 = value2.getConcreteValue(ctx)
    c1.reprGreaterThan(c2)
  }
  override def evaluateAsync(implicit ctx: EvaluationCtx): Future[Try[Boolean]] = {
    val f1 = value1.getConcreteValueAsync(ctx)
    val f2 = value2.getConcreteValueAsync(ctx)
    for {
      o1 <- f1
      o2 <- f2
    } yield for {
      c1 <- o1
      c2 <- o2
    } yield c1.reprGreaterThan(c2)
  }
}
case class BoolExpression(attribute: SimpleAttribute) extends Expression {
  override def evaluate(implicit ctx: EvaluationCtx): Boolean = {
    val bool = attribute.getConcreteValue(ctx).representation
    bool.asInstanceOf[Boolean]
  }
  override def evaluateAsync(implicit ctx: EvaluationCtx): Future[Try[Boolean]] = {
    val f = attribute.getConcreteValueAsync(ctx)
    for {
      o <- f
    } yield for {
      bool <- o
    } yield bool.representation.asInstanceOf[Boolean]
  }
}

case class EqualsValue(value1: Value, value2: Value) extends Expression {
  override def evaluate(implicit ctx: EvaluationCtx): Boolean = {
    val c1 = value1.getConcreteValue(ctx)
    val c2 = value2.getConcreteValue(ctx)
    c1.equalRepr(c2)
  }
  override def evaluateAsync(implicit ctx: EvaluationCtx): Future[Try[Boolean]] = {
    val f1 = value1.getConcreteValueAsync(ctx)
    val f2 = value2.getConcreteValueAsync(ctx)
    for {
      o1 <- f1
      o2 <- f2
    } yield for {
      c1 <- o1
      c2 <- o2
    } yield c1.equalRepr(c2)
  }
}
case class ValueIn(value: Value, list: Value) extends Expression {
  override def evaluate(implicit ctx: EvaluationCtx): Boolean = {
    val c = value.getConcreteValue(ctx)
    val l = list.getConcreteValue(ctx)
    l.reprContains(c)
  }
  override def evaluateAsync(implicit ctx: EvaluationCtx): Future[Try[Boolean]] = {
    val f1 = value.getConcreteValueAsync(ctx)
    val f2 = list.getConcreteValueAsync(ctx)
    for {
      o1 <- f1
      o2 <- f2
    } yield for {
      c <- o1
      l <- o2
    } yield l.reprContains(c)
  }
}
case class And(expression1: Expression, expression2: Expression) extends Expression {
  override def evaluate(implicit ctx: EvaluationCtx) = expression1.evaluate && expression2.evaluate

  override def evaluateAsync(implicit ctx: EvaluationCtx) = {
    // Note: we deliberately evaluate this sequentially to guarantee *logical* and
    // (i.e., in "A & B", B is not even evaluated if A == false.
    expression1.evaluateAsync flatMap {
      _ match {
        case Success(false) =>
          // do not continue evaluation
          Future { Success(false) }
        case Success(true) =>
          expression2.evaluateAsync
      }
    }
  }
}
case class Or(expression1: Expression, expression2: Expression) extends Expression {
  override def evaluate(implicit ctx: EvaluationCtx) = expression1.evaluate || expression2.evaluate

  override def evaluateAsync(implicit ctx: EvaluationCtx) = {
    // Note: we deliberately evaluate this sequentially to guarantee *logical* and
    // (i.e., in "A & B", B is not even evaluated if A == false.
    expression1.evaluateAsync flatMap {
      _ match {
        case Success(true) =>
          // do not continue evaluation
          Future { Success(true) }
        case Success(false) =>
          expression2.evaluateAsync
        case f => 
          // failure: do not continue evaluation
          Future { f } 
      }
    }
  }
}
case class Not(expression: Expression) extends Expression {
  override def evaluate(implicit ctx: EvaluationCtx) = !expression.evaluate

  override def evaluateAsync(implicit ctx: EvaluationCtx) = expression.evaluateAsync map {
    _ match {
      case Success(x) => Success(!x)
      case f => f // failure
    }
  }
}