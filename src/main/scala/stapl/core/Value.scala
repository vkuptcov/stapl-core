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
 * The base trait for all attributes in policies. Each Value has a 
 * type and multiplicity (list or not).
 */
trait Value {
  
  val aType: AttributeType
  
  val isList: Boolean
  
  def getConcreteValue(ctx: EvaluationCtx): ConcreteValue
  
  def in(that: Value): Expression = {
    if (this.isList || !that.isList)
      throw new UnsupportedOperationException("An in operation is only possible between a simple value and a list.")
    typeCheck(that)
    durationTypeCheck("in")
    ValueIn(this, that)
  }

  def ===(that: Value): Expression = {
    if (this.isList || that.isList)
      throw new UnsupportedOperationException("An equals operation is only possible between simple values.")
    typeCheck(that)
    durationTypeCheck("equals")
    EqualsValue(this, that)
  }
  
  // XXX added !==
  def !==(that: Value): Expression = !(this === that)
  
  def gt(that: Value): Expression = {
    if (this.isList || that.isList)
      throw new UnsupportedOperationException("A comparison operation is only possible between simple values.")
    typeCheck(that)
    durationTypeCheck("comparison")
    GreaterThanValue(this, that)
  }
  
  def lt(that: Value): Expression = that gt this
  
  def gteq(that: Value): Expression = (this gt that) | (this === that)
  
  def lteq(that: Value): Expression = (this lt that) | (this === that)
  
  def +(that: Value): Operation = {
    Addition(this, that)
  }
  
  def -(that: Value): Operation = {
    Subtraction(this, that)
  }
  
  def *(that: Value): Operation = {
    Multiplication(this, that)
  }
  
  def /(that: Value): Operation = {
    Division(this, that)
  }

  private def typeCheck(that: Value) {
    AttributeType.checkType(that.aType, this.aType)
  }

  private def durationTypeCheck(operationName: String) {
    if (List(DateTimeDuration, DayDuration, TimeDuration) contains this.aType)
      throw new UnsupportedOperationException(s"An $operationName operation is not possible for durations.")
  }
}
