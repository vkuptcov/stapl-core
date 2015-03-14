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
package stapl.core.pdp

import scala.concurrent.Future
import scala.util.{Success, Try}

import grizzled.slf4j.Logging
import stapl.core._

/**
 * The base class of the context for evaluating a policy. This context
 * represents all information for that policy evaluation, such as the
 * id of the subject, the id of the resource, the id of the action and
 * a method to find attributes.
 *
 * The method to find attributes is required in the evaluation context
 * because certain aspects such as an attribute cache are specific for
 * each individual evaluation context.
 */
trait EvaluationCtx {

  def evaluationId: String
  def subjectId: String
  def resourceId: String
  def actionId: String
  def remoteEvaluator: RemoteEvaluator
  def cachedAttributes: Map[Attribute, ConcreteValue]
  def employedAttributes: Map[Attribute, ConcreteValue]
  protected[core] def findAttribute(attribute: Attribute): ConcreteValue
  protected[core] def getCombinationAlgorithmImplementation(algo: CombinationAlgorithm): CombinationAlgorithmImplementation

  // TODO add type checking here
  //final def findAttribute(attribute: Attribute): ConcreteValue = 
}

/**
 * An implementation of a basic evaluation context. This evaluation context
 * stores the subject id, the resource id, the action id and stores found
 * attribute values in a cache for this evaluation context.
 */
class BasicEvaluationCtx(override val evaluationId: String, request: RequestCtx,
  finder: AttributeFinder, override val remoteEvaluator: RemoteEvaluator,
  bundle: CombinationAlgorithmImplementationBundle = SimpleCombinationAlgorithmImplementationBundle) extends EvaluationCtx with Logging {

  override val subjectId: String = request.subjectId

  override val resourceId: String = request.resourceId

  override val actionId: String = request.actionId

  protected val attributeCache: scala.collection.mutable.Map[Attribute, ConcreteValue] = scala.collection.mutable.Map() //scala.collection.concurrent.TrieMap()

  override def cachedAttributes: Map[Attribute, ConcreteValue] = attributeCache.toMap

  // add all attributes given in the request to the attribute cache
  for ((attribute, value) <- request.allAttributes) {
    attributeCache(attribute) = value
  }

  protected val _employedAttributes: scala.collection.mutable.Map[Attribute, ConcreteValue] = scala.collection.mutable.Map()

  override def employedAttributes = _employedAttributes.toMap

  /**
   * Try to find the value of the given attribute. If the value is already
   * in the attribute cache, that value is returned. Otherwise, the attribute
   * finder is checked and the found value is stored in the attribute cache if
   * a value is found.
   *
   * @throws	AttributeNotFoundException	If the attribute value isn't found
   */
  @throws[AttributeNotFoundException]("if the attribute value isn't found")
  override def findAttribute(attribute: Attribute): ConcreteValue = {
    attributeCache.get(attribute) match {
      case Some(value) => {
        debug("FLOW: found value of " + attribute + " in cache: " + value)
        _employedAttributes(attribute) = value
        value
      }
      case None => { // Not in the cache
        finder.find(this, attribute) match {
          case None =>
            val entityId = attribute.cType match {
              case SUBJECT => subjectId
              case RESOURCE => resourceId
              case ACTION => "ACTION??" // we don't support this
              case ENVIRONMENT => "ENVIRONMENT??" // we don't support this
            }
            debug(s"Didn't find value of $attribute for entity $entityId anywhere, throwing exception")
            throw new AttributeNotFoundException(evaluationId, entityId, attribute)
          case Some(value) =>
            attributeCache(attribute) = value // add to cache
            _employedAttributes(attribute) = value
            debug("FLOW: retrieved value of " + attribute + ": " + value + " and added to cache")
            value
        }
      }
    }
  }

  /**
   * To make sure that we only request an attribute once from the database, we
   * store all futures regarding a certain attribute. This way, we can return this
   * future if an attribute is requested again after the first time.
   */
  private val attributeFutures = scala.collection.mutable.Map[Attribute, Future[Try[ConcreteValue]]]()

  // immediately fill these attribute futures with the futures for the cached attributes
  // to simplify the rest of the code
  for ((attribute, value) <- request.allAttributes) {
    attributeFutures(attribute) = Future successful Success(value)
  }

  /**
   * Return the implementation of the requested combination algorithm.
   */
  def getCombinationAlgorithmImplementation(algo: CombinationAlgorithm): CombinationAlgorithmImplementation = algo match {
    case PermitOverrides => bundle.PermitOverrides
    case DenyOverrides => bundle.DenyOverrides
    case FirstApplicable => bundle.FirstApplicable
  }
}