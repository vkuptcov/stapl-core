package stapl.core.pdp

import scala.annotation.tailrec

import grizzled.slf4j.Logging
import stapl.core.{ConcreteLogObligationAction, ConcreteObligationAction}

/**
 * Class used for representing an obligation service. An obligation service
 * is used by a PDP to fulfill obligations. An obligation service consists 
 * of multiple ObligationServiceModules that each can fulfill some specific
 * obligations.
 */
class ObligationService extends Modules[ObligationServiceModule] {

  /**
   * Tries to fulfill the given ObligationAction using its
   * ObligationServiceModules and returns whether a module
   * was able to fulfill the obligation. More precisely, this method
   * iterates over all modules and returns when the first module has
   * handled the ObligationAction.
   */
  def fulfill(obl: ConcreteObligationAction): Boolean = {
    @tailrec
    def fulfill(modules: List[ObligationServiceModule]): Boolean = modules match {
      case module :: tail => module.fulfill(obl) match {
        case true => true
        case false => fulfill(tail) // continue
      }
      case Nil => false
    }
    fulfill(modules)
  } 
}

/**
 * The general interface of an obligation service module.
 */
trait ObligationServiceModule {
  
  /**
   * Tries to fulfill the given ObligationAction and returns whether
   * this succeeded.
   */
  def fulfill(obl: ConcreteObligationAction): Boolean
}

class LogObligationServiceModule extends ObligationServiceModule with Logging {
  
  override def fulfill(obl: ConcreteObligationAction) = {
    // we only support LogObligationActions
    obl match {
      case log: ConcreteLogObligationAction =>
        info(s"Log obligation: ${log.msg}")
        true
      case _ => false
    }
  }
}