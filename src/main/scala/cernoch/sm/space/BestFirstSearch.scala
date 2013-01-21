package cernoch.sm.space

import annotation.tailrec
import scala.Array

/**
 * A generic best-first-search algorithm
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
abstract class BestFirstSearch[S,R] {

  /**
   * Crops and sorts the states after each iteration
   * 
   * <p>Method can return a reference
   * to the given object or a new object</p>
   */
  def cropAndSort
    ( old: Array[(S,R)],
      neu: Iterable[(S,R)])
  : Array[(S,R)]
  = old ++ neu

  /**
   * Should the algorithm halt?
   */
  def shallWeHalt
    (s: Array[(S,R)])
  : Boolean

  /**
   * Evaluate the score of a state
   *
   * @return [[scala.None]] if state not evaluable
   */
  def stateResult
    (state: S)
  : R

  /**
   * Generate new states
   */
  def descendants
    (state: S)
  : Iterable[S]

  /**
   * Initial state to start the search
   */
  def sourceState : S

  /**
   * Status of the search
   */
  class Status {
    var evaluated = 0
    var generated = 0
    var thrownOut = 0
    var breedings = 0
  }

  var status: Option[Status] = None
  var result: Option[Status] = None

  def start = {
    try {
      status = Some(new Status())

      fireee(Array[(S,R)](), List(sourceState))

    } finally {
      result = status
      status = None
    }
  }

  def bestUpdated
    (state: S, result: R)
  : Unit
  = {}
  
  @tailrec
  private def fireee
    ( draughts: Array[(S,R)],
      breedins: List[S])
  : Array[(S,R)]
  = {
    
    val children =
      for (breedin <- breedins;
           child <- descendants(breedin))
        yield {
          status.get.generated += 1
          child
        }
    
    var newBreeds = List[S]()
    var withScore = List[(S,R)]()
    
    for (child <- children) {

      try {
        val score = stateResult(child)
        status.get.evaluated += 1

        withScore = (child,score) :: withScore

      } catch {
        case _:TooOld => { status.get.thrownOut += 1 }
        case _:TooNew => {
          status.get.breedings += 1
          newBreeds = child :: breedins
        }
      }
    }
    
    val newDraugts = cropAndSort(draughts, withScore)

    // Update the best so far
    if (!newDraugts.isEmpty) {
      val (bState,bResult) = newDraugts(0)

      if (draughts.isEmpty ||
         (bState != draughts(0)._1))
        bestUpdated(bState, bResult)
    }
    
    shallWeHalt(newDraugts) match {
      case true => newDraugts
      case false => fireee(newDraugts, newBreeds)
    }
  }
}
