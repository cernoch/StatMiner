package cernoch.sm.space

import annotation.tailrec
import scala.Array
import java.util.concurrent.Callable

/**
 * A generic best-first-search algorithm
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
abstract class BestFirstSearch[S,R]
  extends Callable[Iterable[(S,R)]] {

  var probe = new SearchProbe[S,R]

  /**
   * Crops and sorts the states after each iteration
   * 
   * <p>Method can return collection reference
   * to the given object or collection new object</p>
   */
  def cropAndSort
    ( old: Iterable[(S,R)],
      neu: Iterable[(S,R)])
  : Iterable[(S,R)]
  = old ++ neu

  /**
   * Should the algorithm halt?
   */
  def shallWeHalt
    (s: Iterable[(S,R)])
  : Boolean

  /**
   * Evaluate the score of collection state
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

  def call() = {
    try {
      status = Some(new Status())
      probe.searchStarted()

      var draughts = List[(S,R)]()
      var breedins = List[S]()
      val source = sourceState
      try {
        probe.evaluating(source)
        val result = stateResult(source)
        status.get.evaluated += 1
        probe.evaluated(source, result)
        probe.bestUpdated(source, result)
        draughts = (source, result) :: draughts

      } catch {
        case _:TooOld => { status.get.thrownOut += 1 }
        case _:TooNew => {
          status.get.breedings += 1
          breedins = source :: breedins
        }
      }

      fireee(draughts, breedins)

    } finally {
      result = status
      status = None
      probe.searchStopped()
    }
  }

  @tailrec
  private def fireee
    ( draughts: Iterable[(S,R)],
      breedins: List[S])
  : Iterable[(S,R)]
  = {
    probe.searchIteration(draughts, breedins)

    val children = for (
      source <- breedins.view ++ draughts.view.map{_._1};
      child <- descendants(source)
    ) yield {
      status.get.generated += 1
      probe.stateGenerated(child, source)
      child
    }
    probe.statesGenerated()
    
    var newBreeds = List[S]()
    var withScore = List[(S,R)]()
    var newDraugts = draughts

    for (child <- children) {

      try {
        probe.evaluating(child)
        val score = stateResult(child)
        status.get.evaluated += 1
        probe.evaluated(child, score)

        withScore = (child,score) :: withScore

        // Save memory
        // TODO: Implement as collection heap
        if (withScore.size > 5) {
          probe.draughtsGoingToBeSorted(newDraugts, withScore)
          newDraugts = cropAndSort(newDraugts, withScore)
          probe.draughtsHaveBeenSorted(newDraugts)
          withScore = List[(S,R)]()
        }

      } catch {
        case _:TooOld => { status.get.thrownOut += 1 }
        case _:TooNew => {
          status.get.breedings += 1
          newBreeds = child :: breedins
        }
      }
    }

    probe.draughtsGoingToBeSorted(newDraugts, withScore)
    newDraugts = cropAndSort(newDraugts, withScore)
    probe.draughtsHaveBeenSorted(newDraugts)

    // Update the best so far
    if (!newDraugts.isEmpty) {
      val (bState,bResult) = newDraugts.head
      if (draughts.isEmpty || (bState != draughts.head._1))
        probe.bestUpdated(bState, bResult)
    }
    
    shallWeHalt(newDraugts) match {
      case true => newDraugts
      case false => fireee(newDraugts, newBreeds)
    }
  }
}
