package cernoch.sm.space

import scala.math._
import cernoch.scalogic.{Term, Atom, Horn}

/**
 * Beam search algorithm with caching non-productive results
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
abstract class BeamSearch[State,Result]
  extends BestFirstSearch[State,Result] {

  var beamWidth = 5
  var maxConsNonImp = 10

  private var lastTime = 0
  private var lastBest : State = _

  def shallWeHalt
    (states: Array[(State, Result)])
  = if (!states.isEmpty) {

    if (lastBest == states.head._1)
      lastTime = 0

    lastBest = states.head._1
    false

  } else {
      lastTime > maxConsNonImp
  }

  private def cropBW[T](a:Array[T])
  = a.slice(0, min(beamWidth, a.length))

  override def cropAndSort
    ( old: Array[(State,Result)],
      neu: Iterable[(State,Result)])
  = cropBW(sortByResults(old,neu))
  
  def sortByResults
    ( old: Array[(State,Result)],
      neu: Iterable[(State,Result)])
  : Array[(State,Result)]
}