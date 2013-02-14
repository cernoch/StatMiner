package cernoch.sm.space

class VerboseProbe[-S,-R] extends SearchProbe[S,R] {

  override def searchStarted() {
    println("===== Search STARTED =====")
  }
  override def searchStopped() {
    println("===== Search STOPPED =====")
  }
  override def searchIteration
  (draughts: Iterable[(S,R)], breedins: Iterable[S]) {
    println("New iteration with " + draughts.size +
      " draughts and " + breedins.size + " breedins.")
  }

  override def stateGenerated(state: S, from: S) {
    println("Gerenated new state: " + state)
  }
  override def statesGenerated() {
    println("All new states generated.")
  }

  override def stateDropped(state: S, result:R) {
    println("State has been dropped out: "+ state)
  }

  override def evaluating(state: S) {
    //println("About to evaluate: "+ state)
  }
  override def evaluated(state: S, result: R) {
    println("State "+ state + " evaluated as " + result)
  }

  override def draughtsGoingToBeSorted
  ( oldDraughts: Iterable[(S,R)],
    candidates: Iterable[(S,R)]) {}
  override def draughtsHaveBeenSorted
  (newDraughts: Iterable[(S,R)]) {}

  override def bestUpdated (state: S, result: R) {
    println(">>>>> New best state found: " + state)
  }

  override def resettingBeamCounter(resettingState: S) {}
  override def consideringStop(c: Int) {
    println("Number of consecutive non-improvements: " + c)
  }
}
