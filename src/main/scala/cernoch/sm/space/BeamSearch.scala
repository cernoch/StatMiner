package cernoch.sm.space

import cernoch.scalogic._
import collection.{mutable => mut}
import annotation.tailrec
import util.Sorting
import scala.math.min

/**
 * A generic beam-search algorithm
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
abstract class BeamSearch
    (val mode: Set[Btom[Term]]) {

  type C = Horn[Atom[Term],Set[Atom[Term]]]

  var beamWidth = 5
  var maxNonImprovements = 5

  /* Evaluate the score of a clause */
  def score(c: C) : Option[BigDecimal]

  class Status {
    var clausesEvaluated = 0
    var clausesGenerated = 0
    
  }

  var status: Option[Status] = None
  var result: Option[Status] = None 

  /*  */
  def start = {
    try {
      status = Some(new Status())

      fireee(Array(), Set(emptyClause), 0)(new Generator(mode))

    } finally {
      result = status
      status = None
    }
  }

  def emptyClause : Horn[Atom[Term],Set[Atom[Term]]]

  @tailrec
  private def fireee
    ( draughts: Array[(C,BigDecimal)],
      breedins: Iterable[C],
      noImpMnt: Int)
    (implicit generator: Generator)
  : C
  = {

    val generated = mut.HashSet[C]()

    for (parent <- breedins) {
      for (offspring <- generator.addAtomToHorn(parent)) {

        // Add the new clauses
        generated += offspring.neu
        status.get.clausesGenerated += 1;

        // Instantiate new variables
        for (inst <- Generator.instantiateHornBody(
                       offspring.neu,
                       offspring.added.variables.toSet))
          generated += inst.neu
          status.get.clausesGenerated += 1;
      }
    }

    // Evaluate all new clauses
    val evaluation = generated map { c =>
      status.get.clausesEvaluated += 1
      (c,score(c))
    } toMap

    // Worst Score In Previous Generation
    val wsipg = draughts.last._2
    val newBreedins = // Generate new breedings
      (for ((c,s) <- evaluation if s.isEmpty) yield c) ++
      (for ((c,s) <- evaluation if s.isDefined && s.get > wsipg) yield c)

    
    // New draughts
    def cropByBeamWidth[T](a:Array[T]) = a.slice(0, min(beamWidth, a.length))
    val newDraugts = cropByBeamWidth(
      (draughts ++ (evaluation mapValues {_.get})) sortBy {- _._2}
    )

    // Number of no improvements
    val newNonImprovements = if (newDraugts == draughts) noImpMnt + 1 else 0

    if (newBreedins.isEmpty || noImpMnt > maxNonImprovements)
      newDraugts.head._1 else
      fireee(newDraugts, newBreedins, newNonImprovements)
  }
}
