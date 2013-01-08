package cernoch.sm.space.bumphunt

import cernoch.scalogic._
import cernoch.scalistics.Hist

import cernoch.sm.algebra._
import cernoch.sm.space.{HeadAtom, ClauseBeam}

import scala.math.{BigDecimal => BigDec}

/**
 * Bump-hunting algorithm via ncALP metric
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
abstract class BeamHistSearch(modes:Set[Btom[FFT]])
  extends ClauseBeam[(Int,String,BigDec)](modes) {

  /**
   * Execute the query.
   *
   * If using a [[cernoch.scalogic.storage.Queriable]] storage engine,
   * you might use [[cernoch.sm.space.QueriableAdaptor]] in here.
   */
  def execQuery
    (state: Horn[HeadAtom, Set[Atom[Term]]])
  : Iterable[Iterable[BigDecimal]]

  /**
   * All used aggregators
   */
  protected def aggregators = {
    val x = new Aggregate[BigDecimal]()
    import x._
    List(min, max, sum, mode, mean, median, variance)
  }

  /**
   * Guess the right number of bins
   */
  protected def numberOfBins
    (histData: Iterable[BigDecimal])
  : Iterable[Int]
  = 3 to 20


  def stateResult
    (state: Horn[HeadAtom, Set[Atom[Term]]])
  = {
    val data = execQuery(state)

    val possibilities
    = for (aggFunc <- aggregators; // Try all aggregators
           aggregated = data.map{aggFunc(_) get}; // Apply the agg.
           binCnt <- numberOfBins(aggregated)) // Try various number of bins
      yield {

        /*
         * Compute the bin centres
         */
        val min = aggregated.min
        val range = aggregated.max - min

        val cuts = for (x <- 1 to (binCnt-1)) yield
          min + ((range * BigDecimal(x)) / binCnt)

        /*
         * Output every possible solution
         */
        ( binCnt,
          aggFunc name,
          Histograms ncALP Hist(aggregated, cuts)
        )
      }

    // Pick the best solution
    Some(possibilities max (Ordering by {x: (Int,String,BigDec) => x._3}))
  }

  def sortByResults
    (old:    Array[(Horn[HeadAtom, Set[Atom[Term]]], (Int,String,BigDec))],
     neu: Iterable[(Horn[HeadAtom, Set[Atom[Term]]], (Int,String,BigDec))])
  = (old ++ neu) sortBy {- _._2._3}
}
