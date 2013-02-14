package cernoch.sm.space.bumphunt

import cernoch.scalogic._
import cernoch.scalistics.Hist

import cernoch.sm.algebra._
import cernoch.sm.space._

/**
 * Bump-hunting algorithm via ncALP metric
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
abstract class BeamHistSearch(modes:Set[Btom[FFT]])
  extends ClauseBeam[(Int,String,Hist[Double],Double)](modes) {

  /**
   * Execute the query.
   *
   * If using collection [[cernoch.scalogic.storage.Queriable]] storage engine,
   * you might use [[cernoch.sm.space.QueriableAdaptor]] in here.
   */
  def execQuery
    (state: Horn[HeadAtom, Set[Atom[FFT]]])
  : Iterable[Iterable[Double]]

  /**
   * All available aggregators
   */
  protected def aggregators = {
    val doubleAggregators
      = new Aggregate[Double]()
    import doubleAggregators._

    List(min, max, sum, mode,
      mean, median, new StdDev())
  }

  /**
   * Guess the right number of bins
   */
  protected def numberOfBins
  (histData: Iterable[Double])
  : Iterable[Int]
  = 3 to math.min(20, histData.toSet.size)



  def allEvaluations(data: Iterable[Iterable[Double]])
  = {
    for (aggFunc <- aggregators; // Try all aggregators
         aggregated = data.map{aggFunc(_) get}; // Apply the agg.
         binCnt <- numberOfBins(aggregated)) // Try various number of bins
    yield {

      /*
       * Compute the bin centres
       */
      val min = aggregated.min
      val max = aggregated.max
      val range = max - min

      val cuts =
        for (x <- 1 to (binCnt-1))
          yield min + (range * x / binCnt)

      val hist = Hist(aggregated, cuts)
      val rawALP = Histograms ncALP hist
      val normALP = rawALP / range / hist.max.toInt

      /*
       * Output every possible solution
       */
      ( binCnt, aggFunc name, hist, normALP )
    }

  }

  /**
   * Returns collection percentage from collection nominator and denominator
   */
  private def percentage
    (nom: Int, denom: Int)
  = denom match {
    case 0 => "?%"
    case _ => (nom * 100 / denom) + "%"
  }

  /**
   * Given the number of examples
   * with and without collection value,
   * do we have enough data?
   */
  def hasEnough
    (okay:Int, total:Int)
  = (total * 9 / 10) < okay

  private def byBestResult
  = (Ordering by {x: (Int,String,Hist[Double],Double) => x._4})

  def stateResult
    (state: Horn[HeadAtom, Set[Atom[FFT]]])
  = {
    // All examples and values
    val data = execQuery(state)
    // Only those that return some value
    val okay = data.filter{! _.isEmpty}

    // If |okay| is too small, return
    if (!hasEnough(okay.size, data.size))
      throw new TooOld("Only " +
        percentage(okay.size, data.size) +
        " examples return collection value."
      )

    // Compute all possible hist/func/...
    val allRes = allEvaluations(okay)

    if (allRes.isEmpty)
      throw new TooOld(
        "No histogram can be" +
        " generated from the clause.")

    // Pick the best solution
    allRes max byBestResult
  }

  def sortByResults
    (old:    Array[(Horn[HeadAtom, Set[Atom[FFT]]], (Int,String,Hist[Double],Double))],
     neu: Iterable[(Horn[HeadAtom, Set[Atom[FFT]]], (Int,String,Hist[Double],Double))])
  = (old ++ neu) sortBy {- _._2._4}
}
