package cernoch.sm.algebra

import cernoch.scalistics.Hist

/**
 * Distance functions on distributions
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
object Distances {

  private val ZERO = BigInt(0)

  /**
   * Use distence metric ''f'' on discrete
   * distributions ''a'' and ''b''.
   */
  private def applyDiscrete
    [T,R]
    (a: Hist[T], b: Hist[T])
    (f: (BigInt,BigInt) => R)
    (g: (R,R) => R)
    (implicit num: Numeric[R])
  = {
    ( // This is a performance improvement:
      if (a.cutPoints.keys == b.cutPoints.keys) {
        a.cutPoints.keys
      } else {
        (Set[T]() ++ a.cutPoints.keys ++ b.cutPoints.keys)
      } // Here starts the real procedure:
      ).foldLeft(f(a.above, b.above)){
      (x,v) => g(x, f(
        a.cutPoints.get(v).getOrElse(ZERO),
        b.cutPoints.get(v).getOrElse(ZERO))
      )
    }
  }

  /** Bhattacharya distance */
  def bhattacharya
    [T]
    (a: Hist[T], b: Hist[T])
  = {
    val denominator = BigDecimal(a.sum * b.sum)
    applyDiscrete(a,b){(x,y) =>
      scala.math.sqrt((
        BigDecimal(x * y) / denominator
        ).toDouble)
    }{_ + _}
  }
}
