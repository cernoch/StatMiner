package cernoch.sm.algebra

import cernoch.scalistics._
import collection.mutable.ArrayBuffer
import Collections._


/**
 * Routines on general histograms
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
object Histograms {

  private val ZERO = BigInt(0)

  def lakes[T]
    (hist: Hist[T])
  = {
    var o = List[(T,List[(T,BigInt)])]()
    val buffer = ArrayBuffer[(T,BigInt)]()

    var rise = true
    var lmax = ZERO
    var lastRise: T = 0.asInstanceOf[T]
    var last = hist.above

    def push = {
      val peak = (lmax min last)
      val NONE = List[(T,BigInt)]()
      val aligned = buffer.foldLeft(NONE)(
        (list,x) => {
          val skip = peak - x._2
          if (skip <= ZERO)
            list
          else
            ((x._1, skip)) :: list
        })

      o = (lastRise, aligned) :: o
    }

    for ((item,curr) <- hist.cutPoints.toArray.reverse) {
      val diff = curr - last

      // A peak WAS reached
      if (rise && diff < ZERO) {
        if (lmax != ZERO) push
        buffer.clear
        lmax = last
      }

      if (rise) {
        if (diff < ZERO)
          rise = false
      } else {
        if (ZERO < diff)
          rise = true
      }
      // Items since the last peak
      buffer += ((item,curr))
      last = curr
      if (ZERO < diff)
        lastRise = item
    }
    if (rise && lmax != ZERO)
      push
    o
  }

  private def alp[T]
    (hist: Hist[T])
    (implicit area: (T,T,BigInt) => BigDecimal)
  = {(
    for ((beg,lake) <- lakes(hist))
    yield {
      var last = beg
      var suma = BigDecimal(0)
      for ((value, depth) <- lake) {
        suma = suma + area(value, last, depth)
        last = value
      }
      suma
    }) sorted }

  private def noise
    (i: Iterable[BigDecimal])
  = (i zip Stream.from(1)).map(a => a._1 / a._2)

  private def maxOrZero
    (i: Iterable[BigDecimal])
  = if (i.isEmpty)
    BigDecimal(0)
  else
    i.max
  
  def ncALP(hist: Hist[BigDecimal])
  = maxOrZero(noise(differentiate(
    BigDecimal(0) :: alp(hist){(x,y,h) => BigDecimal(h)*(x-y)}
  )))
}
