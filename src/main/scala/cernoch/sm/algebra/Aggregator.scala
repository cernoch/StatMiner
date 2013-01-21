package cernoch.sm.algebra

import scala.Some._

/**
 *
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
trait Aggregator[T] extends (Iterable[T] => Option[T]) {

  def name : String

  override def toString = name

  override def equals(o:Any)
  = o.isInstanceOf[Aggregator[_]] &&
    o.asInstanceOf[Aggregator[_]].toString == toString
  override def hashCode = toString.hashCode
}

class Aggregate[T](implicit num: Fractional[T]) {
  val min = new MinValues[T]()
  val max = new MaxValues[T]()
  val sum = new SumValues[T]()
  val mode = new MostOftenValue[T]()
  val mean = new MeanValue[T]()
  val median = new MedianIntegral[T]()
  val variance = new Variance[T]()
}

object Aggregate {

  class Encapsulator[T]
      (i: Iterable[T])
      (implicit num: Fractional[T]) {

    def sum = new SumValues[T]().apply(i).get
    def mode = new MostOftenValue().apply(i).get
    def mean = new MeanValue[T]().apply(i).get
    def median = new MedianIntegral[T]().apply(i).get
    def variance = new Variance[T]().apply(i).get
  }

  implicit def convertor[T]
    (i: Iterable[T])
    (implicit num: Fractional[T])
  = new Encapsulator[T](i)
}



final case class SumValues[T]
(implicit num: Numeric[T])
  extends Aggregator[T] {

  import num._
  def name = "sum"

  def apply(i: Iterable[T])
  = if (i.isEmpty) None else Some(
    i.foldLeft(zero){plus}
  )
}



final case class MinValues[T]
    (implicit num: Ordering[T])
  extends Aggregator[T] {

  def name = "min"

  def apply(i: Iterable[T])
  = if (i isEmpty) None else Some(
    i.reduceLeft(num min)
  )
}



final case class MaxValues[T]
    (implicit num: Ordering[T])
  extends Aggregator[T] {

  def name = "max"

  def apply(i: Iterable[T])
  = if (i isEmpty) None else Some(
    i.reduceLeft(num max)
  )
}



final case class MeanValue[T]
(implicit num: Fractional[T])
  extends Aggregator[T] {

  import num._
  def name = "mean"

  def apply(i: Iterable[T])
  = if (i.isEmpty) None else Some(
    num.div(
      i.foldLeft(zero){plus},
      i.map{x => one}
        .foldLeft(zero){plus}
    ) )
}



final case class MedianIntegral[T]
    (implicit num: Fractional[T])
  extends Aggregator[T] {

  def name = "median"
  override def toString
  = super.toString + "(fractional)"

  def apply(i: Iterable[T])
  = if (i.isEmpty) None else Some({
    val pole = i.toList.sorted(num)
    pole.size % 2 match {
      case 1 => pole(pole.size / 2)
      case 0 => avg(pole(pole.size / 2 - 1), pole(pole.size / 2))
    }
  })

  private def avg(a:T, b:T) = num.div(num.plus(a,b), TWO)
  private val TWO = num.plus(num.one, num.one)
}



final case class MedianOrdered[T]
    (implicit num: Ordering[T])
  extends Aggregator[T] {

  def name = "median"
  override def toString
  = super.toString + "(ordered)"

  def apply(i: Iterable[T])
  = if (i.isEmpty) None else Some({
    val pole = i.toList.sorted
    pole(pole.size / 2)
  })
}



final case class Variance[T]
    (implicit num: Fractional[T])
  extends Aggregator[T] { self =>

  override def name = "variance"

  import num._
  private val mean = new MeanValue[T]()

  def apply(it: Iterable[T])
  = if (it.isEmpty) None else Some({
    val mu = mean(it).get

    it.map{minus(_,mu)}
      .map{sqr}
      .foldLeft(zero){plus}
  })
  
  protected def sqr(a:T) = num.times(a,a)
}



final case class StdDev()
  extends Aggregator[Double] {

  override def name = "stdDev"

  private val varia = new Variance[Double]()

  def apply(it: Iterable[Double])
  = varia(it) map math.sqrt
}



final case class MostOftenValue[T]()
  extends Aggregator[T] {

  def name = "mode"

  def apply(it: Iterable[T])
  = if (it.isEmpty) None else Some(
    it.groupBy(x => x)
      .mapValues(_.size)
      .toArray
      .sortBy(_._2)
      .last._1
  )
}
