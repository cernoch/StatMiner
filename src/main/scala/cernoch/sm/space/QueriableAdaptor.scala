package cernoch.sm.space

import cernoch.scalogic._
import collection.mutable.ArrayBuffer

/**
 * Adapts collection queriable storage for [[cernoch.sm.space.bumphunt.BeamHistSearch]]
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
class QueriableAdaptor
    [Q <: {def query(c: Horn[Atom[FFT], Set[Atom[FFT]]]) : Iterable[Map[Var,Val[_]]] } ]
    (storage: Q)
    extends (Horn[HeadAtom, Set[Atom[FFT]]] => Iterable[Iterable[Double]]) {

  def apply(q: Horn[HeadAtom, Set[Atom[FFT]]])
  = QueriableAdaptor.helper(
      storage.query(q) // Execute the query
      .groupBy{ _(q.head.exVar) }.values, // Group by the example number
    q.head.histVar) // And extract the histogram values

}

private object QueriableAdaptor {

  private[space] def helper(i: Iterable[Iterable[Map[Var,Val[_]]]], v: Var)
  = i.map{ _.map{ _( v ).get.asInstanceOf[BigDecimal].toDouble } }
}



/**
 * Adapts collection queriable storage for [[cernoch.sm.space.bumphunt.BeamHistSearch]]
 * and splits data between train and test sets.
 *
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
class QueryCrossvalAdaptor
  [Q <: {def query(c: Horn[Atom[FFT], Set[Atom[FFT]]]) : Iterable[Map[Var,Val[_]]] } ]
  ( storage: Q, crossvalidator: (Iterable[Any] => Iterable[Any => Boolean]) )
  extends (Horn[HeadAtom, Set[Atom[FFT]]] => Iterable[(Iterable[Iterable[Double]], Iterable[Iterable[Double]] )] ) {

  def apply(q: Horn[HeadAtom, Set[Atom[FFT]]])
  = {
    val data = storage.query(q) // Execute the query
      .groupBy{ _(q.head.exVar) } // Group by the example number

    crossvalidator(data).map( trainTest => {
      val (train,test) = split(data)(trainTest)

      ( QueriableAdaptor.helper(train.map{_._2}, q.head.histVar),
        QueriableAdaptor.helper( test.map{_._2}, q.head.histVar) )
    })
  }

  private def split[T]
    (coll: Iterable[T])
    (how: (T => Boolean))
  : (Iterable[T],Iterable[T])
  = {
    val a = ArrayBuffer[T]()
    val b = ArrayBuffer[T]()

    for (x <- coll) yield how(x) match {
      case true  => a += x
      case false => b += x
    }
    (a,b)
  }
}
