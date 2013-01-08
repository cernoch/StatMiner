package cernoch.sm.space

import cernoch.scalogic._
import storage.Queriable

/**
 * Adapts a queriable storage for [[cernoch.sm.space.bumphunt.BeamHistSearch]]
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
class QueriableAdaptor(storage: Queriable[Horn[Atom[FFT], Set[Atom[FFT]]], Val[_]])
    extends (Horn[HeadAtom, Set[Atom[FFT]]] => Iterable[Iterable[BigDecimal]]) {

  def apply(q: Horn[HeadAtom, Set[Atom[FFT]]])
  = storage.query(q) // Execute the query
    .groupBy{ _(q.head.exVar) }.values // Group by the example number
    .map{ _.map{ _( q.head.histVar ).get.asInstanceOf[BigDecimal] } }
    // And extract the histogram values
}