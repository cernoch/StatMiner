package cernoch.sm.space

import cernoch.scalogic._
import cernoch.sm.algebra.Collections._
import collection.mutable.HashSet

/**
 *
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
trait Refine[C<:Clause[_,_]] {

  def old: C
  def neu: C
  
}

trait AddAtom
    [C<:Clause[_,_]]
  extends Refine[C] {

  def added: Atom[Term]

}

trait Instant
    [C<:Clause[_,_]]
  extends Refine[C] {
  
  def oldVar: Var
  def neuVal: Val[_]
  
}



object Generator {

  /* Adds a new atom to a clause */
  def addAtomToHorn
    [H <: Atom[Term]]
    (mode: Set[Btom[FFT]])
    (orig: Horn[H,Set[Atom[Term]]])
  = {
    val idx = orig
      .bodyAtoms.toList
      .flatMap{_.variables}
      .groupBy{_.dom}

    mode.flatMap(m => {
      val mIn = m.modeIn.toList

      carthesian(mIn.map{v => idx(v.dom)})
        .map{bind => m mapSomeArg (mIn zip bind).toMap.get}
    
    }).map{ a => new AddAtom[Horn[H,Set[Atom[Term]]]]() {
        def added = a
        def old = orig
        def neu = new Horn(orig.head, orig.bodyAtoms + a)
      }
    }
  }

  /* All variables for instantiation */
  def instantiable
    [S <: Iterable[Atom[FFT]]]
    (atoms: S)
  = {
    for(atom <- atoms; bArg <- atom.args
        if bArg.isInstanceOf[Var]
        if bArg.dom.isInstanceOf[CatDom]
        if bArg.dom.asInstanceOf[CatDom].allowed.size > 0)
      yield bArg.asInstanceOf[Var]
  }

  /* Instantiates variables in a Horn clause */
  def instantiateHornBody
    [H<:Atom[Term]]
    (clause: Horn[H,Set[Atom[Term]]],
     candidates: Set[Var])

  = candidates
  .filter{_.dom.isInstanceOf[CatDom]}
  .flatMap(war => {
    val catDom = war.dom.asInstanceOf[CatDom]
    catDom.allowed.map{ wal => {

      new Instant[Horn[H,Set[Atom[Term]]]]() {

        private val _neuVal = Val(wal,catDom)
        private val _neu = new Horn(
          clause.head,
          clause.bodyAtoms.map{
            _.mapSomeArg(Dict(war -> _neuVal).get)
          }
        )

        def old = clause
        def neu = _neu
        def oldVar = war
        def neuVal = _neuVal
      }
    }}
  })
}