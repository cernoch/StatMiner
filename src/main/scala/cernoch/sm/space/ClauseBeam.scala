package cernoch.sm.space

import cernoch.scalogic._

/**
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
@Deprecated
abstract class ClauseBeam[Result]
    (mode: Set[Btom[FFT]])
  extends BeamSearch[ Horn[HeadAtom,Set[Atom[FFT]]], Result ] {

  /**
   * Initial state is an empty horn clause without collection head
   */
  def sourceState = Horn(Set())

  /**
   * Adds new atoms to the horn clause based on the language bias
   */
  protected def addNewAtoms
    (orig: Horn[HeadAtom,Set[Atom[FFT]]])
  = Generator.addAtomToHorn(mode)(orig)

  /**
   * Replaces the head in each refined clause
   */
  protected def replaceHead
    (addRefments: Iterable[AddAtom[Horn[HeadAtom,Set[Atom[FFT]]]]])
  = for (addRefment <- addRefments;
         histVar <- addRefment.added.variables
         if histVar.dom.isInstanceOf[DecDom];
         exVar <- addRefment.added.variables
         if exVar.dom.isKey )
  yield
    new AddAtom[Horn[HeadAtom,Set[Atom[FFT]]]]() {
      def added = addRefment.added
      def old = addRefment.old
      def neu = new Horn(
        new HeadAtom(exVar,histVar),
        addRefment.neu.bodyAtoms )
    }

  /**
   * Instantiates variables in the newly added atom
   *
   * <p>Variables in the head of the clause is omitted.</p>
   */
  protected def instantiate
    (addedAtoms: Iterable[AddAtom[Horn[HeadAtom,Set[Atom[FFT]]]]])
  = addedAtoms.flatMap( offspring => {
    Generator.instantiateHornBody(
      offspring.neu,
      offspring.added.variables.toSet --
        offspring.neu.head.variables )
  })

  def descendants
    (clause: Horn[HeadAtom, Set[Atom[FFT]]])
  = {
    val expand = addNewAtoms(clause)
    val replHd = replaceHead(expand)
    val insted = instantiate(replHd)

    expand.map{_.neu} ++
    replHd.map{_.neu} ++
    insted.map{_.neu}
  }
}

@Deprecated
class HeadAtom(var exVar: Var, var histVar: Var)
  extends Atom[Var]("head", List(exVar, histVar)) {}
