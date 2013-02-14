package cernoch.sm.space

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

import cernoch.sm.algebra.Intervals._
import cernoch.scalistics.{Basis, Interval}
import cernoch.scalogic._


@RunWith(classOf[JUnitRunner])
class RefineTest extends Specification {

  val numD = NumDom("num")

  val catD1 = CatDom("cat1", false)
  val catD2 = CatDom("cat1", false, Set("A", "B", "C"))

  val x = Var(numD)
  val y = Var(catD1)
  val z = Var(catD2)

  val A = Val("A", catD2)
  val B = Val("B", catD2)
  val C = Val("C", catD2)

  "Instantiable variables" should {
    "filter out non-CatDom and value-empty variables" in {
      Generator.instantiable(
        Set(Atom("p", x,y,z))
      ) must_== Set(z)
    }
  }

  "Instantiating Horn clauses" should {
    "instantiate to all possible values" in {

      val retVal =
        Generator.instantiateHornBody(
          new Horn(Atom("h",x),
                   Set(Atom[FFT]("p", x,y,z))),
          Set(y,z)
        )

      (retVal.size must_== 3) and
      (retVal flatMap {_.neu.bodyAtoms.map{_.args(2)}} must_== Set(A,B,C))
    }
  }

  "Adding an atom" should {

    "add an atom to an empty clause" in {

      val btom = new Btom("p", List(x,y,z),
        hooks = Set(), modeIn = Set())

      val horn = new Horn(Atom("h"), Set[Atom[FFT]]())
      val retVal = Generator.addAtomToHorn(Set(btom))(horn)

      (retVal.size must_== 1) and
      (retVal.head.neu must_== new Horn(Atom("h"), Set(btom)))
    }

    "pick all variables of the given type" in {

      val v = Var(catD1)
      val w = Var(catD1)

      val btom = new Btom(
        "p",
        List(x,y,z),
        hooks = Set(),
        modeIn = Set(y))

      val atom1 = new Atom("q", List[FFT](x,v))
      val atom2 = new Atom("r", List[FFT](x,w))

      val horn = new Horn(Atom("h"), Set(atom1,atom2))
      val retVal = Generator.addAtomToHorn(Set(btom))(horn)

      (retVal.size must_== 2) and
      (retVal.flatMap{_.neu.bodyAtoms} must_== Set(
        atom1, atom2, Atom("p", x,v,z), Atom("p", x,w,z)
      ))
    }
  }
}