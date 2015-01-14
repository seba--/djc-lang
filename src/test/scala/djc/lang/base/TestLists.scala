package djc.lang.base

import djc.lang.AbstractTest
import djc.lang.TypedLanguage._
import djc.lang.TypedLanguage.types._
import djc.lang.TypedSyntaxDerived._
import djc.lang.sem._
import djc.lang.typ.Checker._
import util.Bag
import djc.lang.base.Integer._
import djc.lang.base.Double._
import djc.lang.base.String._
import djc.lang.base.Lists._

class TestLists6 extends TestLists(concurrent_6_thread.SemanticsFactory)
//class TestLambda2 extends TestLambda(nondeterm_2_env.Semantics)
//class TestLambda3 extends TestLambda(nondeterm_3_routed.SemanticsFactory)
//class TestLambda4 extends TestLambda(nondeterm_4_grouped.SemanticsFactory)
//class TestLambda5 extends TestLambda(nondeterm_5_parallel.SemanticsFactory)
//class TestLambda6 extends TestLambda(concurrent_6_thread.SemanticsFactory)


class TestLists[V](sem: ISemanticsFactory[V]) extends AbstractTest(sem) {
  val Ti = TInteger
  val Td = TDouble
  val Ts = TString
  val nothing: AbstractSemantics.Res[Bag[Send]] = Set(Bag())

  val l1 = nil(Ti)

  testType("Empty list int", l1, TList(Ti))


  val l2 = 1 :: 2 :: 3 :: nil(Ti)
  testType("Nonempty list int", l2, TList(Ti))

  val l1d = nil(Td)
  testType("Empty list double", l1d, TList(Td))


  val l2d = 1.0 :: 2.0 :: 3.0 :: nil(Td)
  testType("Nonempty list double", l2d, TList(Td))

  val l3 = 1 :: 2.0 :: nil(Ti)
  test("Heterogenuous list") {
    intercept[TypeCheckException] {
      typeCheck(Map(), Map(), Map(), l3)
    }
  }

  def p(t: Type, e: Exp): Par = Par(PRINT(t, e))
  def res(sends: Exp*): AbstractSemantics.Res[Bag[Send]] = Set(Bag(sends.map(PRINT):_*))

  testInterp("Nonempty list double", p(TList(Td), l2d), res(lst(Td, 1.0, 2.0, 3.0)))
  testInterp("Nonempty elemAt", p(Td, l2d.elemAt(1)), res(2.0))
  testType("Nonempty elemAt", p(Td, l2d.elemAt(1)), Unit)
  testInterp("List concat", p(TList(Ts), lst(Ts, "cloud") +++ lst(Ts, "calculus")), res(lst(Ts, "cloud", "calculus")))
  testType("List concat", p(TList(Ts), lst(Ts, "cloud") +++ lst(Ts, "calculus")), Unit)

  val polymorph = TAbs('alpha, SpawnImg(TApp(PRINT_SERVER, TList('alpha)))~>'PRINT!!(nil('alpha)))
  testInterp("Polymorphic", Par(polymorph(Ti)), res(lst(Ti)))
  testType("Polymorphic", polymorph, TUniv('alpha, Unit))
}
