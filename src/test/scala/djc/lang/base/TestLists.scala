package djc.lang.base

import djc.lang.AbstractTest
import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived._
import djc.lang.sem._
import djc.lang.typ.Checker._
import djc.lang.typ.Types._
import util.Bag
import djc.lang.base.Integer._
import djc.lang.base.Double._
import djc.lang.base.Lists._

class TestLists1 extends TestLists(nondeterm_1_subst.Semantics)
//class TestLambda2 extends TestLambda(nondeterm_2_env.Semantics)
//class TestLambda3 extends TestLambda(nondeterm_3_routed.SemanticsFactory)
//class TestLambda4 extends TestLambda(nondeterm_4_grouped.SemanticsFactory)
//class TestLambda5 extends TestLambda(nondeterm_5_parallel.SemanticsFactory)
//class TestLambda6 extends TestLambda(concurrent_6_thread.SemanticsFactory)


class TestLists[V](sem: ISemanticsFactory[V]) extends AbstractTest(sem) {
  val Ti = TInteger
  val Td = TDouble
  val nothing: AbstractSemantics.Res[Bag[Send]] = Set(Bag())

  val l1 = NIL(Ti)

  testType("Empty list int", l1, TList(Ti))


  val l2 = (1, Ti) :: (2, Ti) :: (3, Ti) :: NIL(Ti)
  testType("Nonempty list int", l2, TList(Ti))

  val l1d = NIL(Td)
  testType("Empty list double", l1d, TList(Td))


  val l2d = (1.0, Td) :: (2.0, Td) :: (3.0, Td) :: NIL(Td)
  testType("Nonempty list double", l2d, TList(Td))

  val l3 = (1, Ti) :: (2.0, Td) :: NIL(Ti)
  test("Heterogenuous list") {
    intercept[TypeCheckException] {
      typeCheck(Map(), Set(), l3)
    }
  }

  testInterp("Nonempty list double", Par(Spawn(TApp(PRINT_SERVER, Td))~>'PRINT!!(l2d)), Set(Bag(PRINT(LIST(Td, 1.0, 2.0, 3.0)))))
  testInterp("Nonempty elemAt", Par(Spawn(TApp(PRINT_SERVER, Td))~>'PRINT!!(l2d.elemAt(Td)(1))), Set(Bag(PRINT(2.0))))


  val polymorph = TAbs('alpha, Spawn(TApp(PRINT_SERVER, 'alpha))~>'PRINT!!(NIL('alpha)))

  testInterpUntyped("Polymorphic", Par(TApp(polymorph, Ti)).eraseType ,  Set(Bag(PRINT_NO(LIST(Ti).eraseType))) )
}