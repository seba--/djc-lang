package djc.lang.base

import djc.lang.AbstractTest
import djc.lang.TypedSyntax._
import djc.lang.typ.Types._
import djc.lang.base.Double._
import djc.lang.base.Integer._
import djc.lang.base.Bool._
import djc.lang.base.Maps._
import djc.lang.sem._
import djc.lang.typ.Checker._
import util.Bag

class TestMaps1 extends TestMaps(nondeterm_1_subst.Semantics)
//class TestLambda2 extends TestMaps(nondeterm_2_env.Semantics)
//class TestLambda3 extends TestMaps(nondeterm_3_routed.SemanticsFactory)
//class TestLambda4 extends TestMaps(nondeterm_4_grouped.SemanticsFactory)
//class TestLambda5 extends TestMaps(nondeterm_5_parallel.SemanticsFactory)
//class TestLambda6 extends TestMaps(concurrent_6_thread.SemanticsFactory)


class TestMaps[V](sem: ISemanticsFactory[V]) extends AbstractTest(sem) {
  val Ti = TInteger
  val Td = TDouble
  val Tb = TBool

  val m1 = empty(Ti, Td)

  testType("Empty map int -> double", m1, TMap(Ti, Td))


  val m2 = empty(Ti, Td).insert(Ti, Td, (1, 1.0)).insert(Ti,Td, (2, 2.0))
  testType("Nonempty list int -> double", m2, TMap(Ti, Td))

  val m1dd = empty(Td, Td)
  testType("Empty map double -> double", m1dd, TMap(Td, Td))

  testType("contains", m1dd.hasKey(Td, Td, 1.0), TBool)

  val m3 = empty(Ti, Td).insert(Ti, Td, (1, 1.0)).insert(Ti,Ti, (2, 2))
  test("Heterogenuous map") {
    intercept[TypeCheckException] {
      typeCheck(Map(), Set(), m3)
    }
  }

  def p(t: Type, e: Exp): Par = Par(PRINT(t, e))
  def res(sends: Exp*): AbstractSemantics.Res[Bag[Send]] = Set(Bag(sends.map(PRINT):_*))

  testInterp("hasKey empty", p(Tb, m1dd.hasKey(Td,Td,1.0)), res(fal))
  testInterp("hasKey nonempty", p(Tb, m1dd.insert(Td,Td, (1.0,1.1)).hasKey(Td,Td,1.0)), res(tru))
  testInterp("get", p(Td, m2.get(Ti, Td, 2)), res(2.0))
  testInterp("remove", p(Tb, m1dd.insert(Td,Td, (1.0,1.1)).remove(Td,Td, 1.0).hasKey(Td,Td,1.0)), res(fal))
}