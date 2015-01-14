package djc.lang.base

import djc.lang.AbstractTest
import djc.lang.TypedLanguage._
import djc.lang.TypedLanguage.types._
import djc.lang.base.Double._
import djc.lang.base.Integer._
import djc.lang.base.Bool._
import djc.lang.base.Maps._
import djc.lang.sem._
import djc.lang.typ.Checker._
import util.Bag

class TestMaps6 extends TestMaps(concurrent_6_thread.SemanticsFactory)
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


  val m2 = empty(Ti, Td).insert(1, 1.0).insert(2, 2.0)
  testType("Nonempty list int -> double", m2, TMap(Ti, Td))

  val m1dd = empty(Td, Td)
  testType("Empty map double -> double", m1dd, TMap(Td, Td))

  testType("contains", m1dd.hasKey(1.0), TBool)

  val m3 = empty(Ti, Td).insert(1, 1.0).insert(2, 2)
  test("Heterogenuous map") {
    intercept[TypeCheckException] {
      typeCheck(Map(), Map(), Map(), m3)
    }
  }

  def p(t: Type, e: Exp): Par = Par(PRINT(t, e))
  def res(sends: Exp*): AbstractSemantics.Res[Bag[Send]] = Set(Bag(sends.map(PRINT):_*))

  testInterp("hasKey empty", p(Tb, m1dd.hasKey(1.0)), res(fal))
  testType("hasKey empty", m1dd.hasKey(1.0), TBool)
  testInterp("hasKey nonempty", p(Tb, m1dd.insert(1.0, 1.1).hasKey(1.0)), res(tru))
  testType("hasKey nonempty", m1dd.insert(1.0, 1.1).hasKey(1.0), TBool)
  testInterp("get", p(Td, m2.get(2)), res(2.0))
  testType("get", m2.get(2), TDouble)
  testInterp("remove", p(Tb, m1dd.insert(1.0,1.1).remove(1.0).hasKey(1.0)), res(fal))
  testType("remove", m1dd.insert(1.0,1.1).remove(1.0).hasKey(1.0), TBool)
}