package djc.lang.base

import djc.lang.AbstractTest
import djc.lang.TypedLanguage._
import djc.lang.TypedLanguage.types.{TPair => _, _}
import djc.lang.base.Bool._
import djc.lang.base.Double._
import djc.lang.base.Integer._
import djc.lang.base.Pairs._
import djc.lang.sem._
import djc.lang.typ.Checker._
import util.Bag

class TestPairs1 extends TestPairs(nondeterm_1_subst.Semantics)
//class TestLambda2 extends TestPairs(nondeterm_2_env.Semantics)
//class TestLambda3 extends TestPairs(nondeterm_3_routed.SemanticsFactory)
//class TestLambda4 extends TestPairs(nondeterm_4_grouped.SemanticsFactory)
//class TestLambda5 extends TestPairs(nondeterm_5_parallel.SemanticsFactory)
//class TestLambda6 extends TestPairs(concurrent_6_thread.SemanticsFactory)


class TestPairs[V](sem: ISemanticsFactory[V]) extends AbstractTest(sem) {
  val Ti = TInteger
  val Td = TDouble
  val Tb = TBool

  val p1 = pair(1.0, 2)

  testType("double int pair", p1, TPair(Td, Ti))

  val p2 = pair(1.0, 2, tru, 1337)
  testType("quadruple double int bool int", p2, TTuple(Td, Ti, Tb, Ti))

  val p3 = pair(1.0 -> Tb, 2 -> Tb)
  test("wrong args") {
    intercept[TypeCheckException] {
      typeCheck(Map(), Map(), Map(), p3)
    }
  }

  def p(t: Type, e: Exp): Par = Par(PRINT(t, e))
  def res(sends: Exp*): AbstractSemantics.Res[Bag[Send]] = Set(Bag(sends.map(PRINT):_*))

  testInterp("fst",  p(Td, p2.fst), res(1.0))
  testType("fst", p2.fst, Td)
  testInterp("snd",  p(Ti, p2.snd), res(2))
  testType("snd", p2.snd, Ti)
  testInterp("thrd", p(Tb, p2.thrd), res(tru))
  testType("thrd", p2.thrd, Tb)
  testInterp("frth",  p(Ti, p2.frth), res(1337))
  testType("frth", p2.frth, Ti)
}
