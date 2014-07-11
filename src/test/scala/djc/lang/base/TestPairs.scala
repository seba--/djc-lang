package djc.lang.base

import djc.lang.AbstractTest
import djc.lang.TypedSyntax._
import djc.lang.base.Bool._
import djc.lang.base.Double._
import djc.lang.base.Integer._
import djc.lang.base.Pairs._
import djc.lang.sem._
import djc.lang.typ.Checker._
import djc.lang.typ.Types._
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

  val p1 = pair(1.0 -> Td, 2 -> Ti)

  testType("double int pair", p1, TPair(Td, Ti))

  val p2 = pair(1.0 -> Td, 2 -> Ti, tru -> Tb, 1337 -> Ti)
  testType("quadruple double int bool int", p2, TTuple(Td, Ti, Tb, Ti))

  val p3 = pair(1.0 -> Tb, 2 -> Tb)
  test("wrong args") {
    intercept[TypeCheckException] {
      typeCheck(Map(), Set(), p3)
    }
  }

  def p(t: Type, e: Exp): Par = Par(PRINT(t, e))
  def res(sends: Exp*): AbstractSemantics.Res[Bag[Send]] = Set(Bag(sends.map(PRINT):_*))

  testInterp("fst",  p(Td, p1.fst(Td,Ti)), res(1.0))
  testType("fst", p1.fst(Td,Ti), Td)
  testInterp("snd",  p(Ti, p1.snd(Td,Ti)), res(2))
  testType("snd", p1.snd(Td,Ti), Ti)
  testInterp("thrd", p(TPair(Tb,Ti), p2.thrd(Td, Ti, TPair(Tb, Ti))), res(pair(tru -> Tb, 1337 -> Ti)))
  testType("thrd", p2.thrd(Td, Ti, TPair(Tb,Ti)), TPair(Tb,Ti))
  testInterp("frth",  p(Ti, p2.frth(Td, Ti, Tb, Ti)), res(1337))
  testType("frth", p2.frth(Td, Ti, Tb, Ti), Ti)
}