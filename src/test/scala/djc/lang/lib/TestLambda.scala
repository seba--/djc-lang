package djc.lang.lib

import util.Bag

import djc.lang.base.Bool
import djc.lang.sem._
import djc.lang.AbstractTest

import djc.lang.Syntax._
import djc.lang.SyntaxDerived._


class TestLambda6 extends TestLambda(concurrent_6_thread.SemanticsFactory)


class TestLambda[V](sem: ISemanticsFactory[V]) extends AbstractTest(sem) {


  val lam1 = Lambda('x, Var('x))

  val fooService = ServiceRef(
    LocalServer(Rule(
      Bag(Pattern('foo)),
      Par())),
    'foo)
  val resultService = ServiceRef(
    LocalServer(Rule(
      Bag(Pattern('bar, 'result)),
      Send(Var('result)))),
    'bar
  )
  val app1 = Send(ServiceRef(lam1, 'app), fooService, resultService)

  testInterpUntyped("app1", app1, Set(Bag[Send]()))


  val fooPrintService = ServiceRef(
    LocalServer(Rule(
      Bag(Pattern('foo)),
      Send(ServiceRef(SpawnImg(PRINT_SERVER_NO), 'PRINT), ServiceRef(Var('this), 'foo)))),
    'foo)
  val app2 = Send(ServiceRef(lam1, 'app), fooPrintService, resultService)
  testInterpUntyped("app2", app2, Set(Bag(PRINT_NO(fooPrintService))))


  val tru = Lambda('t, Lambda('f, 't))
  val fal = Lambda('t, Lambda('f, 'f))

  val toBool = LocalService('trans ?('b, 'k), App(App('b, Bool.tru.eraseType), Bool.fal.eraseType, 'k))

  testInterpUntyped("toBool tru = true", Par(toBool !!(tru, SpawnImg(PRINT_SERVER_NO) ~> 'PRINT)), Set(Bag(PRINT_NO(Bool.tru.eraseType))))
  testInterpUntyped("toBool fal = false", Par(toBool !!(fal, SpawnImg(PRINT_SERVER_NO) ~> 'PRINT)), Set(Bag(PRINT_NO(Bool.fal.eraseType))))


  val or = Lambda('p, Lambda('q, App(App('p, tru), 'q)))
  def or(p: Exp, q: Exp): Exp = App(App(or, p), q, SpawnImg(PRINT_SERVER_NO) ~> 'PRINT)

  testInterpUntyped("T or T", Par(or(tru, tru)), Set(Bag(PRINT_NO(tru))))
  testInterpUntyped("T or F", Par(or(tru, fal)), Set(Bag(PRINT_NO(tru))))
  testInterpUntyped("F or T", Par(or(fal, tru)), Set(Bag(PRINT_NO(tru))))
  testInterpUntyped("F or F", Par(or(fal, fal)), Set(Bag(PRINT_NO(fal))))

}
