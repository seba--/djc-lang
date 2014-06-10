package djc.lang.lib

import util.Bag

import djc.lang.sem._
import djc.lang.AbstractTest

import djc.lang.Syntax
import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived._
import djc.lang.typ.Types._
import djc.lang.TypedSyntax.Var
import djc.lang.TypedSyntax.ServiceRef
import djc.lang.TypedSyntax.Rule
import djc.lang.TypedSyntaxDerived._


class TestLambda1 extends TestLambda(nondeterm_1_subst.Semantics)
class TestLambda2 extends TestLambda(nondeterm_2_env.Semantics)
class TestLambda3 extends TestLambda(nondeterm_3_routed.SemanticsFactory)
class TestLambda4 extends TestLambda(nondeterm_4_grouped.SemanticsFactory)
class TestLambda5 extends TestLambda(nondeterm_5_parallel.SemanticsFactory)
class TestLambda6 extends TestLambda(concurrent_6_thread.SemanticsFactory)


class TestLambda[V](sem: ISemanticsFactory[V]) extends AbstractTest(sem) {


  val xt1 = ?()
  val rt1 = xt1
  val lam1 = Lambda('x, xt1, Var('x), rt1)

  testType("lam1", lam1, TFun(xt1, rt1))

  val fooService = ServiceRef(
    LocalServer(Rule(
      Bag(Pattern('foo)),
      Par())),
    'foo)
  val resultService = ServiceRef(
    LocalServer(Rule(
      Bag(Pattern('bar, 'result -> rt1)),
      'result!!())),
    'bar
  )
  val app1 = App(lam1, fooService, resultService)

  testType("app1", app1, Unit)
  testInterp("app1", app1, Set(Bag[Syntax.Send]()))


  val fooPrintService = ServiceRef(
    LocalServer(Rule(
      Bag(Pattern('foo)),
      Send(Spawn(PRINT_SERVER(?()))~>'PRINT, 'this~>'foo))),
    'foo)
  val app2 = App(lam1, fooPrintService, resultService)
  testType("app2", app2, Unit)
  testInterp("app2", app2, Set(Bag(PRINT(fooPrintService))))


//  Set(
//    Bag(
//      Send(
//        ServiceRef(
//          Spawn(false,ServerImpl(Bag(
//            Rule(Bag(Pattern('PRINT)),Par())))),
//          'PRINT),
//        List(
//          ServiceRef(
//            Spawn(false,ServerImpl(Bag(
//              Rule(Bag(Pattern('foo)),Send(ServiceRef(Spawn(false,ServerImpl(Bag(Rule(Bag(Pattern('PRINT)),Par())))),'PRINT),List(ServiceRef(Var('this),'foo))))))),
//            'foo)))))
//
//  Set(
//    Bag(
//      Send(
//        ServiceRef(
//          Spawn(false,ServerImpl(Bag(
//            Rule(Bag(Pattern('PRINT)),Par())))),
//          'PRINT),
//        List(
//          ServiceRef(
//            Spawn(true,ServerImpl(Bag(Rule(Bag(Pattern('foo)),Send(ServiceRef(Spawn(false,ServerImpl(Bag(Rule(Bag(Pattern('PRINT,List())),Par())))),'PRINT),List(ServiceRef(Var('this),'foo))))))),'foo)))))

}