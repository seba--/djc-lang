package djc.lang.lib

import util.Bag

import djc.lang.sem._
import djc.lang.AbstractTest

import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived._
import djc.lang.typ.Types._
import djc.lang.TypedSyntax.Var
import djc.lang.TypedSyntax.ServiceRef
import djc.lang.TypedSyntax.Rule


//class TestLambda1 extends TestLambda(nondeterm_1_subst.Semantics())
//class TestLambda2 extends TestLambda(nondeterm_2_env.Semantics())
//class TestLambda3 extends TestLambda(new nondeterm_3_routed.Semantics().newInstance())
//class TestLambda4 extends TestLambda(new nondeterm_4_grouped.Semantics().newInstance())
//class TestLambda5 extends TestLambda(new nondeterm_5_parallel.Semantics().newInstance())
class TestLambda6 extends TestLambda(concurrent_6_thread.SemanticsFactory, false)


class TestLambda[V](sem: ISemanticsFactory[V], nondeterm: Boolean = true) extends AbstractTest(sem, nondeterm) {


  val xt1 = ?()
  val rt1 = xt1
  val lam1 = Lambda('x, xt1, Var('x), rt1)

  testType("lam1", lam1, TFun(xt1, rt1))

  val fooService = ServiceRef(
    ServerImpl(Rule(
      Bag(Pattern('foo)),
      Par())),
    'foo)
  val resultService = ServiceRef(
    ServerImpl(Rule(
      Bag(Pattern('bar, 'result -> rt1)),
      'result!!())),
    'bar
  )
  val app1 = App(lam1, fooService, resultService)

  testType("app1", app1, Unit)
  testInterp("app1", app1, Set(Bag()))


  val fooPrintService = ServiceRef(
    ServerImpl(Rule(
      Bag(Pattern('foo)),
      Send(PRINT_SERVER(?())~>'PRINT, ServiceRef('this, 'foo)))),
    'foo)
  val app2 = App(lam1, fooPrintService, resultService)
  testType("app2", app2, Unit)
  testInterp("app2", app2, Set(Bag(PRINT(fooPrintService))))



}