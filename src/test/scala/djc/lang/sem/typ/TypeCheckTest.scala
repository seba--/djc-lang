package djc.lang.sem.typ

import org.scalatest.FunSuite
import util.Bag

import djc.lang.typ.Types._
import djc.lang.typ.Checker._
import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived._


class TypeCheckTest extends FunSuite {

  val gamma: Context = Map('n6 -> TBase('Int), 'n7 -> TBase('Int), 'echo -> TSvc(TBase('Int)))

  def cellTypePrivate(t: Type) = TSrv(TSrvRep('get -> TSvc(TSvc(t)), 'set -> TSvc(t), 's -> TSvc(t)))
  def cellTypePublic(t: Type) = TSrv(TSrvRep('get -> TSvc(TSvc(t)), 'set -> TSvc(t)))

  val cellServer =
    ServerImpl(
      Rule(Bag(
          'get?('k -> ?(TVar('V))),
          's?('v -> TVar('V))),
        Par(
          Send('k, 'v),
          Send('this~>'s, 'v))),
      Rule(Bag(
          Pattern('set, 'u -> TVar('V)),
          Pattern('s, 'v -> TVar('V))),
        Send('this~>'s, 'u)))

  val cellFactoryServerType = TUniv('V, TSrvRep('mkCell -> TSvc(TVar('V), TSvc(cellTypePrivate(TVar('V)))))) // should return cellTypePublic
  val cellFactoryServer =
    TAbs('V,
      ServerImpl(
        Rule(
          Bag(Pattern('mkCell, 'init -> TVar('V), 'cont -> TSvc(cellTypePrivate(TVar('V))))),
          Let('cell, cellTypePrivate(TVar('V)), Spawn(cellServer))(
            Par(
              Send(
                ServiceRef(Var('cell), 's),
                Var('init)),
              Send(Var('cont), Var('cell)))))))




  test("cellServer typeCheck") {
    assert(cellTypePrivate(TVar('V)).rep === typeCheck(Map(), Map(), cellServer))
  }

  test("cellFactoryServer typeCheck factory") {
    assert(cellFactoryServerType === typeCheck(Map(), Map(), cellFactoryServer))
  }

  // TODO reinsert these tests, add type annotations
//  test("typeCheck should fail") {
//    val term = Def('x, TAbs('alpha, ServerImpl(Rule(Bag(Pattern('start, 'arg -> TVar('alpha), 'st -> TSvc(TVar('alpha)))),
//      Def('y, TAbs('alpha, ServerImpl(Rule(Bag(Pattern('bla)), Send(ServiceRef(Var('this), 'bla))),
//        Rule(Bag(Pattern('foo, 'b -> TVar('alpha))), Send(Var('st), Var('b))))),
//        Send(ServiceRef(TApp(Var('y), TVar('beta)), 'bla)))))),
//      Send(ServiceRef(TApp(Var('x), TBase('Int)), 'start), Var('n7), Var('echo)))
//
//
//    intercept[TypeCheckException] {
//      typeCheck(gamma, Set('beta), term)
//    }
//  }
//
//  test("typeCheck should pass") {
//    val term = Def('x, TAbs('alpha, ServerImpl(Rule(Bag(Pattern('start, 'arg -> TVar('alpha), 'st -> TSvc(TVar('alpha)))),
//      Def('y, TAbs('beta, ServerImpl(Rule(Bag(Pattern('bla)), Send(ServiceRef(Var('this), 'bla))),
//        Rule(Bag(Pattern('foo, 'b -> TVar('alpha))), Send(Var('st), Var('b))))),
//        Send(ServiceRef(TApp(Var('y), TVar('beta)), 'bla)))))),
//      Send(ServiceRef(TApp(Var('x), TBase('Int)), 'start), Var('n7), Var('echo)))
//
//    assertResult(Unit) {
//      typeCheck(gamma, Set('beta), term)
//    }
//  }


}
