package djc.lang.sem.typesystem

import org.scalatest.FunSuite
import util.Bag


class TypeCheckTest extends FunSuite {
  import TypedFlatSyntax._

  val gamma : Context = Map('n6 -> TBase('Int), 'n7 -> TBase('Int), 'echo -> TSvc(TBase('Int)))

  def mkCell(prog : Prog) : Prog = {
    val cell = Def('cell, ServerImpl(Rule(Bag(Pattern('get, 'k -> TSvc(TVar('V))),
                                              Pattern('s, 'v -> TVar('V))), Par(Send(Var('k), Var('v)),
                                                                                Send(ServiceRef(Var('this), 's), Var('v)))),
                                     Rule(Bag(Pattern('set, 'u -> TVar('V),
                                                            'k -> TSvc()),
                                              Pattern('s, 'v -> TVar('V))), Par(Send(Var('k)),
                                                                            Send(ServiceRef(Var('this), 's), Var('u))))),
                          Par(Send(Var('k), ServiceRef(Var('cell), 'get), ServiceRef(Var('cell), 'set)),
                              Send(ServiceRef(Var('cell), 's), Var('v))))

    Def('factory, TAbs('V, ServerImpl(Rule(Bag(Pattern('mkCell, 'v -> TVar('V),
                                                                'k -> TSvc(TSvc(TSvc(TVar('V))), TSvc(TVar('V), TSvc())))),
                  cell))),
                  prog)
    }


  test("mkCell typeCheck") {
    assertResult(Unit) {
      typeCheck(Map(), Set(), mkCell(Par()))
    }
  }

  test("mkCell typeCheck factory") {
    val t = TUniv('V,TSrv('mkCell -> TSvc(TVar('V), TSvc(TSvc(TSvc(TVar('V))), TSvc(TVar('V), TSvc())))))

    assertResult(t) {
      typeCheck(Map(), Set(), mkCell(Var('factory)))
    }
  }

  test("typeCheck should fail") {
    val term = Def('x, TAbs('alpha, ServerImpl(Rule(Bag(Pattern('start, 'arg -> TVar('alpha), 'st -> TSvc(TVar('alpha)))),
                                                      Def('y, TAbs('alpha, ServerImpl(Rule(Bag(Pattern('bla)), Send(ServiceRef(Var('this), 'bla))),
                                                                                      Rule(Bag(Pattern('foo, 'b -> TVar('alpha))), Send(Var('st), Var('b))))),
                                                              Send(ServiceRef(TApp(Var('y), TVar('beta)), 'bla)))))),
      Send(ServiceRef(TApp(Var('x), TBase('Int)), 'start), Var('n7), Var('echo)))


    intercept[TypeCheckException] {
      typeCheck(gamma , Set('beta), term)
    }
  }

  test("typeCheck should pass") {
    val term = Def('x, TAbs('alpha, ServerImpl(Rule(Bag(Pattern('start, 'arg -> TVar('alpha), 'st -> TSvc(TVar('alpha)))),
      Def('y, TAbs('beta, ServerImpl(Rule(Bag(Pattern('bla)), Send(ServiceRef(Var('this), 'bla))),
        Rule(Bag(Pattern('foo, 'b -> TVar('alpha))), Send(Var('st), Var('b))))),
        Send(ServiceRef(TApp(Var('y), TVar('beta)), 'bla)))))),
      Send(ServiceRef(TApp(Var('x), TBase('Int)), 'start), Var('n7), Var('echo)))

     assertResult(Unit) {
       typeCheck(gamma , Set('beta), term)
     }
  }




}
