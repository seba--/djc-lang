package djc.lang.sem.typesystem

import org.scalatest.FunSuite
import util.Bag


class TypeCheckTest extends FunSuite {
  import TypedFlatSyntax._

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


  test("mkCell typecheck") {
    assertResult(Unit) {
      typeCheck(Map(), Set(), mkCell(Par()))
    }
  }

  test("mkCell typecheck factory") {
    val t = TUniv('V,TSrv('mkCell -> TSvc(TVar('V), TSvc(TSvc(TSvc(TVar('V))), TSvc(TVar('V), TSvc())))))

    assertResult(t) {
      typeCheck(Map(), Set(), mkCell(Var('factory)))
    }
  }



}
