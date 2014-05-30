package djc.lang.lib

import djc.lang.typ.Types._
import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived._

import util.Bag

object Function {

  val consumeType = TUniv('A, TSvc(TVar('A)))
  val consume =
    Def('Id, TUniv('A, TSrv('id -> TSvc(TVar('A)))),
      TAbs('A, 
        ServerImpl(Rule(Bag(Pattern('id, 'x -> TVar('A))), Par()))),
      TAbs('B, ServiceRef(TApp(Var('Id), TVar('B)), 'id)))

}