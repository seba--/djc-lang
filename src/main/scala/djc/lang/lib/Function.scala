package djc.lang.lib

import djc.lang.typ.Types._
import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived._

import util.Bag

object Function {

  val consumeType = TUniv('A, ?('A))
  val consume =
    TAbs('A,
      ServiceRef(
        ServerImpl(Rule(Bag(Pattern('consume, 'x -> TVar('A))), Par())),
        'consume))
}