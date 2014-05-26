package djc.lang

import util.Bag
import djc.lang.TypedSyntax._
import djc.lang.typ.Types._
import djc.lang.typ.Checker._

object TypedSyntaxDerived {

  object Def {
    def apply(x: Symbol, xt: Type, s: Exp, p: Exp): Exp = {
      val srv = ServerImpl(Rule(Bag(Pattern('defined, (x, xt))), p))
      val svc = ServiceRef(srv, 'defined)
      Send(svc, s)
    }

    def apply(x: Symbol, s: Exp, p: Exp, gamma: Context, boundTv: Set[Symbol]): Exp = {
      val xt = typeCheck(gamma, boundTv, s)
      apply(x, xt, s, p)
    }
  }
}