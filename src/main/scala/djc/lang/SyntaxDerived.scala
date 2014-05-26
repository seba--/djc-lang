package djc.lang

import util.Bag
import djc.lang.Syntax._

object SyntaxDerived {

  object Def {
    def apply(x: Symbol, s: Exp, p: Exp) = {
      val srv = ServerImpl(Rule(Bag(Pattern('def, x)), p))
      val svc = ServiceRef(srv, 'def)
      Send(svc, s)
    }
  }

}