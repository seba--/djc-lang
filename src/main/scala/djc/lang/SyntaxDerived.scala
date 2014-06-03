package djc.lang

import util.Bag
import djc.lang.Syntax._

object SyntaxDerived {

  implicit def makeParSend(s: Send) = Par(s)

  def Def(x: Symbol, s: Exp, p: Exp): Exp = {
    val srv = Spawn(ServerImpl(Rule(Bag(Pattern('def, x)), p)))
    Send(ServiceRef(srv, 'def), s)
  }


}