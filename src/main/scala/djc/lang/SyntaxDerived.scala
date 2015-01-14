package djc.lang

import util.Bag
import djc.lang.Syntax._

object SyntaxDerived {

  implicit def makeParSend(s: Send) = Par(s)

  def Let(x: Symbol, s: Exp, p: Exp): Exp = {
    val srv = SpawnImg(ServerImpl(Rule(Bag(Pattern('def, x)), p)))
    Send(ServiceRef(srv, 'def), s)
  }

  def LocalService(p: Pattern, e: Exp) = ServiceRef(SpawnImg(true,ServerImpl(Rule(Bag(p), e))), p.name)

  def Lambda(x: Symbol, e: Exp): Exp =
    SpawnImg(true, ServerImpl(
      Rule(
        Bag(Pattern('app, x, 'k)),
        CPS(e, Var('k)))))

  def CPS(e: Exp, k: Exp): Exp = e match {
    case App(f, a) => App(f, a, k)
    case _ => Send(k, e)
  }

  case class App(f: Exp, arg: Exp) extends Exp
  def App(f: Exp, a: Exp, k: Exp) =
    CPS(f, LocalService(Pattern('k, 'vf),
      CPS(a, LocalService(Pattern('k, 'va),
        Send(ServiceRef(Var('vf), 'app), Var('va), k)))))


}
