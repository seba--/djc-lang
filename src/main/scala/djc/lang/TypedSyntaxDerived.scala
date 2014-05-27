package djc.lang

import util.Bag
import djc.lang.TypedSyntax._
import djc.lang.typ.Types._
import djc.lang.typ.Checker._

object TypedSyntaxDerived {

  // undelimited CPS function type:
  //   t * (u -> Unit) -> Unit
  def TFun(t: Type, u: Type) = TSvc(t, TSvc(u))

  def Def(x: Symbol, xt: Type, s: Exp, p: Exp): Exp = {
    val srv = ServerImpl(Rule(Bag(Pattern('def, (x, xt))), p))
    val svc = ServiceRef(srv, 'def)
    Send(svc, s)
  }

  def Def(x: Symbol, s: Exp, p: Exp, gamma: Context, boundTv: Set[Symbol]): Exp = {
    val xt = typeCheck(gamma, boundTv, s)
    Def(x, xt, s, p)
  }


  def Lambda(x: Symbol, xt: Type, e: Exp, resT: Type): Exp =
    Def('App, TSrv('app -> TSvc(xt, TSvc(resT))),
      ServerImpl(
        Rule(
          Bag(Pattern('app, x -> xt, 'cont -> TSvc(resT))),
          Send(Var('cont), e))),
      ServiceRef(Var('App), 'app))

  def App(f: Exp, arg: Exp, cont: Exp): Exp =
    Send(f, arg, cont)


  def Thunk(e: Exp) =
    Def('Thunk, TSrv('force -> TSrv()),
      ServerImpl(Rule(
        Bag(Pattern('force)),
        e)),
      ServiceRef(Var('Thunk), 'force))

  def Ifc(c: Exp, t: Exp, e: Exp) =
    Send(BaseCall(djc.lang.base.Bool.If,
      c,
      Thunk(t),
      Thunk(e)))
}