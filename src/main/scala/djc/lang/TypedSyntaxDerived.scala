package djc.lang

import util.Bag
import djc.lang.TypedSyntax._
import djc.lang.typ.Types._
import djc.lang.typ.Checker._

object TypedSyntaxDerived {

  implicit def makeParSend(s: Send) = Par(s)

  // undelimited CPS function type:
  //   t * (u -> Unit) -> Unit
  def TFun(t: Type, u: Type) = TSvc(t, TSvc(u))

  def Def(x: Symbol, xt: Type, s: Exp, p: Exp): Exp = {
    val srv = ServerImpl(Rule(Bag(Pattern('def, x -> xt)), p))
    Send(srv~>'def, s)
  }

  def Def(x: Symbol, s: Exp, p: Exp, gamma: Context, boundTv: Set[Symbol]): Exp = {
    val xt = typeCheck(gamma, boundTv, s)
    Def(x, xt, s, p)
  }

  def LambdaServer(x: Symbol, xt: Type, e: Exp, resT: TSrv, init: Exp=Par()): Exp =
    ServiceRef(
      ServerImpl(
        Rule(
          Bag(Pattern('app, x -> xt, 'result-> ?(resT))),
          Def('SERVER, resT, e,
            Par(init!!'SERVER, 'result!!'SERVER)))),
      'app)


  def Lambda(x: Symbol, xt: Type, e: Exp, resT: Type): Exp =
    ServiceRef(
      ServerImpl(
        Rule(
          Bag(Pattern('app, x -> xt, 'result-> ?(resT))),
          'result!!e)),
      'app)



  def App(f: Exp, arg: Exp, cont: Exp) =
    Send(f, arg, cont)


  def ThunkWrong(e: Exp) =
    Def('Thunk, TSrv('force -> TSrv()),
      ServerImpl(Rule(
        Bag(Pattern('force)),
        e)),
      ServiceRef(Var('Thunk), 'force))

  def Thunk(e: Exp) =
    ServiceRef(
      ServerImpl(Rule(
        Bag(Pattern('force)),
        e)),
      'force)

  def Ifc(c: Exp, t: Exp, e: Exp) =
    Send(BaseCall(djc.lang.base.Bool.If,
      c,
      Thunk(t),
      Thunk(e)))
}