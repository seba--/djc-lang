package djc.lang

import djc.lang.typ.{FreeVars, SubstTemplate}
import util.Bag
import djc.lang.TypedSyntax._
import djc.lang.typ.Types._

object TypedSyntaxDerived {

  implicit def makeParSend(s: Send) = Par(s)

  // undelimited CPS function type:
  //   t * (u -> Unit) -> Unit
  def TFun(t: Type, u: Type) = TSvc(t, TSvc(u))

  def TFun(t1: Type, t2: Type, u: Type) = TSvc(t1, t2, TSvc(u))

  //  def Let(x: Symbol, xt: Type, s: Exp, p: Exp): Exp = {
  //    val srv = LocalServer(Rule(Bag(Pattern('def, x -> xt)), p))
  //    Send(srv~>'def, s)
  //  }

  def Letk(x: Symbol, xt: Type, s: Send)(p: Exp): Exp = {
    val cont = SpawnLocal(ServerImpl(Rule('$letkont ? (x -> xt), p))) ~> '$letkont
    Send(s.rcv, s.args :+ cont)
  }

  def Letk(thisType: Type)(x: Symbol, xt: Type, s: Send)(p: Exp): Exp =
    This(thisType)(Letk(x, xt, s)(p))

  def Let(x: Symbol, xt: Type, e1: Exp)(e2: Exp): Exp =
    LocalService(Pattern('$let, x -> xt), e2)!!(e1)

  def Let(thisType: Type)(x: Symbol, xt: Type, e1: Exp)(e2: Exp): Exp =
    This(thisType)(Let(x, xt, e1)(e2))

  //Preserves the meaning of 'this in nested local control expressions.
  //A local control expression is a ServerImpl if it has only a single rule with
  //a singleton join pattern and its service name starts with "$".
  def This(thisType: Type)(e: Exp): Exp = {
    val self = Gensym.gensym('self, FreeVars(e))

    //Modified substitution function which lets substs for 'this' pass into
    //rule bodies of local control expressions.
    object PatchThisRefs extends SubstTemplate('this, Var(self)) {
      def isThisTransparentName(sym: Symbol) = sym.name.startsWith("$") && !sym.name.endsWith("$")

      override def map: TMapE = {
        case ServerImpl(List(Rule(ps, p)))
          if ps.size == 1 && isThisTransparentName(ps.head.name) =>
          val Pattern(name, params) = ps.head
          ServerImpl(List(Rule(Bag(Pattern(name, params)), map(p))))

        case e => super.map(e)
      }
    }

    LocalService(Pattern('$let$, self -> thisType), PatchThisRefs(e))!!('this)
  }

  def Service(p: Pattern, e: Exp) = ServiceRef(Server(Rule(Bag(p), e)), p.name)
  def LocalService(p: Pattern, e: Exp) = ServiceRef(LocalServer(Rule(Bag(p), e)), p.name)

  def Lambda(x: Symbol, t: (Type,Type), e: Exp): Exp = Lambda(x, t._1, e, t._2)
  def Lambda(x: Symbol, xt: Type, e: Exp, resT: Type): Exp =
      SpawnLocal(ServerImpl(
        Rule(
          'app?(x -> xt, 'k-> ?(resT)),
          'k!!(e))))~>'app
  def Lambda(xs: List[(Symbol,Type)], e: Exp, resT: Type): Exp = {
    val args = (xs :+ ('k -> ?(resT))).toSeq
    SpawnLocal(ServerImpl(
      Rule(
        'app ?(args:_*),
        'k !! (e)))) ~> 'app
  }

  val TThunk = TSrvRep('$force -> ?())
  def Thunk(e: Exp) =
    ServerImpl(Rule(Bag(Pattern('$force)), e))


  //can write Ifc(cond) { thenexp } Else { elseexp }
  def Ifc(c: Exp)(t: Exp) = new PartialIf(c,t)
  def Ifc(c: Symbol)(t: Exp) = new PartialIf(c,t)

  class PartialIf(c: Exp, t: Exp) {
    def Else(e: Exp): Exp =
      Send(
        ServiceRef(
          SpawnLocal(
            BaseCall(djc.lang.base.Bool.If,
              c,
              Thunk(t),
              Thunk(e)
            )),
          '$force))
  }


  def Ifc(thisType: Type)(c: Exp)(t: Exp) =
    new PartialIf(c, t) {
      override def Else(e: Exp): Exp = {
        This(thisType)(super.Else(e))
      }
    }

}