package djc.lang.typ.inference

import djc.lang.Gensym
import util.Bag


object ExtSyntaxDerived {
  import ExtLanguage._
  import ExtLanguage.types._
  import ExtLanguage.op._

  implicit def makeParSend(s: Send) = Par(s)

  // undelimited CPS function type:
  //   t * (u -> Unit) -> Unit
  def TFun(p: (Type,Type)): TSvc = TFun(p._1, p._2)
  def TFun(t: Type, u: Type) = TSvc(t, TSvc(u))
  def TFun(t1: Type, t2: Type, u: Type) = TSvc(t1, t2, TSvc(u))

  implicit class SymbolAssignment(val x: Symbol) extends AnyVal {
    def <--(that: Exp): (Symbol, that.type) = (x, that)
  }

  def Letk(x: Symbol, xt: Type, s: Send)(p: Exp): Exp = {
    val cont = SpawnLocal(ServerImpl(Rule('$letkont ? (x -> xt), p))) ~> '$letkont
    This(Send(s.rcv, s.args :+ cont))
  }

  def Letk(p: (Symbol, Send))(body: Exp): Exp = Letk(p._1, p._2)(body)

  def Letk(x: Symbol, s: Send)(body: Exp): Exp = {
    val cont = USpawnLocal(UServerImpl(URule('$letkont??(x), body))) ~> '$letkont
    This(Send(s.rcv, s.args :+ cont))
  }

  def Let(x: Symbol, xt: Type, e1: Exp)(e2: Exp): Exp =
    This(LocalService(Pattern('$let, x -> xt), e2)!!(e1))

  object Let {
    val letSymbol = '$let
    def apply(p: (Symbol, Exp))(body: Exp): Exp = Let(p._1, p._2)(body)
    def apply(x: Symbol, e1: Exp)(body: Exp): Exp =
      This(ULocalService(letSymbol ?? (x), body) !! (e1))

    def unapply(e: Exp): Option[(Symbol, Exp, Exp)] = e match {
      case Send(ServiceRef(Spawn(true, UServerImpl(List(URule(ps, body)))), `letSymbol`), List(e1)) if ps.size == 1 =>
        val UPattern(`letSymbol`, List(x)) = ps.head
        Some((x, e1, body))
      case _ => None
    }

  }
  //Preserves the meaning of 'this in nested local control expressions.
  //A local control expression is a ServerImpl if it has only a single rule with
  //a singleton join pattern and its service name starts with "$".
  object This {
    val letSymbol = '$let$

    def apply(e: Exp): Exp = {
      val self = Gensym.gensym('$self, freeVars(e))

      //Modified substitution function which lets substs for 'this' pass into
      //rule bodies of local control expressions.
      object PatchThisRefs extends Subst('this, Var(self)) {
        def isThisTransparentName(sym: Symbol) = sym.name.startsWith("$") && !sym.name.endsWith("$")

        override def map: TMapE = {
          case UServerImpl(List(URule(ps, p)))
            if ps.size == 1 && isThisTransparentName(ps.head.name) =>
            val UPattern(name, params) = ps.head
            if (params.isEmpty)
              ServerImpl(List(Rule(Bag(name?()), map(p))))
            else
              UServerImpl(List(URule(ps, map(p))))

          case ServerImpl(List(Rule(ps, p)))
            if ps.size == 1 && isThisTransparentName(ps.head.name) =>
            val Pattern(name, params) = ps.head
            ServerImpl(List(Rule(Bag(Pattern(name, params)), map(p))))

          case e => super.map(e)
        }
      }

      ULocalService(letSymbol??(self), PatchThisRefs(e))!!('this)
    }

    def unapply(e: Exp): Option[(UServerImpl, Exp)] = e match {
      case Send(ServiceRef(Spawn(true, srv@UServerImpl(_)), `letSymbol`), List(e1)) => Some((srv, e1))
      case _ => None
    }
  }

  def Service(p: Pattern, e: Exp) = ServiceRef(Server(Rule(Bag(p), e)), p.name)
  def LocalService(p: Pattern, e: Exp) = ServiceRef(LocalServer(Rule(Bag(p), e)), p.name)
  def ULocalService(p: UPattern, e: Exp) = ServiceRef(ULocalServer(URule(Bag(p), e)), p.name)

  //TODO patch 'this in lambda bodies
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

  def Lambda(x: Symbol, e: Exp): Exp =
    USpawnLocal(UServerImpl(
      URule('app??(x, 'k)){
        'k!!(e)
      }
    ))~>'app

  def Lambda(xs: Symbol*)(e: Exp): Exp = {
    val args = (xs :+ 'k).toSeq
    USpawnLocal(UServerImpl(
      URule('app??(args:_*)){
        'k!!(e)
      }
    ))~>'app
  }

  val TThunk = TSrvRep('$force -> ?())
  def Thunk(e: Exp) =
    ServerImpl(Rule(Bag(Pattern('$force)), e))

  //can write Ifc(cond) { thenexp } Else { elseexp }
  def Ifc(c: Exp)(t: Exp) = new PartialIf((c,t))
  def Ifc(c: Symbol)(t: Exp) = new PartialIf((c,t))

  class PartialIf(val condThen: (Exp, Exp)) extends AnyVal {
    def Else(e: Exp): Exp =
      This(Send(
        ServiceRef(
          SpawnLocal(
            BaseCall(djc.lang.base.Bool.If.toFamily(ExtLanguage),
              condThen._1,
              Thunk(condThen._2),
              Thunk(e)
            )),
          '$force)))
  }
}