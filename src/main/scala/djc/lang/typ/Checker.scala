package djc.lang.typ

import djc.lang.Gensym._
import djc.lang.TypedSyntax._
import djc.lang.typ.Types._

object Checker {
  type Context = Map[Symbol, Type]

  case class TypeCheckException(msg: String) extends RuntimeException(msg)

  def typeCheck(gamma: Context, boundTv: Set[Symbol], p: Exp): Type = p match {
    case Def(x, s, p2) =>
      val t = typeCheck(gamma, boundTv, s)
      typeCheck(gamma + (x -> t), boundTv, p2)

    case Par(ps)
      if ps.map(typeCheck(gamma, boundTv, _)) forall (_ === Unit) =>
      Unit

    case Send(rcv, args) =>
      (typeCheck(gamma, boundTv, rcv), args.map(typeCheck(gamma, boundTv, _))) match {
        case (TSvc(ts1), ts2) if ts1.corresponds(ts2)(_ === _) =>
          Unit
        case x => throw TypeCheckException(s"typeCheck failed at $p\ngamma: $gamma\nboundTv: $boundTv\nwith $x")
      }

    case Var(x) if gamma.contains(x) =>
      gamma(x)

    case ServiceRef(srv, x) =>
      typeCheck(gamma, boundTv, srv) match {
        case TSrv(svcs) if svcs.contains(x) =>
          svcs(x)
        case x => throw TypeCheckException(s"typeCheck failed at $p\ngamma: $gamma\nboundTv: $boundTv\nwith $x")
      }

    case srv@ServerImpl(rules)
      if (rules.map {
        r =>
          typeCheck(gamma ++ r.rcvars + ('this -> srv.signature), boundTv, r.p)
      } forall (_ === Unit))
        && (FreeTypeVars(srv.signature) subsetOf boundTv) =>

      srv.signature

    case TApp(p2, t) if FreeTypeVars(t) subsetOf boundTv =>
      typeCheck(gamma, boundTv, p2) match {
        case TUniv(alpha, t2) =>
          SubstType(alpha, t)(t2)

        case x => throw TypeCheckException(s"typeCheck failed at $p\ngamma: $gamma\nboundTv: $boundTv\nwith $x")
      }

    case TAbs(alpha, p1) =>
      val dontSubst = !boundTv(alpha)
      lazy val alphafresh = gensym(alpha, boundTv)
      lazy val p1fresh = SubstType(alpha, TVar(alphafresh))(p1)
      val (alphares, p1res) = if (dontSubst) (alpha, p1) else (alphafresh, p1fresh)
      val t = typeCheck(gamma, boundTv + alphares, p1res)

      TUniv(alphares, t)

    case _ =>
      throw TypeCheckException(s"typeCheck failed at $p\ngamma: $gamma\nboundTv: $boundTv")
  }
}