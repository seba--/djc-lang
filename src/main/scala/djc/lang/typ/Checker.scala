package djc.lang.typ

import djc.lang.Gensym._
import djc.lang.TypedSyntax._
import djc.lang.typ.Types._

object Checker {
  type Context = Map[Symbol, Type]

  case class TypeCheckException(msg: String) extends RuntimeException(msg)

  def typeCheck(gamma: Context, boundTv: Set[Symbol], p: Exp): Type = p match {
    case BaseCall(b, es) => {
      val ts = es map (typeCheck(gamma, boundTv, _))
      if (ts.corresponds(b.ts)(_ === _))
        b.res
      else
        throw TypeCheckException(s"Arguments of base call mismatch. Was: $ts, Expected: ${b.ts}")
    }

    case Par(ps)
      if ps.map(typeCheck(gamma, boundTv, _)) forall (_ === Unit) =>
      Unit

    case Seq(Nil) => Unit
    case Seq(ps)
      if {
        val heads = ps.slice(0, ps.length - 1)
        heads.map(typeCheck(gamma, boundTv, _)) forall (_ === Unit)
      } => typeCheck(gamma, boundTv, ps.last)

    case Send(rcv, args) => {
      val trcv: TSvc = typeCheck(gamma, boundTv, rcv) match {
        case t: TSvc => t
        case t => throw TypeCheckException(s"Illegal receiver type. Expected: TSvc(_), was: $t")
      }
      val targs = args.map(typeCheck(gamma, boundTv, _))
      if (!trcv.params.corresponds(targs)(_===_))
        throw TypeCheckException(s"Send arguments have wrong types for receiver \n  $rcv.\nArguments: $args\nExpected: ${trcv.params}\nWas: $targs")
      Unit
    }

    case Var(x) =>
      gamma.getOrElse(x, throw TypeCheckException(s"Unbound variable $x"))

    case ServiceRef(srv, x) =>
      typeCheck(gamma, boundTv, srv) match {
        case TSrv(svcs) if svcs.contains(x) =>
          svcs(x)
        case x => throw TypeCheckException(s"typeCheck failed at $p\ngamma: $gamma\nboundTv: $boundTv\nwith $x")
      }

    case srv@ServerImpl(rules,_) => {
      val ruleTypes = rules map (typecheckRule(gamma, boundTv, _, srv.signature))

      if (!(FreeTypeVars(srv.signature) subsetOf boundTv))
        throw TypeCheckException(s"Illegal free type variables: ${FreeTypeVars(srv.signature) -- boundTv}")

      srv.signature
    }


    case TApp(p2, t) if FreeTypeVars(t) subsetOf boundTv =>
      typeCheck(gamma, boundTv, p2) match {
        case TUniv(alpha, bound, t2) if t <:< bound =>
          SubstType(alpha, t)(t2)

        case x => throw TypeCheckException(s"typeCheck failed at $p\ngamma: $gamma\nboundTv: $boundTv\nwith $x")
      }

    case TAbs(alpha, bound1, p1) =>
      val dontSubst = !boundTv(alpha)
      lazy val alphafresh = gensym(alpha, boundTv)
      lazy val p1fresh = SubstType(alpha, TVar(alphafresh))(p1)
      val (alphares, p1res) = if (dontSubst) (alpha, p1) else (alphafresh, p1fresh)
      val t = typeCheck(gamma, boundTv + alphares, p1res)

      TUniv(alphares, bound1, t)

    case TCast(e, t) => {
      typeCheck(gamma, boundTv, e)
      t
    }

    case _ =>
      throw TypeCheckException(s"typeCheck failed at $p\ngamma: $gamma\nboundTv: $boundTv")
  }


  def typecheckRule(gamma: Context, boundTv: Set[Symbol], r: Rule, srvSignature: TSrv): Type = {
//    val t = typeCheck(gamma ++ srvSignature.svcs ++ r.rcvars, boundTv, r.p)
    val t = typeCheck(gamma ++ r.rcvars + ('this -> srvSignature), boundTv, r.p)
    if (!(t === Unit))
      throw TypeCheckException(s"Illegal rule type in rule $r, expected: Unit, was: $t")
    t
  }
}