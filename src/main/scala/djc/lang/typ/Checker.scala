package djc.lang.typ

import djc.lang.Gensym
import djc.lang.Gensym._
import djc.lang.TypedSyntax._
import djc.lang.typ.Types._

object Checker {
  type Context = Map[Symbol, Type]
  type TVarContext = Map[Symbol, Type]

  case class TypeCheckException(msg: String) extends RuntimeException(msg)

  //TODO make the contexts implicit parameters?
  def subtype(tgamma: TVarContext)(tpe1: Type, tpe2: Type): Boolean = (tpe1, tpe2) match {
    case (_, Top) =>
      true

    case (Bot, _) =>
      true

    case (Unit, Unit) =>
      true

    case (TVar(alpha), TVar(beta)) if alpha == beta => //TODO should be bound in tgamma or not?
      true

    case (TVar(alpha), t) =>
      tgamma.contains(alpha) && subtype(tgamma)(tgamma(alpha), t)

    case (TUniv(alpha, u1, t1), TUniv(beta, u2, t2)) if u1 === u2 =>
      lazy val ftv1 = FreeTypeVars(t1)
      lazy val ftv2 = FreeTypeVars(t2)
      val alphares =
        if (alpha != beta && ftv2(alpha))
          Gensym(alpha, ftv1 ++ ftv2 ++ tgamma.keySet)
        else alpha

      subtype(tgamma + (alphares -> u1))(SubstType(alpha -> TVar(alphares))(t1), SubstType(beta -> TVar(alphares))(t2))

    case (TSvc(args), TSvc(args1)) =>
      args1.corresponds(args)(subtype(tgamma)(_,_))

    case (TSrvRep(svcs), TSrvRep(svcs1)) =>
      svcs1.forall { case (k,t) => svcs.contains(k) && subtype(tgamma)(svcs(k), t) }

    case (TSrv(t), TSrv(t1)) =>
      subtype(tgamma)(t, t1)

    case (TBase(name, targs), TBase(name1, targs1)) =>
      name == name1 && targs.corresponds(targs1)(_ === _) //TODO support variance?
      //TODO what about equivalence classes of Bot?

    case _ => false
  }

  def promote(tgamma: TVarContext)(tpe: Type): Type = tpe match {
    case TVar(alpha) =>
      if (!tgamma.contains(alpha))
        throw TypeCheckException(s"promotion of type variable $alpha undefined with tgamma: $tgamma")
      promote(tgamma)(tgamma(alpha))

    case _ => tpe
  }

  def meet(tgamma: TVarContext)(s: Type, t: Type): Type = (s,t) match {
    case (s,t) if subtype(tgamma)(s, t) => s

    case (s,t) if subtype(tgamma)(t, s) => t

    case (TUniv(alpha, u1, t1), TUniv(beta, u2, t2)) if u1 === u2 =>
      lazy val ftv1 = FreeTypeVars(t1)
      lazy val ftv2 = FreeTypeVars(t2)
      val alphares =
        if (alpha != beta && ftv2(alpha))
          Gensym(alpha, ftv1 ++ ftv2 ++ tgamma.keySet)
        else alpha

      TUniv(alphares, u1, meet(tgamma + (alphares -> u1))(SubstType(alpha -> TVar(alphares))(t1), SubstType(beta -> TVar(alpha))(t2)))

    case (TSvc(args), TSvc(args1)) if args.length == args1.length =>
      TSvc((args zip args1) map {case (x,y) => join(tgamma)(x,y)})

    case (TSrvRep(svcs), TSrvRep(svcs1)) =>
      val m1 = svcs.filterKeys(svcs1.contains(_))
      val m2 = svcs1.filterKeys(svcs.contains(_))
      val merge = for((k,t1) <- m1) yield k -> meet(tgamma)(t1, m2(k))
      val diff1 = svcs.filterKeys(!svcs1.contains(_))
      val diff2 = svcs1.filterKeys(!svcs.contains(_))
      TSrvRep(merge ++ diff1 ++ diff2)

    case (TSrv(t1), TSrv(t2)) =>
      TSrv(meet(tgamma)(t1,t2))

    case _ => Bot
  }

  def join(tgamma: TVarContext)(s: Type, t: Type): Type = (s,t) match {
    case (s,t) if subtype(tgamma)(s, t) => t

    case (s,t) if subtype(tgamma)(t, s) => s

    case (TVar(alpha), t) =>
      join(tgamma)(tgamma(alpha), t)

    case (s, TVar(alpha)) =>
      join(tgamma)(s, tgamma(alpha))

    case (TUniv(alpha, u1, t1), TUniv(beta, u2, t2)) if u1 === u2 =>
      lazy val ftv1 = FreeTypeVars(t1)
      lazy val ftv2 = FreeTypeVars(t2)
      val alphares =
        if (alpha != beta && ftv2(alpha))
          Gensym(alpha, ftv1 ++ ftv2 ++ tgamma.keySet)
        else alpha

      TUniv(alphares, u1, join(tgamma + (alphares -> u1))(SubstType(alpha -> TVar(alphares))(t1), SubstType(beta -> TVar(alpha))(t2)))

    case (TSvc(args), TSvc(args1)) if args.length == args1.length =>
      TSvc((args zip args1) map {case (x,y) => meet(tgamma)(x,y)})

    case (TSrvRep(svcs), TSrvRep(svcs1)) =>
      val m1 = svcs.filterKeys(svcs1.contains(_))
      val m2 = svcs1.filterKeys(svcs.contains(_))
      val merge = for((k,t1) <- m1) yield k -> join(tgamma)(t1, m2(k))
      TSrvRep(merge)

    case (TSrv(t1), TSrv(t2)) =>
      TSrv(join(tgamma)(t1,t2))

    case _ => Top
  }

  def typeCheck(gamma: Context, tgamma: TVarContext, p: Exp): Type = p match {
//    case _ if {println(s"gammma: ${gamma.keys}");false} => ?()

    case BaseCall(b, ts, es) => {
      val argTs = es map (typeCheck(gamma, tgamma, _))
      val (tArgs, bounds) = b.targs.unzip
      if (!ts.corresponds(bounds)(subtype(tgamma)(_,_)))
         throw TypeCheckException(s"Type arguments do not match type parameters. Applied $ts to ${b.targs}\n in $p")

      val sigma: Type => Type = SubstType(tArgs zip ts)(_)
      val bSig = b.ts map sigma
      if (argTs.corresponds(bSig)(subtype(tgamma)(_, _))) // actual arguments have subtypes of declared parameters
        sigma(b.res)
      else
        throw TypeCheckException(s"Arguments of base call mismatch. Was: $argTs, Expected: ${bSig}\n  in ${BaseCall(b, ts, es)}")
    }

    case Par(ps) =>
      val psTypes = ps.map(typeCheck(gamma, tgamma, _))
      val joined = psTypes.foldLeft(Unit.asInstanceOf[Type])( (t,u) => join(tgamma)(t,u))
      if (!subtype(tgamma)(joined, Unit))
        throw TypeCheckException(s"Illegal parallel composition of types $psTypes\n which joins to $joined\n in $p")
       //TODO do we handle Bot correctly here?
      Unit

    case Send(rcv, args) =>
      val targs = args.map(typeCheck(gamma, tgamma, _))

      promote(tgamma)(typeCheck(gamma, tgamma, rcv)) match {
        case trcv: TSvc =>
          if (!targs.corresponds(trcv.params)(subtype(tgamma)(_,_))) // actual send arguments have subtypes of declared parameters
            throw TypeCheckException(s"Send arguments have wrong types for receiver \n  $rcv.\nArguments: $args\nExpected: ${trcv.params}\nWas: $targs\nwith gamma: $gamma\ntgamma: $tgamma")
          Unit
        case Bot => Bot
        case t => throw TypeCheckException(s"Illegal receiver type. Expected: TSvc(_), was $t\n  in ${Send(rcv, args)}")
      }

    case Var(x) =>
      gamma.getOrElse(x, throw TypeCheckException(s"Unbound variable $x\ngamma: $gamma\ntgamma: $tgamma"))

    case ref@ServiceRef(srv, x) =>
      promote(tgamma)(typeCheck(gamma, tgamma, srv)) match {
        case Bot => Bot
        case TSrv(t) => promote(tgamma)(t) match {
          case TSrvRep(svcs) =>
            if (!svcs.contains(x))
              throw TypeCheckException(s"service $x is not a member of server templates $svcs")
              svcs(x)
          case x => throw TypeCheckException(s"typeCheck ServiceRef: expected TSrvRep(_) but got $t (which promotes to $x")
        }
        case TSrvRep(_) => throw TypeCheckException(s"Cannot refer to service of non-running server in $ref")
        case x => throw TypeCheckException(s"typeCheck failed at $p\ngamma: $gamma\ntgamma: $tgamma\nwith $x")
      }

    case srv@ServerImpl(rules) => {
      val ruleTypes = rules map (typecheckRule(gamma, tgamma, _, srv.signature))

      if (!(FreeTypeVars(srv.signature) subsetOf tgamma.keySet))
        throw TypeCheckException(s"Illegal free type variables: ${FreeTypeVars(srv.signature) -- tgamma.keySet}")

      srv.signature
    }

    case sp@Spawn(_, e) =>
      val argt = typeCheck(gamma, tgamma, e)
      promote(tgamma)(argt) match {
        case Bot => Bot
        case t: TSrvRep  => TSrv(argt)
        case t => throw TypeCheckException(s"Illegal spawn expression. Expected: TSrvRep(_), was $argt (which promotes to $t)\n  in $sp)}")
      }

    case TApp(p2, t) =>
      if (!(FreeTypeVars(t) subsetOf tgamma.keySet))
        throw TypeCheckException(s"typeCheck failed at $p\ngamma: $gamma\ntgamma: $tgamma\n  free type vars ${FreeTypeVars(t) -- tgamma.keySet}")
      promote(tgamma)(typeCheck(gamma, tgamma, p2)) match {
        case Bot => Bot
        case TUniv(alpha, bound, t2) if subtype(tgamma)(t, bound) =>
          SubstType(alpha -> t)(t2)

        case x => throw TypeCheckException(s"typeCheck failed at $p\ngamma: $gamma\ntgamma: $tgamma\nwith $x")
      }

    case TAbs(alpha, bound1, p1) =>
      val dontSubst = !tgamma.contains(alpha)
      lazy val alphafresh = gensym(alpha, tgamma.keySet)
      lazy val p1fresh = SubstType(alpha -> TVar(alphafresh))(p1)
      val (alphares, p1res) = if (dontSubst) (alpha, p1) else (alphafresh, p1fresh)
      val t = typeCheck(gamma, tgamma + (alphares -> bound1), p1res)

      TUniv(alphares, bound1, t)

    case UnsafeCast(e, t) =>
      typeCheck(gamma, tgamma, e)
      t

    case UpCast(e, t) =>
      val te = typeCheck(gamma, tgamma, e)
      if (subtype(tgamma)(te,t))
        t
      else
        throw TypeCheckException(s"Cannot upcast\n  expression $e\n  of type $te\n  to type $t")

    case _ =>
      throw TypeCheckException(s"typeCheck failed at $p\ngamma: $gamma\ntgamma: $tgamma")
  }


  def typecheckRule(gamma: Context, tgamma: TVarContext, r: Rule, srvSignature: TSrvRep): Type = {
    val ruleGamma = gamma ++ r.rcvars + ('this -> TSrv(srvSignature))
//    println(s"rule-pats: ${r.ps}")
//    println(s"rule-gamma: ${ruleGamma.keys}")
    val t = typeCheck(ruleGamma, tgamma, r.p)
    if (!subtype(tgamma)(t, Unit))
      throw TypeCheckException(s"Illegal rule type in rule $r, expected: Unit, was: $t")
    promote(tgamma)(t)
  }
}