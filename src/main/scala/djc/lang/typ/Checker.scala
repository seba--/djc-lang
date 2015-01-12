package djc.lang.typ

import djc.lang.{TypedLanguage, Gensym}
import djc.lang.Gensym._
import djc.lang.TypedLanguage._

object Checker {
  import types._

  type Context = Map[Symbol, Type]
  type TVarContext = Map[Symbol, Type]
  type LocTyping = Map[Int, Type]

  case class TypeCheckException(msg: String) extends RuntimeException(msg)

  //TODO make the contexts implicit parameters?
  def subtype(tgamma: TVarContext)(tpe1: Type, tpe2: Type): Boolean = {
    import op._
    (tpe1, tpe2) match {
      case (_, Top) =>
        true

      case (Bot, _) =>
        true

      case (Unit, Unit) =>
        true

      case (TNULL, TSrvRep(_)) =>
        true

      case (TVar(alpha), TVar(beta)) if alpha == beta =>
        true

      case (TVar(alpha), t) =>
        tgamma.contains(alpha) && subtype(tgamma)(tgamma(alpha), t)

      case (TUniv(alpha, u1, t1), TUniv(beta, u2, t2)) if u1 === u2 =>
        lazy val ftv1 = freeTypeVars(t1)
        lazy val ftv2 = freeTypeVars(t2)
        val alphares =
          if (alpha != beta && ftv2(alpha))
            Gensym(alpha, ftv1 ++ ftv2 ++ tgamma.keySet)
          else alpha

        subtype(tgamma + (alphares -> u1))(substType(alpha -> TVar(alphares))(t1), substType(beta -> TVar(alphares))(t2))

      case (TSvc(args), TSvc(args1)) =>
        args1.corresponds(args)(subtype(tgamma)(_, _))

      case (TSrvRep(svcs), TSrvRep(svcs1)) =>
        svcs1.forall { case (k, t) => svcs.contains(k) && subtype(tgamma)(svcs(k), t)}

      case (TImg(t1), TImg(t2)) =>
        subtype(tgamma)(t1, t2)

      case (TSrv(t), TSrv(t1)) =>
        subtype(tgamma)(t, t1)

      case (TPair(n, ts), TPair(n1, ts1)) if n1 <= n =>
        ts.take(n1).corresponds(ts1)(subtype(tgamma)(_, _))

      case (TBase(name, targs), TBase(name1, targs1)) =>
        name == name1 && targs.corresponds(targs1){ (t1,t2) => subtype(tgamma)(t1, t2) && subtype(tgamma)(t2,t1)  } //TODO support variance?
      //TODO what about equivalence classes of Bot?

      case _ => false
    }
  }

  def promote(tgamma: TVarContext)(tpe: Type): Type = tpe match {
    case TVar(alpha) =>
      if (!tgamma.contains(alpha))
        throw TypeCheckException(s"promotion of type variable $alpha undefined with tgamma: $tgamma")
      promote(tgamma)(tgamma(alpha))

    case _ => tpe
  }

  def meet(tgamma: TVarContext)(s: Type, t: Type): Type = {
    import op._

    (s,t) match {
      case (s,t) if subtype(tgamma)(s, t) => s

      case (s,t) if subtype(tgamma)(t, s) => t

      case (TUniv(alpha, u1, t1), TUniv(beta, u2, t2)) if u1 === u2 =>
        lazy val ftv1 = freeTypeVars(t1)
        lazy val ftv2 = freeTypeVars(t2)
        val alphares =
          if (alpha != beta && ftv2(alpha))
            Gensym(alpha, ftv1 ++ ftv2 ++ tgamma.keySet)
          else alpha

        TUniv(alphares, u1, meet(tgamma + (alphares -> u1))(substType(alpha -> TVar(alphares))(t1), substType(beta -> TVar(alpha))(t2)))

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

      case (TImg(t1), TImg(t2)) =>
        TImg(meet(tgamma)(t1,t2))

      case (TPair(n, ts), TPair(n1, ts1)) =>
        val m = n min n1
        val (tsmin, tsmax) = if (n <= n1) (ts,ts1) else (ts1, ts)
        TPair((((tsmin zip tsmax.take(m)) map {case (t,t1) => meet(tgamma)(t,t1)}) ++ tsmax.drop(m)):_*)

      case _ => Bot
    }
  }

  def join(tgamma: TVarContext)(s: Type, t: Type): Type = {
    import op._
    (s,t) match {
      case (s,t) if subtype(tgamma)(s, t) => t

      case (s,t) if subtype(tgamma)(t, s) => s

      case (TVar(alpha), t) =>
        join(tgamma)(tgamma(alpha), t)

      case (s, TVar(alpha)) =>
        join(tgamma)(s, tgamma(alpha))

      case (TUniv(alpha, u1, t1), TUniv(beta, u2, t2)) if u1 === u2 =>
        lazy val ftv1 = freeTypeVars(t1)
        lazy val ftv2 = freeTypeVars(t2)
        val alphares =
          if (alpha != beta && ftv2(alpha))
            Gensym(alpha, ftv1 ++ ftv2 ++ tgamma.keySet)
          else alpha

        TUniv(alphares, u1, join(tgamma + (alphares -> u1))(substType(alpha -> TVar(alphares))(t1), substType(beta -> TVar(alpha))(t2)))

      case (TSvc(args), TSvc(args1)) if args.length == args1.length =>
        TSvc((args zip args1) map {case (x,y) => meet(tgamma)(x,y)})

      case (TSrvRep(svcs), TSrvRep(svcs1)) =>
        val m1 = svcs.filterKeys(svcs1.contains(_))
        val m2 = svcs1.filterKeys(svcs.contains(_))
        val merge = for((k,t1) <- m1) yield k -> join(tgamma)(t1, m2(k))
        TSrvRep(merge)

      case (TSrv(t1), TSrv(t2)) =>
        TSrv(join(tgamma)(t1,t2))

      case (TImg(t1), TImg(t2)) =>
        TImg(join(tgamma)(t1,t2))

      case (TPair(n,ts), TPair(n1, ts1)) =>
        val m = n min n1
        TPair((ts.take(m) zip ts1.take(m)) map {case (t, t1) => join(tgamma)(t, t1)}:_*)

      case _ => Top
    }
  }

  def typeCheck(gamma: Context, tgamma: TVarContext, tlocs: LocTyping, p: Exp): Type = p match {
//    case _ if {println(s"gammma: ${gamma.keys}");false} => ?()
    case NULL => TImg(TNULL)

    case Img(srvt@ServerImpl(rules), buffer) =>
      val t = typeCheck(gamma, tgamma, tlocs, srvt)
      buffer.forall {
        case s@Send(ServiceRef(_, svc), args) =>
          val TSvc(targs) = promote(tgamma)(srvt.signature.svcs.getOrElse(svc, throw TypeCheckException(s"T-Img: Encountered service reference $svc which is not defined in template $srvt")))
          val argsValid = (args zip targs) forall { case (e, t) => subtype(tgamma)(typeCheck(gamma, tgamma, tlocs, e), t) }
          if (!argsValid)
            throw TypeCheckException(s"T-Img: Send value $s has wrong argument types")
          true

        case x => throw TypeCheckException(s"T-Img: Encountered non-send-value $x in buffer of server image $p")
      }
      TImg(t)

    case Addr(i) if tlocs.isDefinedAt(i) =>
      promote(tgamma)(tlocs(i)) match {
        case TImg(t) => TSrv(t)
        case t => throw TypeCheckException(s"Value at address $i is not a server image type, got type $t")
      }

    case Snap(e) => promote(tgamma)(typeCheck(gamma, tgamma, tlocs, e)) match {
      case TSrv(t) => TImg(t)
      case t => throw TypeCheckException(s"Expected a server instance type but got $t")
    }

    case Repl(addr, img) => (promote(tgamma)(typeCheck(gamma, tgamma, tlocs, addr)), promote(tgamma)(typeCheck(gamma, tgamma, tlocs, img))) match {
      case (TSrv(t1), TImg(t2)) if t1 === t2 => Unit
      case (t1, t2) => throw TypeCheckException(s"Expected matching server instance and image type, but got $t1 and $t2")
    }

    case BaseCall(b, ts, es) => {
      val argTs = es map (typeCheck(gamma, tgamma, tlocs, _))
      val (tArgs, bounds) = b.targs.unzip
      if (!ts.corresponds(bounds)(subtype(tgamma)(_,_)))
         throw TypeCheckException(s"Type arguments do not match type parameters. Applied $ts to ${b.targs}\n in $p")

      val sigma: Type => Type = op.substType(tArgs zip ts)(_)
      val bSig = b.ts map sigma
      if (argTs.corresponds(bSig)(subtype(tgamma)(_, _))) // actual arguments have subtypes of declared parameters
        sigma(b.res)
      else
        throw TypeCheckException(s"Arguments of base call mismatch. Was: $argTs, Expected: ${bSig}\n  in ${BaseCall(b, ts, es)}")
    }

    case Par(ps) =>
      val psTypes = ps.map(typeCheck(gamma, tgamma, tlocs, _))
      val joined = psTypes.foldLeft(Unit.asInstanceOf[Type])( (t,u) => join(tgamma)(t,u))
      if (!subtype(tgamma)(joined, Unit))
        throw TypeCheckException(s"Illegal parallel composition of types $psTypes\n which joins to $joined\n in $p")
       //TODO do we handle Bot correctly here?
      Unit

    case Send(rcv, args) =>
      val targs = args.map(typeCheck(gamma, tgamma, tlocs, _))

      promote(tgamma)(typeCheck(gamma, tgamma, tlocs, rcv)) match {
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
      promote(tgamma)(typeCheck(gamma, tgamma, tlocs, srv)) match {
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
      val ruleTypes = rules map (typecheckRule(gamma, tgamma, tlocs, _, srv.signature))

      if (!(op.freeTypeVars(srv.signature) subsetOf tgamma.keySet))
        throw TypeCheckException(s"Illegal free type variables: ${op.freeTypeVars(srv.signature) -- tgamma.keySet}")

      srv.signature
    }

    case sp@Spawn(_, e) =>
      val argt = typeCheck(gamma, tgamma, tlocs, e)
      promote(tgamma)(argt) match {
        case Bot => Bot
        case TImg(t)  => TSrv(t)
        case t => throw TypeCheckException(s"Illegal spawn expression. Expected: TSrvRep(_), was $argt (which promotes to $t)\n  in $sp)}")
      }

    case TApp(p2, t) =>
      if (!(op.freeTypeVars(t) subsetOf tgamma.keySet))
        throw TypeCheckException(s"typeCheck failed at $p\ngamma: $gamma\ntgamma: $tgamma\n  free type vars ${op.freeTypeVars(t) -- tgamma.keySet}")
      promote(tgamma)(typeCheck(gamma, tgamma, tlocs, p2)) match {
        case Bot => Bot
        case TUniv(alpha, bound, t2) if subtype(tgamma)(t, bound) =>
          op.substType(alpha -> t)(t2)

        case x => throw TypeCheckException(s"typeCheck failed at $p\ngamma: $gamma\ntgamma: $tgamma\nwith $x")
      }

    case TAbs(alpha, bound1, p1) =>
      val dontSubst = !tgamma.contains(alpha)
      lazy val alphafresh = gensym(alpha, tgamma.keySet)
      lazy val p1fresh = TypedLanguage.op.substType(alpha -> TVar(alphafresh))(p1)
      val (alphares, p1res) = if (dontSubst) (alpha, p1) else (alphafresh, p1fresh)
      val t = typeCheck(gamma, tgamma + (alphares -> bound1), tlocs, p1res)

      TUniv(alphares, bound1, t)

    case UnsafeCast(e, t) =>
      typeCheck(gamma, tgamma, tlocs, e)
      t

    case UpCast(e, t) =>
      val te = typeCheck(gamma, tgamma, tlocs, e)
      if (subtype(tgamma)(te,t))
        t
      else
        throw TypeCheckException(s"Cannot upcast\n  expression $e\n  of type $te\n  to type $t")

    case _ =>
      throw TypeCheckException(s"typeCheck failed at $p\ngamma: $gamma\ntgamma: $tgamma")
  }


  def typecheckRule(gamma: Context, tgamma: TVarContext, tlocs: LocTyping, r: Rule, srvSignature: TSrvRep): Type = {
    val ruleGamma = gamma ++ r.rcvars + ('this -> TSrv(srvSignature))
//    println(s"rule-pats: ${r.ps}")
//    println(s"rule-gamma: ${ruleGamma.keys}")
    val t = typeCheck(ruleGamma, tgamma, tlocs, r.p)
    if (!subtype(tgamma)(t, Unit))
      throw TypeCheckException(s"Illegal rule type in rule $r, expected: Unit, was: $t")
    promote(tgamma)(t)
  }
}