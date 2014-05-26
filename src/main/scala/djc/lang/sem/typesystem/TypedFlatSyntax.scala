package djc.lang.sem.typesystem

import djc.lang.Syntax
import util.Bag
import scala.collection.immutable.ListMap

object TypedFlatSyntax {

  abstract class Type {
    /**
     * Equality modulo bound vars
     */
    def ===(that: Type): Boolean = this == that
  }

  case object Unit extends Type

  case class TSvc(params: List[Type]) extends Type {
    override def ===(that: Type) = that match {
      case TSvc(params1) => params.corresponds(params1)(_ === _)
      case _ => false
    }
  }

  object TSvc {
    def apply(params: Type*): TSvc = TSvc(List(params: _*))
  }

  case class TSrv(svcs: Map[Symbol, Type]) extends Type {
    override def ===(that: Type) = that match {
      case TSrv(svcs1) => svcs.keySet == svcs1.keySet && svcs.forall {
        case (s, tpe) => tpe === svcs1(s)
      }
      case _ => false
    }
  }

  object TSrv {
    def apply(svcs: (Symbol, Type)*): TSrv = TSrv(Map(svcs: _*))
  }

  case class TVar(alpha: Symbol) extends Type

  case class TBase(name: Symbol) extends Type

  case class TUniv(alpha: Symbol, tpe: Type) extends Type {
    override def ===(that: Type) = that match {
      case TUniv(beta, tpe1) => tpe === SubstType(beta, TVar(alpha))(tpe1)
      case _ => false
    }
  }

  type Context = Map[Symbol, Type]

  def typeCheck(gamma: Context, boundTv: Set[Symbol], p: Prog): Type = p match {
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

  case class TypeCheckException(msg: String) extends RuntimeException(msg)

  abstract class Prog {
    def toFlatSyntax: Syntax.Prog
  }

  case class Def(x: Symbol, s: Prog, p: Prog) extends Prog {
    override def toFlatSyntax = Syntax.Def(x, s.toFlatSyntax, p.toFlatSyntax)
  }

  case class Par(ps: Bag[Prog]) extends Prog {
    override def toFlatSyntax = Syntax.Par(ps map (_.toFlatSyntax))
  }

  object Par {
    def apply(ps: Prog*) = new Par(Bag(ps: _*))
  }

  case class Send(rcv: Prog, args: List[Prog]) extends Prog {
    override def toFlatSyntax = Syntax.Send(rcv.toFlatSyntax, args.map(_.toFlatSyntax))
  }

  object Send {
    def apply(rcv: Prog, args: Prog*) = new Send(rcv, List(args: _*))
  }

  case class Var(x: Symbol) extends Prog {
    override def toFlatSyntax = Syntax.Var(x)
  }

  case class ServiceRef(srv: Prog, x: Symbol) extends Prog {
    override def toFlatSyntax = Syntax.ServiceRef(srv.toFlatSyntax, x)
  }

  case class ServerImpl(rules: Bag[Rule]) extends Prog {
    override def toFlatSyntax = Syntax.ServerImpl(rules map (_.toFlatSyntax))

    lazy val signature = {
      import collection.mutable.{HashMap, MultiMap, Set}
      val mm = new HashMap[Symbol, Set[Type]] with MultiMap[Symbol, Type]
      for (Rule(ps, _) <- rules; Pattern(svcname, params) <- ps)
        mm.addBinding(svcname, TSvc(params.map(_._2).toList))

      val m =
        mm.foldLeft(Map[Symbol, Type]()) {
          (m, kv) =>
            val (svcname, types) = kv
            if (types.size > 1)
              throw SyntaxException(s"inconsistent type annotations for service $svcname : $types")
            m + (svcname -> types.last)
        }
      TSrv(m)
    }
  }

  object ServerImpl {
    def apply(rules: Rule*) = new ServerImpl(Bag(rules: _*))
  }


  case class TApp(p: Prog, t: Type) extends Prog {
    override def toFlatSyntax = p.toFlatSyntax
  }

  case class TAbs(alpha: Symbol, p: Prog) extends Prog {
    override def toFlatSyntax = p.toFlatSyntax
  }

  case class Rule(ps: Bag[Pattern], p: Prog) {
    def toFlatSyntax = Syntax.Rule(ps map (_.toFlatSyntax), p.toFlatSyntax)

    lazy val rcvars: ListMap[Symbol, Type] = {
      val m = ListMap[Symbol, Type]()
      ps.foldLeft(m)(_ ++ _.params)
    }
  }

  case class Pattern(name: Symbol, params: ListMap[Symbol, Type]) {
    def toFlatSyntax = Syntax.Pattern(name, params.keys.toList)
  }

  object Pattern {
    def apply(name: Symbol, param: (Symbol, Type)*) = new Pattern(name, ListMap(param: _*))
  }

  case class SyntaxException(msg: String) extends RuntimeException(msg)

  trait Mapper {
    def apply(prog: Prog): Prog = map(prog)

    def apply(tpe: Type): Type = mapType(tpe)

    def map(prog: Prog): Prog = prog match {
      case Def(x, p1, p2) =>
        Def(x, map(p1), map(p2))
      case Par(ps) =>
        Par(ps map map)
      case Send(p, args) =>
        Send(map(p), (args map map))
      case Var(x) =>
        Var(x)
      case ServiceRef(p1, x) =>
        ServiceRef(map(p1), x)
      case ServerImpl(rs) =>
        ServerImpl(rs map mapRule)
      case TApp(p1, t) =>
        TApp(map(p1), mapType(t))
      case TAbs(alpha, p1) =>
        TAbs(alpha, map(p1))
    }

    def mapType(tpe: Type): Type = tpe match {
      case Unit => Unit
      case TSvc(ts) => TSvc((ts map mapType))
      case TSrv(svcs) =>
        TSrv(svcs mapValues mapType)
      case TVar(alpha) => TVar(alpha)
      case TBase(name) => TBase(name)
      case TUniv(alpha, tpe1) => TUniv(alpha, mapType(tpe1))
    }

    def mapRule(rule: Rule): Rule = {
      val Rule(ps, prog) = rule
      Rule(ps map mapPattern, map(prog))
    }

    def mapPattern(pattern: Pattern): Pattern = {
      val Pattern(name, params) = pattern
      val transformedParams = params.foldLeft(ListMap[Symbol, Type]()) {
        (m, kv) =>
          m + (kv._1 -> mapType(kv._2))
      }
      Pattern(name, transformedParams)
    }
  }

  trait Fold {
    def fold[T](init: T)(prog: Prog): T = prog match {
      case Def(x, p1, p2) =>
        fold(fold(init)(p1))(p2)
      case Par(ps) =>
        ps.foldLeft(init)(fold(_)(_))
      case Send(p, args) =>
        args.foldLeft(fold(init)(p))(fold(_)(_))
      case Var(x) =>
        init
      case ServiceRef(p1, x) =>
        fold(init)(p1)
      case ServerImpl(rs) =>
        rs.foldLeft(init)(foldRule(_)(_))
      case TApp(p1, t) =>
        fold(foldType(init)(t))(p1)
      case TAbs(alpha, p1) =>
        fold(init)(p1)
    }

    def foldType[T](init: T)(tpe: Type): T = tpe match {
      case Unit =>
        init
      case TSvc(ts) =>
        ts.foldLeft(init)(foldType(_)(_))
      case TSrv(svcs) =>
        svcs.foldLeft(init) {
          (i, kv) => foldType(i)(kv._2)
        }
      case TVar(alpha) =>
        init
      case TBase(name) =>
        init
      case TUniv(alpha, tpe1) =>
        foldType(init)(tpe1)
    }

    def foldRule[T](init: T)(rule: Rule): T = {
      val Rule(ps, prog) = rule
      fold((ps.foldLeft(init)(foldPattern(_)(_))))(prog)
    }

    def foldPattern[T](init: T)(pattern: Pattern): T = {
      val Pattern(_, params) = pattern
      params.foldLeft(init) {
        (i, kv) => foldType(i)(kv._2)
      }
    }
  }

  object FreeVars extends Fold {
    def apply(prog: Prog): Set[Symbol] = fold(Set[Symbol]())(prog)

    def fold(init: Set[Symbol])(prog: Prog): Set[Symbol] = prog match {
      case Def(x, p1, p2) =>
        fold(fold(init)(p1) - 'this)(p2) - x
      case Var(x) =>
        init + x
      case _ => super.fold(init)(prog)
    }

    def foldType(init: Set[Symbol])(tpe: Type) = init

    def foldPattern(init: Set[Symbol])(pattern: Pattern) = init

    def foldRule(init: Set[Symbol])(rule: Rule): Set[Symbol] = {
      super.foldRule(init)(rule) -- rule.rcvars.keySet
    }
  }

  object FreeTypeVars extends Fold {
    def apply(prog: Prog): Set[Symbol] = fold(Set[Symbol]())(prog)

    def apply(tpe: Type): Set[Symbol] = foldType(Set[Symbol]())(tpe)

    def fold(init: Set[Symbol])(prog: Prog): Set[Symbol] = prog match {
      case TAbs(alpha, p1) =>
        fold(init)(p1) - alpha
      case _ => super.fold(init)(prog)
    }

    def foldType(init: Set[Symbol])(tpe: Type): Set[Symbol] = tpe match {
      case TVar(alpha) =>
        init + alpha
      case TUniv(alpha, tpe1) =>
        foldType(init)(tpe1) - alpha
      case _ => super.foldType(init)(tpe)
    }
  }

  case class SubstProg(x: Symbol, repl: Prog) extends Mapper {
    lazy val replVars = FreeVars(repl)
    lazy val replTVars = FreeTypeVars(repl)

    override def map(prog: Prog): Prog = prog match {
      case Def(x2, p1, p2) =>
        val captureAvoiding = !replVars.contains(x2)
        lazy val x2fresh = gensym(x2, replVars)
        lazy val p2fresh = SubstProg(x2, Var(x2fresh))(p2)
        val (x2res, p2res) = if (captureAvoiding) (x2, p2) else (x2fresh, p2fresh)

        if (x == 'this)
          Def(x2res, p1, p2res)
        else if (x == x2)
          Def(x, map(p1), p2)
        else
          Def(x2res, map(p1), map(p2res))

      case Var(y) if x == y =>
        repl

      case TAbs(alpha, p1) =>
        val captureAvoiding = !replTVars(alpha)
        lazy val alphafresh = gensym(alpha, replTVars)
        lazy val p1fresh = SubstType(alpha, TVar(alphafresh))(p1)
        val (alphares, p1res) = if (captureAvoiding) (alpha, p1) else (alphafresh, p1fresh)

        TAbs(alphares, map(p1res))

      case _ =>
        super.map(prog)
    }

    override def mapRule(rule: Rule): Rule = {
      val Rule(ps, prog) = rule
      val boundNames = rule.rcvars.toList map (_._1)
      val conflictingNames = boundNames filter replVars
      val captureAvoiding = conflictingNames.isEmpty

      lazy val replacements = conflictingNames zip gensyms(conflictingNames, replVars)
      lazy val progfresh = replacements.foldLeft(prog) {
        (p, kv) => SubstProg(kv._1, Var(kv._2))(p)
      }
      lazy val rename: Symbol => Symbol = replacements.toMap orElse {
        case s: Symbol => s
      }
      lazy val psfresh = ps map {
        pat =>
          Pattern(pat.name, pat.params.map {
            kv => rename(kv._1) -> kv._2
          })
      }
      val (psres, progres) = if (captureAvoiding) (ps, prog) else (psfresh, progfresh)

      if (boundNames contains x)
        rule
      else
        Rule(psres, map(progres))
    }

    override def mapType(tpe: Type): Type = tpe
  }

  case class SubstType(alpha: Symbol, repl: Type) extends Mapper {
    lazy val replTVars = FreeTypeVars(repl)

    override def map(prog: Prog): Prog = prog match {
      case TAbs(alpha1, p1) =>
        val captureAvoiding = !replTVars(alpha1)
        lazy val alpha1fresh = gensym(alpha1, replTVars)
        lazy val p1fresh = SubstType(alpha1, TVar(alpha1fresh))(p1)
        val (alpha1res, p1res) = if (captureAvoiding) (alpha1, p1) else (alpha1fresh, p1fresh)

        if (alpha == alpha1)
          prog
        else
          TAbs(alpha1res, map(p1res))

      case _ => super.map(prog)
    }

    override def mapType(tpe: Type): Type = tpe match {
      case TVar(alpha1) if alpha == alpha1 =>
        repl

      case TUniv(alpha1, tpe1) =>
        val captureAvoiding = !replTVars(alpha1)
        lazy val alpha1fresh = gensym(alpha1, replTVars)
        lazy val tpe1fresh = SubstType(alpha1, TVar(alpha1fresh))(tpe1)
        val (alpha1res, tpe1res) = if (captureAvoiding) (alpha1, tpe1) else (alpha1fresh, tpe1fresh)

        if (alpha == alpha1)
          tpe
        else
          TUniv(alpha1res, tpe1res)

      case _ => super.mapType(tpe)
    }
  }

  def gensyms(l: List[Symbol], used: Set[Symbol]): List[Symbol] = l match {
    case Nil => Nil
    case s :: ss =>
      val sfresh = gensym(s, used)
      sfresh :: gensyms(ss, used + sfresh)
  }

  def gensym(x: Symbol, used: Set[Symbol]): Symbol = gensym(x, 0, used)

  def gensym(x: Symbol, i: Int, used: Set[Symbol]): Symbol = {
    val s = Symbol(s"${x.name}_$i")
    if (used contains s)
      gensym(x, i + 1, used)
    else
      s
  }
}
