package djc.lang

import util.Bag
import scala.collection.immutable.ListMap

import djc.lang.typ.Types._

object TypedSyntax {

  abstract class Prog {
    def eraseType: Syntax.Prog
  }

  case class Def(x: Symbol, s: Prog, p: Prog) extends Prog {
    override def eraseType = Syntax.Def(x, s.eraseType, p.eraseType)
  }

  case class Par(ps: Bag[Prog]) extends Prog {
    override def eraseType = Syntax.Par(ps map (_.eraseType))
  }

  object Par {
    def apply(ps: Prog*) = new Par(Bag(ps: _*))
  }

  case class Send(rcv: Prog, args: List[Prog]) extends Prog {
    override def eraseType = Syntax.Send(rcv.eraseType, args.map(_.eraseType))
  }

  object Send {
    def apply(rcv: Prog, args: Prog*) = new Send(rcv, List(args: _*))
  }

  case class Var(x: Symbol) extends Prog {
    override def eraseType = Syntax.Var(x)
  }

  case class ServiceRef(srv: Prog, x: Symbol) extends Prog {
    override def eraseType = Syntax.ServiceRef(srv.eraseType, x)
  }

  case class ServerImpl(rules: Bag[Rule]) extends Prog {
    override def eraseType = Syntax.ServerImpl(rules map (_.eraseType))

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
    override def eraseType = p.eraseType
  }

  case class TAbs(alpha: Symbol, p: Prog) extends Prog {
    override def eraseType = p.eraseType
  }

  case class Rule(ps: Bag[Pattern], p: Prog) {
    def eraseType = Syntax.Rule(ps map (_.eraseType), p.eraseType)

    lazy val rcvars: ListMap[Symbol, Type] = {
      val m = ListMap[Symbol, Type]()
      ps.foldLeft(m)(_ ++ _.params)
    }
  }

  case class Pattern(name: Symbol, params: ListMap[Symbol, Type]) {
    def eraseType = Syntax.Pattern(name, params.keys.toList)
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
}
