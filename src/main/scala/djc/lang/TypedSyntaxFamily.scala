package djc.lang

import djc.lang.typ.{TypeOps, TypeFamily, TypedSyntaxOps}
import util.Bag
import util.ListOps._

import scala.collection.immutable.{Queue, ListMap}

/**
 * Created by oliver on 22.09.14.
 */
trait TypedSyntaxFamily {
  self =>

  val op: TypedSyntaxOps { val syntax: self.type }
  type TSF = TypedSyntaxFamily
  val types: TypeFamily

  import types._
  import op._

  abstract class Exp {
    val family: TypedSyntaxFamily = self
    def eraseType: Syntax.Exp
    def toFamily(TF: TSF): TF.Exp
  }

  case class Addr(i: Symbol) extends Exp {
    def eraseType = Syntax.Addr(i)

    def toFamily(TF: TSF) = TF.Addr(i)
  }

  case object NULL extends Exp {
    def eraseType = Syntax.NULL
    def toFamily(TF: TSF) = TF.NULL
  }

  case class Img(template: Exp, buffer: Bag[Syntax.Request]) extends Exp {
    def eraseType = Syntax.Img(template.eraseType, buffer)
    def toFamily(TF: TSF) = TF.Img(template.toFamily(TF), buffer)
  }
  object Img {
    def apply(template: Exp): Img = new Img(template, Bag())
  }

  case class Snap(addr: Exp) extends Exp {
    def eraseType = Syntax.Snap(addr.eraseType)

    def toFamily(TF: TSF) = TF.Snap(addr.toFamily(TF))
  }

  case class Repl(addr: Exp, img: Exp) extends Exp {
    def eraseType = Syntax.Repl(addr.eraseType, img.eraseType)

    def toFamily(TF: TSF) = TF.Repl(addr.toFamily(TF), img.toFamily(TF))
  }

  case class Par(ps: Bag[Exp]) extends Exp {
    override def eraseType = Syntax.Par(ps map (_.eraseType))
    override def toString =
      if (ps.isEmpty)
        "Par()"
      else
        s"Par(${ps.toString})"

    override def toFamily(TF: TSF) = TF.Par(ps map {_.toFamily(TF)})
  }
  object Par { def apply(ps: Exp*) = new Par(Bag(ps: _*))}

  case class Send(rcv: Exp, args: List[Exp]) extends Exp {
    override def eraseType = Syntax.Send(rcv.eraseType, args.map(_.eraseType))

    override def toFamily(TF: TSF) = TF.Send(rcv.toFamily(TF), args map {_.toFamily(TF)})
  }

  object Send {
    def apply(rcv: Exp, args: Exp*): Send = Send(rcv, List(args: _*))
  }

  case class Var(x: Symbol) extends Exp {
    override def eraseType = Syntax.Var(x)
    override def toFamily(TF: TSF) = TF.Var(x)
  }

  case class ServiceRef(srv: Exp, x: Symbol) extends Exp {
    override def eraseType = Syntax.ServiceRef(srv.eraseType, x)
    override def toFamily(TF: TSF) = TF.ServiceRef(srv.toFamily(TF), x)
  }

  case class ServerImpl(rules: List[Rule]) extends Exp {
    override def eraseType = Syntax.ServerImpl(rules map (_.eraseType))

    lazy val signature = {
      import scala.collection.mutable.{HashMap, MultiMap, Set}
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
      TSrvRep(m)
    }

    override def toFamily(TF: TSF) = TF.ServerImpl(rules map {_.toFamily(TF)})
  }
  object ServerImpl {
    def apply(rules: Rule*): ServerImpl = ServerImpl(List(rules: _*))
  }

  case class Spawn(local: Boolean, e: Exp) extends Exp {
    override def eraseType = Syntax.Spawn(local, e.eraseType)
    override def toFamily(TF: TSF) = TF.Spawn(local, e.toFamily(TF))
  }
  object Spawn {
    def apply(e: Exp): Spawn = Spawn(false, e)
  }
  object SpawnLocal {
    def apply(e: Exp): Spawn = Spawn(true, e)
  }
  object Server {
    def apply(rules: List[Rule]): Spawn = Spawn(false, Img(ServerImpl(rules), Bag()))
    def apply(rules: Rule*): Spawn = Spawn(false, Img(ServerImpl(List(rules: _*)), Bag()))
  }
  object LocalServer {
    def apply(rules: List[Rule]): Spawn = Spawn(true, Img(ServerImpl(rules), Bag()))
    def apply(rules: Rule*): Spawn = Spawn(true, Img(ServerImpl(List(rules: _*)), Bag()))
  }
  object SpawnImg {
    def apply(e: Exp): Spawn = new Spawn(false, Img(e, Bag()))
    def apply(local: Boolean, e: Exp): Spawn = new Spawn(local, Img(e))
  }
  object SpawnLocalImg {
    def apply(e: Exp): Spawn = new Spawn(true, Img(e))
  }

  case class TApp(p: Exp, t: Type) extends Exp {
    override def eraseType = p.eraseType

    override def toFamily(TF: TSF) = TF.TApp(p.toFamily(TF), t.toFamily(TF.types))
  }
  object TApp {
    def apply(e: Exp, ts: Type*) = ts.foldLeft(e){case (e1, t) => new TApp(e1, t) }
  }

  case class TAbs(alpha: Symbol, bound: Type, p: Exp) extends Exp {
    override def eraseType = p.eraseType

    override def toFamily(TF: TSF) = TF.TAbs(alpha, bound.toFamily(TF.types), p.toFamily(TF))
  }
  object TAbs {
    def apply(alpha: Symbol, p: Exp): TAbs = TAbs(alpha, Top, p)
    def apply(alpha1: Symbol, alpha2: Symbol, alphas: Symbol*)(p: Exp): TAbs = {
      (alpha1 +: alpha2 +: alphas).foldRight(p) {
        case (a, e) => TAbs(a, Top, e)
      }.asInstanceOf[TAbs]
    }
    def apply(alpha: (Symbol,Type), alphas: (Symbol,Type)*)(p: Exp): TAbs = {
      (alpha +: alphas).foldRight(p) {
        case ((a,t), e) => TAbs(a, t, e)
      }.asInstanceOf[TAbs]
    }
  }

  case class UnsafeCast(e: Exp, t: Type) extends Exp {
    override def eraseType = e.eraseType

    override def toFamily(TF: TSF) = TF.UnsafeCast(e.toFamily(TF), t.toFamily(TF.types))
  }

  case class UpCast(e: Exp, t: Type) extends Exp {
    override def eraseType = e.eraseType

    override def toFamily(TF: TSF) = TF.UpCast(e.toFamily(TF), t.toFamily(TF.types))
  }

  case class Rule(ps: Bag[Pattern], p: Exp) {
    def eraseType = Syntax.Rule(ps map (_.eraseType), p.eraseType)

    lazy val rcvars: ListMap[Symbol, Type] = {
      val m = ListMap[Symbol, Type]()
      ps.foldLeft(m)(_ ++ _.params)
    }

    override def toString = s"Rule($ps  =>  $p)"

    val family: TypedSyntaxFamily = self
    def toFamily(TF: TSF): TF.Rule = TF.Rule(ps map {_.toFamily(TF)}, p.toFamily(TF))
  }
  object Rule {
    def apply(ps: Pattern*)(p: Exp): Rule = Rule(Bag(ps:_*), p)
  }

  case class Pattern(name: Symbol, params: ListMap[Symbol, Type]) {
    def eraseType = Syntax.Pattern(name, params.keys.toList)
    val family: TypedSyntaxFamily = self
    def toFamily(TF: TSF): TF.Pattern = TF.Pattern(name, params.map { case (k, t) => (k, t.toFamily(TF.types))}  )
  }

  object Pattern {
    def apply(name: Symbol, param: (Symbol, Type)*) = new Pattern(name, ListMap(param: _*))
  }

  case class SyntaxException(msg: String) extends RuntimeException(msg)



  type Value = Syntax.Value
  abstract class BaseOp(val targs: List[(Symbol, Type)], val ts: List[Type], val res: Type) extends Syntax.BaseOp {
    def this(ts: List[Type], res: Type) = this(Nil, ts, res)
    def this(targs: (Symbol,Type)*)(ts: List[Type], res: Type) = this(List(targs:_*), ts, res)
    def eraseType: Syntax.BaseOp = this

    val family: TypedSyntaxFamily = self
    def toFamily(TF: TSF): TF.BaseOp = TF.BaseOpFromImpl(targs map {case (s, t) => (s, t.toFamily(TF.types))}, ts map {_.toFamily(TF.types)}, res.toFamily(TF.types), this)
  }

  //TODO this should be the standard way of defining typed baseops, i.e. decorate an untyped impl
  case class BaseOpFromImpl(override val targs: List[(Symbol, Type)], override val ts: List[Type], override val res: Type, impl: Syntax.BaseOp) extends BaseOp(targs, ts, res) {
      def reduce(vs: List[Syntax.Value]) = impl.reduce(vs)
  }

  case class BaseCall(b: BaseOp, ts: List[Type], es: List[Exp]) extends Exp {
    def this(b: BaseOp, es: List[Exp]) = this(b, Nil, es)
    def eraseType = Syntax.BaseCall(b.eraseType, es map (_.eraseType))

    lazy val resultType: Type = {
      val (tvars,_) = b.targs.unzip
      val sigma: Type => Type = types.op.substType(tvars zip ts)(_)
      sigma(b.res)
    }

    override def toFamily(TF: TSF) = TF.BaseCall(b.toFamily(TF), ts map {_.toFamily(TF.types)}, es map {_.toFamily(TF)})
  }
  object BaseCall {
  //  def apply(b: BaseOp, ts: Type*)(es: Exp*): BaseCall = BaseCall(b, List(ts:_*), List(es:_*))
    def apply(b: BaseOp, ts: List[Type], es: Exp*): BaseCall = BaseCall(b, ts, List(es:_*))
    def apply(b: BaseOp, es: Exp*): BaseCall = BaseCall(b, Nil, List(es:_*))
    def apply(b: BaseOp, es: List[Exp]): BaseCall = BaseCall(b, Nil, List(es:_*))
  }

  implicit def symbolBound(s: Symbol): (Symbol, Type) = (s, Top)
  implicit def varSymbol(s: Symbol) = Var(s)
  implicit def varSymbolInfixExp(s: Symbol) = new InfixExp(Var(s))
  implicit def varSymbolInfixPattern(s: Symbol) = PatternSymbol(s)
  implicit def patternBag(p: Pattern) = Bag(p)
  implicit def infixPattern(p: Pattern) = InfixPattern(Bag(p))
  implicit def infixPattern(ps: Bag[Pattern]) = InfixPattern(ps)
//  implicit def infixSend(e: Send) = new InfixSend(e)
  implicit def infixExp(e: Exp) = new InfixExp(e)
  implicit def infixSendExp(e: Send) = new InfixExp(e)
  class InfixExp(val e1: Exp) {
    def ~>(s: Symbol) = ServiceRef(e1, s)
    def !!(es: Exp*) = Send(e1, List(es:_*))
    def apply(t: Type): Exp = TApp(e1, t)
    def apply(ts: Type*): Exp = ts.foldLeft(this)((e,t) => new InfixExp(e.apply(t))).e1
    def &&(e2: Exp): Exp = Par(e1, e2)
    def &&(e2s: Bag[Exp]): Exp = Par(e2s + e1)
    def cast(t: Type) = UpCast(e1, t)
    def as(t: Type) = UpCast(e1, t)
  }
//  class InfixSend(e1: Send) extends InfixExp(e1) {
//    def >>(e2: Send) = TypedSyntaxDerived.SendSeq(e1, e2)
//  }
  case class PatternSymbol(s: Symbol) {
    def ?(ps: (Symbol, Type)*) = Pattern(s, ListMap(ps:_*))
  }
  case class InfixPattern(p1: Bag[Pattern]) {
    def &&(ps: Bag[Pattern]): Bag[Pattern] = p1 ++ ps
    def &&(p2: Pattern): Bag[Pattern] = p1 + p2
  }

  def ?(ts: Type*) = TSvc(List(ts:_*))
  implicit def tvarSymbol(s: Symbol) = TVar(s)
  implicit def infixType(t: Type) = InfixType(t)
  case class InfixType(t: Type) {
    def apply(t2: Type): Type = t match {
      case TUniv(x, _, t1) => types.op.substType(x -> t2)(t1)
      case _ => throw new IllegalArgumentException(s"Expect TUniv but got $t")
    }
    def apply(t2s: Type*): Type = t2s.foldLeft(this)((t, t2) => InfixType(t.apply(t2))).t

    def ++(t2: Type): Type = (t, t2) match {
      case (TSrvRep(svcs1), TSrvRep(svcs2)) =>
        if (!svcs1.keySet.intersect(svcs2.keySet).isEmpty)
          throw new IllegalArgumentException(s"Cannot build union of server types: Overlapping service declarations.")
        else
          TSrvRep(svcs1 ++ svcs2)
      case _ => throw new IllegalArgumentException(s"Cannot build union of server types: Expected server types but got ${(t,t2)}")
    }
  }

  implicit class VarsConstraint(alpha: Symbol) {
    def <<(bound: Option[Type]) = (alpha, bound)
    def <<(bound: Type) = (alpha, bound)
  }


  trait Mapper {
    final type TMapE = PartialFunction[Exp, Exp]

    val typeMapper: types.Mapper
    import typeMapper.mapType

    def apply(prog: Exp): Exp = map(prog)

    def map: TMapE = {
      case Addr(i) => Addr(i)
      case NULL => NULL
      case Img(template, buffer) =>
        Img(map(template), buffer)
      case Snap(addr) =>
        Snap(map(addr))
      case Repl(addr, img) =>
        Repl(map(addr), map(img))
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
      case Spawn(local, e) =>
        Spawn(local, map(e))
      case TApp(p1, t) =>
        TApp(map(p1), mapType(t))
      case TAbs(alpha, bound1, p1) =>
        TAbs(alpha, mapType(bound1), map(p1))
      case UnsafeCast(e, t) =>
        UnsafeCast(map(e), mapType(t))
      case UpCast(e, t) =>
        UpCast(map(e), mapType(t))
      case BaseCall(b, ts, es) =>
        BaseCall(b, ts map mapType, es map map)
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

  trait StrictFold[T] {
    final type FoldE = PartialFunction[Exp, T]
    val typeFold: types.StrictFold[T]
    import typeFold.foldType

    def apply(e: Exp): T

    def fold(init: T): FoldE = {
      case Addr(i) => init
      case NULL => init
      case Img(template, buffer) =>
        fold(init)(template)
      case Snap(addr) =>
        fold(init)(addr)
      case Repl(addr, img) =>
        fold(fold(init)(addr))(img)
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
      case Spawn(local, e) =>
        fold(init)(e)
      case TApp(p1, t) =>
        fold(foldType(init)(t))(p1)
      case TAbs(alpha, bound1, p1) =>
        fold(foldType(init)(bound1))(p1)
      case UnsafeCast(e, t) =>
        fold(foldType(init)(t))(e)
      case UpCast(e, t) =>
        fold(foldType(init)(t))(e)
      case BaseCall(b, ts, es) =>
        es.foldLeft(ts.foldLeft(init)(foldType(_)(_)))(fold(_)(_))
    }

    def foldRule(init: T)(rule: Rule): T = {
      val Rule(ps, prog) = rule
      fold((ps.foldLeft(init)(foldPattern(_)(_))))(prog)
    }

    def foldPattern(init: T)(pattern: Pattern): T = {
      val Pattern(_, params) = pattern
      params.foldLeft(init) {
        (i, kv) => foldType(i)(kv._2)
      }
    }
  }

  trait LazyFold[T]  {
    final type FoldE = PartialFunction[Exp, T]

    val typeFold: types.LazyFold[T]
    import typeFold.foldType

    def apply(e: Exp): T

    def fold(init: => T): FoldE = {
      case Addr(i) => init
      case NULL => init
      case Img(template, buffer) =>
        fold(init)(template)
      case Snap(addr) =>
        fold(init)(addr)
      case Repl(addr, img) =>
        fold(fold(init)(img))(addr)
      case Par(ps) =>
        ps.lazyFoldr(init)((e, t) => fold(t)(e))
      case Send(p, args) =>
        fold(args.lazyFoldr(init)((e, t) => fold(t)(e)))(p)
      case Var(x) =>
        init
      case ServiceRef(p1, x) =>
        fold(init)(p1)
      case ServerImpl(rs) =>
        rs.lazyFoldr(init)((e, t) => foldRule(t)(e))
      case Spawn(local, e) =>
        fold(init)(e)
      case TApp(p1, t) =>
        fold(foldType(init)(t))(p1)
      case TAbs(alpha, bound1, p1) =>
        foldType(fold(init)(p1))(bound1)
      case UnsafeCast(e, t) =>
        fold(foldType(init)(t))(e)
      case UpCast(e, t) =>
        fold(foldType(init)(t))(e)
      case BaseCall(b, ts, es) =>
        ts.lazyFoldr(es.lazyFoldr(init)((e, t) => fold(t)(e)))((tpe, t) => foldType(t)(tpe))
    }

    def foldRule(init: => T)(rule: Rule): T = {
      val Rule(ps, prog) = rule
      ps.lazyFoldr(fold(init)(prog))((p, t) => foldPattern(t)(p))
    }

    def foldPattern(init: => T)(pattern: Pattern): T = {
      val Pattern(_, params) = pattern
      params.toList.lazyFoldr(init)((p, t) => foldType(t)(p._2))
    }
  }
}
