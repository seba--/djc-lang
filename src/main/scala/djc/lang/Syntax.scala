package djc.lang

import util.Bag

import scala.collection.immutable.Queue

object Syntax {

  abstract class Exp

  case class Addr(i: Symbol) extends Exp

  case object NULL extends Exp

  case class Img(template: Exp, buffer: Bag[Request]) extends Exp
  object Img {
    def apply(template: Exp): Img = new Img(template, Bag())
  }

  case class Snap(addr: Exp) extends Exp

  case class Repl(addr: Exp, img: Exp) extends Exp

  case class Par(ps: Bag[Exp]) extends Exp {
    override def toString =
      if (ps.isEmpty)
        "Par()"
      else
        s"Par(${ps.toString})"
  }
  object Par { def apply(ps : Exp*): Par = new Par(Bag(ps:_*)) }

  case class Send(rcv: Exp, args: List[Exp]) extends Exp
  object Send { def apply(rcv: Exp, args: Exp*): Send = new Send(rcv, List(args:_*)) }

  case class Var(x: Symbol) extends Exp

  case class ServiceRef(srv: Exp, x: Symbol) extends Exp

  case class ServerImpl(rules: List[Rule]) extends Exp {
    lazy val services = rules flatMap (r => r.ps map (p => p.name))
  }
  object ServerImpl { def apply(rules: Rule*): ServerImpl = new ServerImpl(List(rules:_*)) }
  object LocalServer {
    def apply(rules: List[Rule]): Spawn = Spawn(true, Img(ServerImpl(rules), Bag()))
    def apply(rules: Rule*): Spawn = Spawn(true, Img(ServerImpl(List(rules: _*)), Bag()))
  }

  case class Spawn(local: Boolean, e: Exp) extends Exp {
    override def equals(a: Any) = a.isInstanceOf[SpawnAny] && a == this || a.isInstanceOf[Spawn] && {
      val o = a.asInstanceOf[Spawn]
      o.local == local && o.e == e
    }
    override def hashCode = e.hashCode
  }
  class SpawnAny(e: Exp) extends Spawn(false, e) {
    override def equals(a: Any) = a.isInstanceOf[Spawn] && a.asInstanceOf[Spawn].e == e
    override def hashCode = e.hashCode
  }
  object Spawn {
    def apply(e: Exp): Spawn = Spawn(false, e)
  }
  object SpawnAny { def apply(e: Exp): SpawnAny = new SpawnAny(e) }
  object SpawnImg {
    def apply(e: Exp): Spawn = new Spawn(false, Img(e, Bag()))
    def apply(local: Boolean, e: Exp): Spawn = new Spawn(local, Img(e))
  }
  object SpawnLocalImg {
    def apply(e: Exp): Spawn = new Spawn(true, Img(e))
  }
  case class Rule(ps: Bag[Pattern], p: Exp)

  case class Pattern(name: Symbol, params: List[Symbol])
  object Pattern { def apply(name: Symbol, params: Symbol*): Pattern = new Pattern(name, List(params:_*)) }



  trait Value {
    def toExp: Exp
  }
  trait BaseOp {
    implicit def valueResult(v: Value): Either[Value, Exp] = Left(v)
    implicit def expResult(v: Exp): Either[Value, Exp] = Right(v)
    def reduce(vs: List[Value]): Either[Value, Exp]
  }
  case class BaseCall(b: BaseOp, es: List[Exp]) extends Exp
  object BaseCall {
    def apply(b: BaseOp, es: Exp*): BaseCall = BaseCall(b, List(es:_*))
  }

  case class Request(svc: Symbol, args: List[Value])


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
    def &&(e2: Exp): Exp = Par(e1, e2)
    def &&(e2s: Bag[Exp]): Exp = Par(e2s + e1)
  }
  //  class InfixSend(e1: Send) extends InfixExp(e1) {
  //    def >>(e2: Send) = TypedSyntaxDerived.SendSeq(e1, e2)
  //  }
  case class PatternSymbol(s: Symbol) {
    def ?(ps: Symbol*) = Pattern(s, List(ps:_*))
  }
  case class InfixPattern(p1: Bag[Pattern]) {
    def &&(ps: Bag[Pattern]): Bag[Pattern] = p1 ++ ps
    def &&(p2: Pattern): Bag[Pattern] = p1 + p2
  }




  trait Mapper {
    def apply(prog: Exp): Exp = map(prog)

    def map(prog: Exp): Exp = prog match {
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
      case BaseCall(b, es) =>
        BaseCall(b, es map map)
    }

    def mapRule(rule: Rule): Rule = {
      val Rule(ps, prog) = rule
      Rule(ps map mapPattern, map(prog))
    }

    def mapPattern(pattern: Pattern): Pattern = pattern
  }

  trait Fold {
    def fold[T](init: T)(prog: Exp): T = prog match {
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
      case BaseCall(b, es) =>
        es.foldLeft(init)(fold(_)(_))
    }

    def foldRule[T](init: T)(rule: Rule): T = {
      val Rule(ps, prog) = rule
      fold((ps.foldLeft(init)(foldPattern(_)(_))))(prog)
    }

    def foldPattern[T](init: T)(pattern: Pattern): T = init
  }
}