package djc.lang

import util.Bag

abstract class Prog
case class Def(x: Symbol, s: Server, p: Prog) extends Prog
case class Par(ps: Bag[Prog]) extends Prog
case class Send(rcv: Service, args: List[Service]) extends Prog

abstract class Service
case class ServiceVar(x: Symbol) extends Service
case class ServiceRef(srv: Server, x: Symbol) extends Service

abstract class Server
case class ServerVar(x: Symbol) extends Server
case class ServerImpl(rules: List[Rule]) extends Server

case class Rule(ps: List[Pattern], p: Prog)
case class Pattern(name: Symbol, params: List[Symbol])

object Folder {
  type FProg[T] = ((Symbol, T, T) => T, Bag[T] => T, (T, List[T]) => T)
  type FService[T] = (Symbol => T, (T, Symbol) => T)
  type FServer[T] = (Symbol => T, List[T] => T)
  type FRule[T] = (List[T], T) => T
  type FPattern[T] = (Symbol, List[Symbol]) => T

  type Folder[T] = (FProg[T], FService[T], FServer[T], FRule[T], FPattern[T])

  def fold[T](f: Folder[T], p: Prog): T = p match {
    case Def(x, s, p) => f._1._1(x, fold(f, s), fold(f, p))
    case Par(ps) => f._1._2(ps map (fold(f, _)))
    case Send(rcv, args) => f._1._3(fold(f, rcv), args map (fold(f, _)))
  }
  
  def fold[T](f: Folder[T], s: Service): T = s match {
    case ServiceVar(x) => f._2._1(x)
    case ServiceRef(srv, x) => f._2._2(fold(f, srv), x)
  }
  
  
  def fold[T](f: Folder[T], s: Server): T = s match {
    case ServerVar(x) => f._3._1(x)
    case ServerImpl(rules) => f._3._2(rules map (fold(f, _)))
  }
    
  def fold[T](f: Folder[T], r: Rule): T =
    f._4(r.ps map (fold(f, _)), fold(f, r.p))
 
  def fold[T](f: Folder[T], p: Pattern): T =
    f._5(p.name, p.params)
}

object Mapper {
  type FProg = (Option[(Symbol, Server, Prog) => Prog], Option[Bag[Prog] => Prog], Option[(Service, List[Service]) => Prog])
  type FService = (Option[Symbol => Service], Option[(Server, Symbol) => Service])
  type FServer = (Option[Symbol => Server], Option[List[Rule] => Server])
  type FRule = Option[(List[Pattern], Prog) => Rule]
  type FPattern = Option[(Symbol, List[Symbol]) => Pattern]

  type Mapper = (FProg, FService, FServer, FRule, FPattern)

  def map(f: Mapper, p: Prog): Prog = p match {
    case Def(x, s, p) => f._1._1 match {
      case None => Def(x, map(f,s), map(f,p))
      case Some(f) => f(x, s, p)
    }
    case Par(ps) => f._1._2 match {
      case None => Par(ps map (map(f, _)))
      case Some(f) => f(ps)
    }
    case Send(rcv, args) => f._1._3 match {
      case None => Send(map(f, rcv), args map (map(f, _)))
      case Some(f) => f(rcv, args)
    }
  }

  def map(f: Mapper, s: Service): Service = s match {
    case ServiceVar(x) => f._2._1 match {
      case None => ServiceVar(x)
      case Some(f) => f(x)
    }
    case ServiceRef(srv, x) => f._2._2 match {
      case None => ServiceRef(map(f, srv), x)
      case Some(f) => f(srv, x)
    }
  }


  def map(f: Mapper, s: Server): Server = s match {
    case ServerVar(x) => f._3._1 match {
      case None => ServerVar(x)
      case Some(f) => f(x)
    }
    case ServerImpl(rules) => f._3._2 match {
      case None => ServerImpl(rules map (map(f, _)))
      case Some(f) => f(rules)
    }
  }

  def map(f: Mapper, r: Rule): Rule = f._4 match {
    case None => Rule(r.ps map (map(f, _)), map(f, r.p))
    case Some(f) => f(r.ps, r.p)
  }

  def map(f: Mapper, p: Pattern): Pattern = f._5 match {
    case None => Pattern(p.name, p.params)
    case Some(f) => f(p.name, p.params)
  }
}