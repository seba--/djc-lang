package djc.lang

import util.Bag

object FlatSyntax {

  abstract class Prog {
    def fold[T](f: Folder.Type[T]): T

    def map(f: Mapper.Type): Prog

    def toSyntaxProg: Syntax.Prog = throw new UnsupportedOperationException
    def toSyntaxServer: Syntax.Server = throw new UnsupportedOperationException
    def toSyntaxService: Syntax.Service = throw new UnsupportedOperationException
  }

  case class Def(x: Symbol, s: Prog, p: Prog) extends Prog {
    def fold[T](f: Folder.Type[T]): T = f._1(x, s.fold(f), p.fold(f))

    def map(f: Mapper.Type): Prog = f._1 match {
      case None => Def(x, s.map(f), p.map(f))
      case Some(g) => g(x, s, p)
    }

    override def toSyntaxProg = Syntax.Def(x, s.toSyntaxServer, p.toSyntaxProg)
  }

  case class Par(ps: Bag[Prog]) extends Prog {
    def fold[T](f: Folder.Type[T]): T = f._2(ps map (_.fold(f)))

    def map(f: Mapper.Type): Prog = f._2 match {
      case None => Par(ps map (_.map(f)))
      case Some(g) => g(ps)
    }

    override def toSyntaxProg = Syntax.Par(ps map (_.toSyntaxProg))
  }

  case class Send(rcv: Prog, args: List[Prog]) extends Prog {
    def fold[T](f: Folder.Type[T]): T = f._3(rcv.fold(f), args map (_.fold(f)))

    def map(f: Mapper.Type): Prog = f._3 match {
      case None => Send(rcv.map(f), args map (_.map(f)))
      case Some(g) => g(rcv, args)
    }

    override def toSyntaxProg = Syntax.Send(rcv.toSyntaxService, args map (_.toSyntaxService))
  }

  case class Var(x: Symbol) extends Prog {
    def fold[T](f: Folder.Type[T]): T = f._4(x)

    def map(f: Mapper.Type): Prog = f._4 match {
      case None => Var(x)
      case Some(g) => g(x)
    }

    override def toSyntaxServer = Syntax.ServerVar(x)
    override def toSyntaxService = Syntax.ServiceVar(x)
  }

  case class ServiceRef(srv: Prog, x: Symbol) extends Prog {
    def fold[T](f: Folder.Type[T]): T = f._5(srv.fold(f), x)

    def map(f: Mapper.Type): Prog = f._5 match {
      case None => ServiceRef(srv.map(f), x)
      case Some(g) => g(srv, x)
    }

    override def toSyntaxService = Syntax.ServiceRef(srv.toSyntaxServer, x)
  }

  case class ServerImpl(rules: Bag[Rule]) extends Prog {
    def fold[T](f: Folder.Type[T]): T = f._6(rules map (_.fold(f)))

    def map(f: Mapper.Type): Prog = f._6 match {
      case None => ServerImpl(rules map (_.map(f)))
      case Some(g) => g(rules)
    }

    override def toSyntaxServer = Syntax.ServerImpl(rules map (_.toSyntaxRule))
  }

  case class Rule(ps: Bag[Pattern], p: Prog) {
    def fold[T](f: Folder.Type[T]): T = f._7(ps map (_.fold(f)), p.fold(f))

    def map(f: Mapper.Type): Rule = f._7 match {
      case None => Rule(ps map (_.map(f)), p.map(f))
      case Some(g) => g(ps, p)
    }

    def toSyntaxRule = Syntax.Rule(ps map (_.toSyntaxPattern), p.toSyntaxProg)
  }

  case class Pattern(name: Symbol, params: List[Symbol]) {
    def fold[T](f: Folder.Type[T]): T = f._8(name, params)

    def map(f: Mapper.Type): Pattern = f._8 match {
      case None => Pattern(name, params)
      case Some(g) => g(name, params)
    }

    def toSyntaxPattern = Syntax.Pattern(name, params)
  }


  object Folder {
    type FDef[T] = (Symbol, T, T) => T
    type FPar[T] = Bag[T] => T
    type FSend[T] = (T, List[T]) => T
    type FVar[T] = Symbol => T
    type FServiceRef[T] = (T, Symbol) => T
    type FServerImpl[T] = Bag[T] => T
    type FRule[T] = (Bag[T], T) => T
    type FPattern[T] = (Symbol, List[Symbol]) => T

    type Type[T] = (FDef[T], FPar[T], FSend[T], FVar[T], FServiceRef[T], FServerImpl[T], FRule[T], FPattern[T])
  }

  object Mapper {
    type FDef = (Symbol, Prog, Prog) => Prog
    type FPar = Bag[Prog] => Prog
    type FSend = (Prog, List[Prog]) => Prog
    type FVar = Symbol => Prog
    type FServiceRef = (Prog, Symbol) => Prog
    type FServerImpl = Bag[Rule] => Prog
    type FRule = (Bag[Pattern], Prog) => Rule
    type FPattern = (Symbol, List[Symbol]) => Pattern

    type Type = (Option[FDef], Option[FPar], Option[FSend], Option[FVar], Option[FServiceRef], Option[FServerImpl], Option[FRule], Option[FPattern])
  }
}