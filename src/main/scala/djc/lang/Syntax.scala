package djc.lang

import util.Bag

object Syntax {

  abstract class Prog

  case class Def(x: Symbol, s: Prog, p: Prog) extends Prog

  case class Par(ps: Bag[Prog]) extends Prog
  object Par { def apply(ps : Prog*): Par = new Par(Bag(ps:_*)) }

  case class Send(rcv: Prog, args: List[Prog]) extends Prog
  object Send { def apply(rcv: Prog, args: Prog*): Send = new Send(rcv, List(args:_*)) }

  case class Var(x: Symbol) extends Prog

  case class ServiceRef(srv: Prog, x: Symbol) extends Prog

  case class ServerImpl(rules: Bag[Rule]) extends Prog
  object ServerImpl { def apply(rules: Rule*): ServerImpl = new ServerImpl(Bag(rules:_*)) }

  case class Rule(ps: Bag[Pattern], p: Prog)

  case class Pattern(name: Symbol, params: List[Symbol])
  object Pattern { def apply(name: Symbol, params: Symbol*): Pattern = new Pattern(name, List(params:_*)) }
}