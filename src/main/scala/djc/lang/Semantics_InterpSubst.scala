package djc.lang

import scala.Symbol
import djc.lang.Folder._
import djc.lang.Mapper._
import scala.language.postfixOps
import util.Bag

object Semantics_InterpSubst {
  type Val = Bag[Send]
  type Res[T] = Set[T]

  def interp(p: Prog): Res[Val] = p match {
    case Def(x, s, p)
      => interp(map(substServer(x, s), p))
    case Par(ps) => {
      nondeterministic(
        crossProduct[Send](ps map (interp(_))),
        (x: Bag[Send]) => interpSends(x))
    }
    case s@Send(rcv, args) => interpSends(Bag(s))
  }

  def interpSends(sends: Bag[Send]): Res[Val] = {
    val canSend = selectSends(sends)
    if (canSend.isEmpty)
      Set(sends)
    else
      nondeterministic(
        canSend,
        (p: (Server, Rule, Map[Symbol, Service], Bag[Send])) => fireRule(p._1, p._2, p._3, p._4, sends))
  }

  def selectSends(sends: Bag[Send]): Res[(Server, Rule, Map[Symbol, Service], Bag[Send])] =
    nondeterministic(
      (sends map (collectRules(_))).flatten,
      (r: (Server, Rule)) =>
        matchRule(r._1, r._2.ps, sends) map (x => (r._1, r._2, x._1, x._2))
    )

  def matchRule(server: Server, pats: List[Pattern], sends: Bag[Send]): Res[(Map[Symbol, Service], Bag[Send])] = pats match {
    case Nil => Set((Map(), Bag()))
    case Pattern(name, params)::restPats => {
      val matchingSends = sends.filter({
        case Send(ServiceRef(server2, `name`), args) => server == server2 && params.size == args.size
        case _ => false
      })
      nondeterministic(
        matchingSends.toSet,
        (s: Send) => matchRule(server, restPats, sends - s) map (
          p => (p._1 ++ (params zip s.args), p._2 + s)
        )
      )
    }
  }

  def fireRule(server: Server, rule: Rule, subst: Map[Symbol, Service], usedSends: Bag[Send], allSends: Bag[Send]): Res[Val] = {
    var p = map(substServer('this, server), rule.p)
    for ((x, s) <- subst)
      p = map(substService(x, s), p)
    val rest = allSends diff usedSends
    interp(Par(Bag(p) ++ (rest)))
  }

  def collectRules(s: Send): List[(Server, Rule)] = s match {
    case Send(ServiceRef(s@ServerImpl(rules), _), _) => rules map ((s, _))
    case Send(_, _) => List()
  }

  def crossProduct[T](tss: Bag[Res[Bag[T]]]): Res[Bag[T]] =
    if (tss.isEmpty)
      Set(Bag())
    else {
      val rest = crossProduct(tss.tail)
      for (ts <- tss.head;
           t <- ts;
           prod <- rest)
        yield prod + t
    }

  def nondeterministic[T,U](ts: Res[T], f: T => Res[U]): Res[U] = (ts map f).flatten


  def substService(x: Symbol, s: Service): Mapper = {
    ((None, // Def
      None, // Par
      None) // Send
    ,(Some(x2 => if (x == x2) s else ServiceVar(x2)), // ServiceVar
      None) // ServiceRef
    ,(None, // ServerVar
      None) // ServerImpl
    ,Some((ps: List[Pattern], p: Prog) => substServiceRule(x, s, ps, p)) // Rule
    ,None) // Pattern
  }
  def substServiceRule(x: Symbol, s: Service, ps: List[Pattern], p: Prog): Rule = {
    val patVars = (ps map (fold(freeServiceVars, _))).flatten
    if (patVars contains x)
      Rule(ps, p)
    else
      Rule(ps, map(substService(x, s), p))
  }

  def substServer(x: Symbol, s: Server): Mapper = {
    ((Some((x2: Symbol, s2: Server, p2: Prog) => substServerDef(x, s, x2, s2, p2)), // Def
      None, // Par
      None) // Send
    ,(None, // ServiceVar
      None) // ServiceRef
    ,(Some(x2 => if (x == x2) s else ServerVar(x2)), // ServerVar
      None) // ServerImpl
    ,None // Rule
    ,None) // Pattern
  }
  def substServerDef(x: Symbol, s: Server, x2: Symbol, s2: Server, p2: Prog) = {
    val isThis = x == 'this
    val svars = fold(freeServerVars, s)
    val captureAvoiding = !svars.contains(x2)
    lazy val x2fresh = gensym(x2, svars)
    lazy val p2fresh = map(substServer(x2, ServerVar(x2fresh)), p2)
    val (x2res, p2res) = if (captureAvoiding) (x2,p2) else (x2fresh,p2fresh)

    if (isThis)
      Def(x2res, s2, map(substServer(x, s), p2res))
    else if (x == x2)
      Def(x2, map(substServer(x, s), s2), p2)
    else
      Def(x2res, map(substServer(x, s), s2), map(substServer(x, s), p2res))
  }


  val freeServerVars: Folder[Set[Symbol]] = {
    type R = Set[Symbol]
    (((x: Symbol, ds: R, ps: R) => (ds-'this) ++ (ps-x), // Def
      (xs: Bag[R]) => xs.toSet.flatten, // Par
      (srv: R, args: List[R]) => srv ++ args.flatten) // Send
     ,(x => Set(), // ServiceVar
      (srv: R, x: Symbol) => srv) // ServiceRef
     ,(x => Set(x), // ServerVar
      (xs: List[R]) => xs.toSet.flatten) // ServerImpl
     ,(ps: List[R], p: R) => p // Rule
     ,(name: Symbol, params: List[Symbol]) => Set()) // Pattern
  }

  val freeServiceVars: Folder[Set[Symbol]] = {
    type R = Set[Symbol]
    (((x: Symbol, ds: R, ps: R) => ds ++ ps, // Def
      (xs: Bag[R]) => xs.toSet.flatten, // Par
      (srv: R, args: List[R]) => srv ++ args.flatten) // Send
     ,(x => Set(x), // ServiceVar
      (srv: R, x: Symbol) => srv) // ServiceRef
     ,(x => Set(), // ServerVar
      (xs: List[R]) => xs.toSet.flatten) // ServerImpl
     ,(ps: List[R], p: R) => p -- (ps.flatten) // Rule
     ,(name: Symbol, params: List[Symbol]) => params.toSet) // Pattern
  }

  def gensym(x: Symbol, used: Set[Symbol]): Symbol = gensym(x, 0, used)
  def gensym(x: Symbol, i: Int, used: Set[Symbol]): Symbol = {
    val s = Symbol(s"${x.name}_$i")
    if (used contains s)
      gensym(x, i+1, used)
    else
      s
  }



}