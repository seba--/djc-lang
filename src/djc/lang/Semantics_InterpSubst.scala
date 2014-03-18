package djc.lang

import scala.Symbol
import djc.lang.Folder._
import djc.lang.Mapper._


object Semantics_InterpSubst {
  type Val = List[Send]

  def interp(p: Prog): List[Val] = p match {
    case Def(x, s, p)
      => interp(map(substServer(x, s), p))
    case Par(ps) =>
      nondeterministic(
        (ps map (interp(_))).reduce(_++_),
        (x: List[Send]) => interpSends(x))
  }

  def interpSends(sends: List[Send]): List[Val] =
    nondeterministic(
      selectSends(sends),
      (p: (Rule, Map[Symbol, Service], List[Send])) => fireRule(p._1, p._2, p._3, sends))

  def selectSends(sends: List[Send]): List[(Rule, Map[Symbol, Service], List[Send])] =
    nondeterministic(
      (sends map (collectRules(_))) reduce(_++_) distinct,
      (r: (Server, Rule)) =>
        matchRule(r._1, r._2.ps, sends) map (x => (r._2, x._1, x._2))
    )

  def matchRule(server: Server, pats: List[Pattern], sends: List[Send]): List[(Map[Symbol, Service], List[Send])] = pats match {
    case Nil => List()
    case Pattern(name, params)::restPats => {
      val matchingSends = sends.filter({
        case Send(ServiceRef(server2, `name`), args) => server == server2 && params.size == args.size
        case _ => false
      })
      nondeterministic(
        matchingSends distinct,
        (s: Send) => matchRule(server, restPats, remove(s, sends)) map (
          p => (p._1 ++ (params zip s.args), s::p._2)
        )
      )
    }
  }

  def fireRule(rule: Rule, subst: Map[Symbol, Service], usedSends: List[Send], allSends: List[Send]): List[Val] = {
    var p = rule.p
    for ((x, s) <- subst)
      p = map(substService(x, s), p)

    nondeterministic(
      interp(p),
      (newSends: List[Send]) => interpSends((allSends diff usedSends) ++ newSends)
    )
  }

  def collectRules(s: Send): List[(Server, Rule)] = s match {
    case Send(ServiceRef(s@ServerImpl(rules), _), _) => rules map ((s, _))
    case Send(_, _) => List()
  }

  def nondeterministic[T,U](ts: List[T], f: T => List[U]): List[U] = ts map f reduce(_++_) distinct

  def remove[T](t: T, ts: List[T]): List[T] = ts match {
    case Nil => Nil
    case `t`::rest => rest
    case t2::rest => t2::(remove(t, rest))
  }



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
    val patVars = (ps map (fold(freeServiceVars, _))) reduce(_++_)
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
      (xs: List[R]) => xs.reduce(_++_), // Par
      (srv: R, args: List[R]) => srv ++ args.reduce(_++_)) // Send
     ,(x => Set(), // ServiceVar
      (srv: R, x: Symbol) => srv) // ServiceRef
     ,(x => Set(x), // ServerVar
      (xs: List[R]) => xs.reduce(_++_)) // ServerImpl
     ,(ps: List[R], p: R) => p // Rule
     ,(name: Symbol, params: List[Symbol]) => Set()) // Pattern
  }

  val freeServiceVars: Folder[Set[Symbol]] = {
    type R = Set[Symbol]
    (((x: Symbol, ds: R, ps: R) => ds ++ ps, // Def
      (xs: List[R]) => xs.reduce(_++_), // Par
      (srv: R, args: List[R]) => srv ++ args.reduce(_++_)) // Send
     ,(x => Set(x), // ServiceVar
      (srv: R, x: Symbol) => srv) // ServiceRef
     ,(x => Set(), // ServerVar
      (xs: List[R]) => xs.reduce(_++_)) // ServerImpl
     ,(ps: List[R], p: R) => p -- (ps.reduce(_++_)) // Rule
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