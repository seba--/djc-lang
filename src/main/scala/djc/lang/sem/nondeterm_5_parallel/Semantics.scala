package djc.lang.sem.nondeterm_5_parallel

import scala.language.postfixOps
import util.Bag
import djc.lang.sem.{Crossproduct, AbstractSemantics}
import djc.lang.Syntax._
import Data._

object Semantics extends AbstractSemantics[(Value, Servers)] {
  import Crossproduct._

  def normalizeVal(v: Val) = v match {
    case (UnitVal, servers) =>
      val sends = servers.values.toSet.foldLeft(Bag[SendVal]()) {
        case (b, b1) => b ++ b1
      }
      sends.map(sval => sval.toNormalizedProg)
  }

  override def interp(p: Exp) = {
    Router.routeTable = collection.mutable.Map()
    val res = interp(p, Map(), Map())
    res filter {case (v, ss) => interp(v.toNormalizedProg, Map(), ss).size == 1}
  }

  def interp(p: Exp, env: Env, servers: Servers): Res[Val] = p match {
    case Var(y) if env.isDefinedAt(y) =>
      Set((env(y), emptyServers))

    case addr@ServerAddr(_) =>
      Set((ServerVal(addr), emptyServers))

    case s@ServerImpl(rules) =>
      val raddr = Router.registerServer(ServerClosure(s, env))
      val addr = ServerAddr(raddr)
      val nuServers = Map(raddr -> Bag[SendVal]())
      Set((ServerVal(addr), nuServers))

    case ServiceRef(srv, x) =>
      nondeterministic[Val,Val](
      interp(srv, env, servers),
      { case (sval@ServerVal(addr), nuServers) =>
        //val ServerClosure(impl, _) = lookupAddr(addr)
        //   if impl.rules.exists(_.ps.exists(_.name == x)) => //TODO add this check back once we have good solution for primitive services
        Set((ServiceVal(sval, x), nuServers))

        //   case ServerVal(impl, _) => throw SemanticException(s"service $x not defined in server $impl")
      }
      )

    case Par(ps) =>
      nondeterministic[Servers, Val](
        crossProductMap(ps map (interp(_, env, servers) map {case (UnitVal, nuServers) => nuServers})),
        nuServers => interpSends(servers &&& nuServers))

    case Send(rcv, args) =>
      nondeterministic[Val,Val](
      interp(rcv, env, servers),
      { case (svc@ServiceVal(srvVal, x), nuServers) =>
        val addr = ServerAddr.unapply(srvVal.addr).get
        val s = for(l <- crossProductList(args map (interp(_, env, servers)));
                    (values, maps) = l.map{case (v,ss) => (v,ss)}.unzip)
        yield (values, maps.foldLeft(emptyServers) { case (m, m1) => m &&& m1 })

        nondeterministic[(List[Value], Servers), Val](s,
        { case (argVals, nuServers1) =>
          val srvs = List(servers, nuServers, nuServers1).reduce(_ &&& _)
          interpSends(sendToServer(srvs, addr, SendVal(svc, argVals)))   }
        )
      }
      )

    case ExpClosure(p1, env1) => interp(p1, env1, servers)
  }

  def interpSends(servers: Servers): Res[Val] = {
    val canSend = selectServerSends(servers)
    if (canSend.isEmpty)
      Set((UnitVal, servers))
    else
      nondeterministic[Bag[(ServerVal, Rule, Match)], Val](
        canSend,
        {matches =>
          val (newProgs, newServers) = fireRules(matches, servers)
          val progClosures = newProgs map (p => ExpClosure(p._1, p._2).asInstanceOf[Exp])
          interp(Par(progClosures), Map(), newServers) + ((UnitVal, servers))
        })
  }

  def selectServerSends(servers: Servers): Res[Bag[(ServerVal, Rule, Match)]] = {
    val bag = (Bag() ++ servers.values).map(selectSends(_)).filter(!_.isEmpty)
    if (bag.isEmpty)
      Set()
    else
      crossProductAlt(bag)
  }


  def selectSends(sends: Bag[SendVal]): Res[(ServerVal, Rule, Match)] =
    nondeterministic[(ServerVal, Rule), (ServerVal, Rule, Match)](
    (sends map collectRules).flatten,
    { case (srvVal, rule) => matchRule(srvVal, rule.ps, sends) map (x => (srvVal, rule, x)) }
    )

  def matchRule(server: ServerVal, pats: Bag[Pattern], sends: Bag[SendVal]): Res[Match] =
    if (pats.isEmpty)
      Set(Match(Map(), Bag()))
    else {
      val name = pats.head.name
      val params = pats.head.params
      val matchingSends = sends.filter({
        case SendVal(ServiceVal(`server`, `name`), args) => params.size == args.size
        case _ => false
      })
      nondeterministic[SendVal, Match](
        matchingSends,
        s => matchRule(server, pats.tail, sends - s) map (
          p => Match(p.subst ++ (params zip s.args), p.used + s)
          )
      )
    }

  def fireRules(rules: Bag[(ServerVal, Rule, Match)], oldServers: Servers): (Bag[(Exp, Env)], Servers) = {
    var newServers = oldServers
    val newProgs = rules map { case (srv, rule, mtch) => { // fire rules in parallel
      val (prog, env, newServers1) = fireRule(srv, rule, mtch, newServers)
      newServers = newServers1
      (prog, env)
    }}

    (Bag() ++ newProgs, newServers)
  }

  def fireRule(server: ServerVal, rule: Rule, ma: Match, orig: Servers): (Exp, Env, Servers) = {
    val ServerClosure(_, env0) = lookupAddr(server.addr)
    val env = env0 ++ ma.subst + ('this -> server)

    val raddr = ServerAddr.unapply(server.addr).get
    val queue = orig(raddr)
    val newQueue = queue -- ma.used
    val rest = orig.updated(raddr, newQueue)

    (rule.p, env, rest)
  }

  def collectRules(s: SendVal): Bag[(ServerVal, Rule)] = {
    val ServerClosure(impl, _) = lookupAddr(s.rcv.srv.addr)
    impl.rules map ((s.rcv.srv, _))
  }
}
