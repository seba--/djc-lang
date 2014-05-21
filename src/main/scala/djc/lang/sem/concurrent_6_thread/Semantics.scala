package djc.lang.sem.concurrent_6_thread

import util.Bag

import djc.lang.FlatSyntax._
import djc.lang.sem.AbstractSemantics

import Data._
//import djc.lang.Syntax.Mapper._
import djc.lang.sem.FlatSubstitution._
import djc.lang.sem.Crossproduct._

/**
 * Created by seba on 09/04/14.
 */
object Semantics extends AbstractSemantics[Value] { // all data is in the global state
  def normalizeVal(v: Val) = ((Bag() ++ Router.routeTable.values) map (_.normalizeVal)).flatten

  override def interp(p: Prog): Res[Val] = {
    Router.routeTable = collection.mutable.Map()
    val res = interp(p, Map())
    Thread.sleep(50)
    Router.routeTable.values.map(_.terminate = true)
    res
  }

  def interp(p: Prog, env: Env): Res[Val] = p match {
    case Var(y) if env.isDefinedAt(y) =>
      Set(env(y))

    case Def(x, p1, p2) =>
      nondeterministic[Val, Val](
      interp(p1, env),
      { result => interp(p2, env + (x -> result)) }
      )

    case s@ServerImpl(rules) =>
      val server = new ServerThread(s, env)
      val addr = Router.registerServer(server)
      server.addr = ServerAddr(addr)
      server.start()
      Set(ServerVal(server.addr))

    case ServiceRef(srv, x) =>
      nondeterministic[Val,Val](
      interp(srv, env),
      { case sval@ServerVal(addr) =>
        //val ServerClosure(impl, _) = lookupAddr(addr)
        //   if impl.rules.exists(_.ps.exists(_.name == x)) => //TODO add this check back once we have good solution for primitive services
        Set(ServiceVal(sval, x))

        //   case ServerVal(impl, _) => throw SemanticException(s"service $x not defined in server $impl")
      }
      )

    case Par(ps) if (ps map (interp(_, env))).forall(_ == Set(UnitVal)) =>
      Set(UnitVal)

    case Send(rcv, args) =>  //TODO check if right number of arguments
      nondeterministic[Val,Val](
      interp(rcv, env),
      { case svc@ServiceVal(srvVal, x) =>
          lookupAddr(srvVal.addr)
          nondeterministic[List[Value], Val](
            crossProductList(args map (interp(_, env))),
           { argVals =>
               lookupAddr(srvVal.addr).sendRequest(SendVal(svc, argVals))
               Set(UnitVal)
           }
        )
      }
      )

    case ProgClosure(p1, env1) => interp(p1, env1)
  }

  def interpSends(server: ServerThread) {
    for (r <- server.impl.rules) {
      val canSend = matchRule(ServerVal(server.addr), r.ps, server.inbox)
      if (!canSend.isEmpty) {
        val ma = canSend.head
        val (newProg, env, newQueue) = fireRule(ServerVal(server.addr), r, ma, server.inbox)
        server.inbox = newQueue
        interp(newProg, env)
        return
      }
    }
  }

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

  def fireRule(server: ServerVal, rule: Rule, ma: Match, oldQueue: Bag[SendVal]): (Prog, Env, Bag[SendVal]) = {
    val serverThread = lookupAddr(server.addr)
    val env = serverThread.env ++ ma.subst + ('this -> server)
    val newQueue = oldQueue -- ma.used
    (rule.p, env, newQueue)
  }

}
