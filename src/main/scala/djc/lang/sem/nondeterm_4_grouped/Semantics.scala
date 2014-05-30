package djc.lang.sem.nondeterm_4_grouped

import scala.language.postfixOps
import util.Bag
import djc.lang.sem.{ISemanticsFactory, Crossproduct, AbstractSemantics}
import djc.lang.Syntax._
import Data._
import Router._
import djc.lang.FlattenPar.flattenPars



object SemanticsFactory extends ISemanticsFactory[(Value, Servers)] {

  def newInstance() = {
    val router = new Router
    val data = new Data(router)
    new Semantics(router, data)
  }

  class Semantics(val router: Router, val data: Data) extends AbstractSemantics[(Value, Servers)] {
    import data._
    import Crossproduct._


    def normalizeVal(v: Val) = v match {
      case (UnitVal, servers) =>
        val sends = servers.values.toSet.foldLeft(Bag[ISendVal]()) {
          case (b, b1) => b ++ b1
        }
        sends.map(sval => sval.toNormalizedResolvedProg)
    }

    override def interp(p: Par) = interp(p, Map(), Map())

    def interp(p: Exp, env: Env, servers: Servers): Res[Val] = p match {
      case BaseCall(b, es) =>
        nondeterministic[List[BaseValue], Val](
          crossProductList(es.map (interp(_, env, servers).map (p => makeBaseValue(p._1)))),
          vs => Set((unmakeBaseValue(b.reduce(vs)), emptyServers)))

      case Var(y) if env.isDefinedAt(y) =>
        Set((env(y), emptyServers))

      case addr@ServerAddr(_) =>
        Set((ServerVal(addr), emptyServers))

      case s@ServerImpl(_,_) =>
        val raddr = router.registerServer(ServerClosure(s, env))
        val addr = ServerAddr(raddr)
        Set((ServerVal(addr), emptyServers))

      case ServiceRef(srv, x) =>
        nondeterministic[Val, Val](
        interp(srv, env, emptyServers), {
          case (sval@ServerVal(addr), `emptyServers`) =>
            Set((ServiceVal(sval, x), emptyServers))
        }
        )

      case Par(ps) =>
        nondeterministic[Servers, Val](
          crossProductMap(flattenPars(ps) map (interp(_, env, emptyServers) map {
            case (UnitVal, nuServers) => nuServers
          })),
          nuServers => interpSends(servers &&& nuServers))

//      case Seq(Nil) =>
//        Set((UnitVal, servers))
//      case Seq(p :: Nil) =>
//        interp(p, env, servers)
//      case Seq(p :: ps) =>
//        nondeterministic[Val, Val](
//        interp(p, env, servers), {
//          case (UnitVal, nuservers) => interp(Seq(ps), env, servers &&& nuservers)
//        }
//        )


      case Send(rcv, args) =>
        nondeterministic[Val, Val](
        interp(rcv, env, emptyServers), {
          case (svc@ServiceVal(srvVal, x), `emptyServers`) =>
            val addr = ServerAddr.unapply(srvVal.addr).get
            crossProductList(args.map(interp(_, env, emptyServers) map (_._1))) map (
               argVals => (UnitVal, sendToServer(servers, addr, SendVal(svc, argVals)))
            )
        }
        )
    }

    def interpSends(servers: Servers): Res[Val] = {
      val canSend = selectServerSends(servers)
      if (canSend.isEmpty)
        Set((UnitVal, servers))
      else
        nondeterministic[(IServerVal, Rule, Match), Val](
        canSend, {
          case (srv, r, m) =>
            val (newProg, newEnv, nuServers) = fireRule(srv, r, m, servers)
            interp(newProg, newEnv, nuServers)
        })
    }

    def selectServerSends(servers: Servers): Res[(IServerVal, Rule, Match)] =
      servers.values.toSet.map((bag: Bag[ISendVal]) => selectSends(bag)).flatten //TODO is a set really adequate? what about bag?

    def selectSends(sends: Bag[ISendVal]): Res[(IServerVal, Rule, Match)] =
      nondeterministic[(IServerVal, Rule), (IServerVal, Rule, Match)](
      (sends map collectRules).flatten, {
        case (srvVal, rule) => matchRule(srvVal, rule.ps, sends) map (x => (srvVal, rule, x))
      }
      )

    def matchRule(server: IServerVal, pats: Bag[Pattern], sends: Bag[ISendVal]): Res[Match] =
      if (pats.isEmpty)
        Set(Match(Map(), Bag()))
      else {
        val name = pats.head.name
        val params = pats.head.params
        val matchingSends = sends.filter({
          case SendVal(ServiceVal(`server`, `name`), args) => params.size == args.size
          case _ => false
        })
        nondeterministic[ISendVal, Match](
          matchingSends,
          s => matchRule(server, pats.tail, sends - s) map (
            p => Match(p.subst ++ (params zip s.args), p.used + s)
            )
        )
      }

    def fireRule(server: IServerVal, rule: Rule, ma: Match, orig: Servers): (Exp, Env, Servers) = {
      val ServerClosure(_, env0) = router.lookupAddr(server.addr)
      val env = env0 ++ ma.subst + ('this -> server)

      val raddr = ServerAddr.unapply(server.addr).get
      val queue = orig(raddr)
      val newQueue = queue -- ma.used
      val rest = orig.updated(raddr, newQueue)

      (Par(rule.p), env, rest)
    }

    def collectRules(s: ISendVal): Bag[(IServerVal, Rule)] = {
      val ServerClosure(impl, _) = router.lookupAddr(s.rcv.srv.addr)
      impl.rules map ((s.rcv.srv, _))
    }
  }
}