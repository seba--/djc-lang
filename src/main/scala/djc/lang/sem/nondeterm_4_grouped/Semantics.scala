package djc.lang.sem.nondeterm_4_grouped

import scala.language.postfixOps
import util.Bag
import djc.lang.sem.{ISemanticsFactory, Crossproduct, AbstractSemantics}
import djc.lang.Syntax._
import Data._
import Router._
import djc.lang.FlattenPar.flattenPars
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder


object SemanticsFactory extends ISemanticsFactory[(Value, ServerSends)] {

  def newInstance() = {
    val router = new Router
    val data = new Data(router)
    new Semantics(router, data)
  }

  class Semantics(val router: Router, val data: Data) extends AbstractSemantics[(Value, ServerSends)] {
    import data._
    import Crossproduct._

    def normalizeVal(v: Val) = v match {
      case (UnitVal, servers) =>
        val sends = servers.foldLeft(Bag[ISendVal]()) {
          case (b, b1) => b + b1._2
        }
        sends.map(sval => sval.toNormalizedResolvedProg)
    }

    val isFullyNondeterministic = true

    type Res[T] = Set[T]
    def resToSet[T](res: Res[T]) = res

    override def interp(p: Par) = interp(p, Map(), noSends)

    def interp(p: Exp, env: Env, serverSends: ServerSends): Res[Val] = p match {
      case BaseCall(b, es) =>
        nondeterministic[List[BaseValue], Val](
          crossProductList(es.map (interp(_, env, serverSends).map (p => makeBaseValue(p._1)))),
          vs => Set((unmakeBaseValue(b.reduce(vs)), noSends)))

      case Var(y) if env.isDefinedAt(y) =>
        Set((env(y), noSends))

      case addr@ServerAddr(_) =>
        Set((ServerVal(addr), noSends))

      case s@ServerImpl(_) =>
        Set((ServerClosure(s, env), noSends))

      case Spawn(_, e) =>
        nondeterministic[Val,Val](
          interp(e, env, serverSends),
          {case (closure@ServerClosure(_,_), sends) =>
            val raddr = router.registerServer(closure)
            val addr = ServerAddr(raddr)
            Set((ServerVal(addr), sends))
          }
        )

      case ServiceRef(srv, x) =>
        nondeterministic[Val, Val](
          interp(srv, env, noSends),
          {case (sval@ServerVal(addr), `noSends`) =>
            Set((ServiceVal(sval, x), noSends))
          }
        )

      case Par(ps) =>
        nondeterministic[ServerSends, Val](
          crossProductAlt(flattenPars(ps) map (interp(_, env, noSends) map {
            case (UnitVal, nuServers) => nuServers.head
          })),
          newSends => interpSends(serverSends ++ newSends)
        )

      case Send(rcv, args) =>
        nondeterministic[Val, Val](
        interp(rcv, env, noSends), {
          case (svc@ServiceVal(srvVal, x), `noSends`) =>
            val addr = ServerAddr.unapply(srvVal.addr).get
            crossProductList(args.map(interp(_, env, noSends))) map (
               argVals => {
                 val newSends = argVals.foldLeft(noSends)((bag, s) => bag ++ s._2)
                 val normalizedArgVals = argVals map (_._1)
                 (UnitVal, sendToServer(serverSends ++ newSends, addr, SendVal(svc, normalizedArgVals)))
               }
            )
        }
        )
    }

    def interpSends(servers: ServerSends): Res[Val] = {
      val canSend = selectServerSends(servers)
      if (canSend.isEmpty)
        Set((UnitVal, servers))
      else
        nondeterministic[(Router.Addr, (Rule, Match)), Val](
          canSend,
          p => {
            val addr = p._1
            val r = p._2._1
            val m = p._2._2
            val (newProg, newEnv, nuServers) = fireRule(addr, r, m, servers)
            interp(newProg, newEnv, nuServers)
          }
        )
    }


//        nondeterministic[(Rule, Match), Val](
//          canSend,
//          (r, m) =>
//            val (newProg, newEnv, nuServers) = fireRule(srv, r, m, servers)
//            interp(newProg, newEnv, nuServers)
//        )
//    }

    def selectServerSends(servers: ServerSends): SetMap[Router.Addr, (Rule, Match)] = {
      val newServers = servers.groupBy(_._1).mapValues(selectSends(_))
      newServers.flatMap(p => p._2 map ((p._1,_)))
    }

    def selectSends(sends: Bag[(Router.Addr, ISendVal)]): Res[(Rule, Match)] =
      nondeterministic[Rule, (Rule, Match)](
        (sends map (p => collectRules(p._2))).flatten,
        rule => matchRule(rule.ps, sends) map (x => (rule, x))
      )

    def matchRule(pats: Bag[Pattern], sends: Bag[(Router.Addr,ISendVal)]): Res[Match] =
      if (pats.isEmpty)
        Set(Match(Map(), Bag()))
      else {
        val name = pats.head.name
        val params = pats.head.params
        val matchingSends = sends.filter({
          case (_,SendVal(ServiceVal(_, `name`), args)) => params.size == args.size
          case _ => false
        })
        nondeterministic[(Router.Addr, ISendVal), Match](
          matchingSends,
          s => matchRule(pats.tail, sends - s) map (
              p => Match(p.subst ++ (params zip s._2.args), p.used + s)
            )
        )
      }

    def fireRule(addr: Router.Addr, rule: Rule, ma: Match, orig: ServerSends): (Exp, Env, ServerSends) = {
      val ServerClosure(_, env0) = router.lookupAddr(addr)
      val env = env0 ++ ma.subst + ('this -> ServerVal(ServerAddr(addr)))

      val rest = orig -- ma.used
      (Par(rule.p), env, rest)
    }

    def collectRules(s: ISendVal): List[Rule] = {
      val ServerClosure(impl, _) = router.lookupAddr(s.rcv.srv.addr)
      impl.rules
    }
  }
}