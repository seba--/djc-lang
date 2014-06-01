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

    implicit val buildSetFromMap = new CanBuildFrom[Map[_,_], Val, Set[Val]] {
      type From = Map[_,_]
      type To = Set[Val]
      def apply(from: From): Builder[Val, To] = Set.newBuilder[Val]
      def apply(): Builder[Val, To] = Set.newBuilder[Val]
    }

    def interpSends(servers: Servers): Res[Val] = {
      val canSend = selectServerSends(servers)
      if (canSend.isEmpty)
        Set((UnitVal, servers))
      else
        canSend.flatMap[Val, Set[Val]](p => {
          val addr = p._1
          nondeterministic[(Rule,Match), Val](
            p._2,
            p => {
              val r = p._1
              val m = p._2
              val (newProg, newEnv, nuServers) = fireRule(addr, r, m, servers)
              interp(newProg, newEnv, nuServers)
            }
          )
        })


//        nondeterministic[(Rule, Match), Val](
//          canSend,
//          (r, m) =>
//            val (newProg, newEnv, nuServers) = fireRule(srv, r, m, servers)
//            interp(newProg, newEnv, nuServers)
//        )
    }

    def selectServerSends(servers: Servers): Map[Router.Addr, Res[(Rule, Match)]] = { //Res[(IServerVal, Rule, Match)] = {
      servers.mapValues(selectSends(_)) filter (!_._2.isEmpty)
    }

    def selectSends(sends: Bag[ISendVal]): Res[(Rule, Match)] =
      nondeterministic[Rule, (Rule, Match)](
        (sends map collectRules).flatten,
        rule => matchRule(rule.ps, sends) map (x => (rule, x))
      )

    def matchRule(pats: Bag[Pattern], sends: Bag[ISendVal]): Res[Match] =
      if (pats.isEmpty)
        Set(Match(Map(), Bag()))
      else {
        val name = pats.head.name
        val params = pats.head.params
        val matchingSends = sends.filter({
          case SendVal(ServiceVal(_, `name`), args) => params.size == args.size
          case _ => false
        })
        nondeterministic[ISendVal, Match](
          matchingSends,
          s => matchRule(pats.tail, sends - s) map (
            p => Match(p.subst ++ (params zip s.args), p.used + s)
            )
        )
      }

    def fireRule(addr: Router.Addr, rule: Rule, ma: Match, orig: Servers): (Exp, Env, Servers) = {
      val ServerClosure(_, env0) = router.lookupAddr(addr)
      val env = env0 ++ ma.subst + ('this -> ServerVal(ServerAddr(addr)))

      val queue = orig(addr)
      val newQueue = queue -- ma.used
      val rest = orig.updated(addr, newQueue)

      (Par(rule.p), env, rest)
    }

    def collectRules(s: ISendVal): Res[Rule] = {
      val ServerClosure(impl, _) = router.lookupAddr(s.rcv.srv.addr)
      impl.rules
    }
  }
}