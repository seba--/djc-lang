package djc.lang.sem.nondeterm_5_parallel

import scala.language.postfixOps
import util.Bag
import djc.lang.sem.{ISemanticsFactory, Crossproduct, AbstractSemantics}
import djc.lang.Syntax._
import Data._
import Router._
import scala.collection.immutable.ListMap

object SemanticsFactory extends ISemanticsFactory[(Value, Servers)] {
  def newInstance() = {
    val router= new Router
    val data = new Data(router)
    new Inner(router, data)
  }
  
  class Inner(val router: Router, val data: Data) extends AbstractSemantics[(Value, Servers)] {
    import data._

    import Crossproduct._

    def normalizeVal(v: Val) = v match {
      case (UnitVal, servers) =>
        val sends = servers.foldLeft(Bag[ISendVal]()) {
          case (b, b1) => b + b1._2
        }
        sends.map(sval => sval.toNormalizedResolvedProg)
    }

    val isFullyNondeterministic = false

    type Res[T] = Set[T]
    def resToSet[T](res: Res[T]) = res

    override def interp(p: Par) = interp(p, Map(), emptyServers)

    def interp(p: Exp, env: Env, servers: Servers): Res[Val] = p match {
      case BaseCall(b, es) =>
        nondeterministic[(List[BaseValue],Servers), Val](
        crossProductList(es map (interp(_, env, servers))) map (
          _.foldRight ((List[BaseValue](), emptyServers))
                      ((p, r) => (makeBaseValue(p._1)::r._1, p._2 ++ r._2))),
        {case (vs, nuservers) => Set((unmakeBaseValue(b.reduce(vs)), servers ++ nuservers))})

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
          interp(srv, env, servers),
          {case (sval@ServerVal(addr), nuServers) =>
            Set((ServiceVal(sval, x), emptyServers))}
        )

      case Par(ps) =>
        nondeterministic[Servers, Val](
          crossProductAlt(FlattenParWithExpClosure.flattenPars(ps) map (interp(_, env, emptyServers) map {
            case (UnitVal, nuServers) => nuServers.head
          })),
          newSends => interpSends(servers ++ newSends)
        )

      case Send(rcv, args) =>
        nondeterministic[Val, Val](
          interp(rcv, env, servers),
          {case (svc@ServiceVal(srvVal, x), _) =>
              val addr = ServerAddr.unapply(srvVal.addr).get
              crossProductList(args.map(interp(_, env, emptyServers))) map (
                argVals => {
                  val newSends = argVals.foldLeft(emptyServers)((bag, s) => bag ++ s._2)
                  val normalizedArgVals = argVals map (_._1)
                  (UnitVal, sendToServer(servers ++ newSends, addr, SendVal(svc, normalizedArgVals)))
                }
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
        nondeterministic[ListMap[Router.Addr, (Rule, Match)], Val](
          crossProductNew(canSend),
          rules => {
            val (newProgs,newServers) = fireRules(rules, servers)
            interp(Par(newProgs), Map(), newServers)
          }
        )
    }

    def selectServerSends(servers: Servers): Map[Router.Addr, Res[(Rule, Match)]] = {
      val newServers = servers.groupBy(_._1).mapValues(selectSends(_))
      newServers filter(!_._2.isEmpty) //.flatMap(p => p._2 map ((p._1,_)))
    }


    def selectSends(sends: Bag[(Router.Addr,ISendVal)]): Res[(Rule, Match)] =
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
        nondeterministic[(Router.Addr,ISendVal), Match](
          matchingSends,
          s => matchRule(pats.tail, sends - s) map (
            p => Match(p.subst ++ (params zip s._2.args), p.used + s)
            )
        )
      }

    def fireRules(rules: ListMap[Router.Addr, (Rule, Match)], oldServers: Servers): (Bag[Exp], Servers) = {
      var newServers = oldServers
      val newProgs = rules map {
        case (srv, (rule, mtch)) => {
          // fire rules in parallel
          val (prog, newServers1) = fireRule(srv, rule, mtch, newServers)
          newServers = newServers1
          prog
        }
      }

      (Bag() ++ newProgs, newServers)
    }

    def fireRule(addr: Router.Addr, rule: Rule, ma: Match, orig: Servers): (Exp, Servers) = {
      val ServerClosure(_, env0) = router.lookupAddr(addr)
      val env = env0 ++ ma.subst + ('this -> ServerVal(ServerAddr(addr)))

      val rest = orig -- ma.used
      (ExpClosure(rule.p, env), rest)
    }

    def collectRules(s: ISendVal): Res[Rule] = {
      val ServerClosure(impl, _) = router.lookupAddr(s.rcv.srv.addr)
      impl.rules
    }
  }

}