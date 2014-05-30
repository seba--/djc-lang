package djc.lang.sem.concurrent_6_thread

import util.Bag

import djc.lang.Syntax._
import djc.lang.sem.{ISemanticsFactory, AbstractSemantics}

import Data._
import Router._
import djc.lang.FlattenPar.flattenPars

/**
 * Created by seba on 09/04/14.
 */

trait ISemantics {
  def interpSends(server: Server)
}

object SemanticsFactory extends ISemanticsFactory[Value] {
  
  def newInstance() = {
    val router = new Router
    val data = new Data(router)
    new Semantics(router, data)
  }

  class Semantics(val router: Router, val data: Data) extends AbstractSemantics[Value] with ISemantics {
    import data._

    // all data is in the global state
    def normalizeVal(v: Val) = ((Bag() ++ router.routeTable.values) map (_.normalizeVal)).flatten

    override def interp(p: Par): Res[Val] = {
      val res = interp(p, Map())
      ServerThread.waitUntilStable(router.routeTable.values)
      val res2 = res filter (v => interp(v.toNormalizedProg, Map()).size == 1)
      router.routeTable.values.map(_.waitForTermination())
      res2
    }

    def interp(p: Exp, env: Env): Res[Val] = p match {
      case BaseCall(b, es) => {
        val vs = es map (interp(_, env).head) map (makeBaseValue(_))
        Set(unmakeBaseValue(b.reduce(vs)))
      }

      case Var(y) if env.isDefinedAt(y) =>
        Set(env(y))

      case s@ServerImpl(rules) =>
        val serverThread = new ServerThread(this, s, env)
        val addr = router.registerServer(serverThread)

        val serverAddr = ServerAddr(addr, 0)
        val server = router.lookupServer(serverAddr)
        server.addr = serverAddr
        
        serverThread.start()
        Set(ServerVal(serverAddr))

      case ServiceRef(srv, x) =>
        interp(srv, env).head match {
          case sval@ServerVal(addr) => Set(ServiceVal(sval, x))
        }


      case Par(ps) =>
        flattenPars(ps).map(interp(_, env)).foldLeft[Res[Val]](Set(UnitVal)) ((p1,p2) => (p1.head, p2.head) match {case (UnitVal,UnitVal) => Set(UnitVal)})

      case Seq(Nil) =>
        Set(UnitVal)
      case Seq(p :: Nil) =>
        interp(p, env)
      case Seq(p :: ps) =>
        interp(p, env).head match {
          case UnitVal => interp(Seq(ps), env)
        }


      case Send(rcv, args) =>
        interp(rcv, env).head match {
          case svc@ServiceVal(srvVal, x) =>
            router.lookupAddr(srvVal.addr)
            val argVals = args map (interp(_, env).head)
            router.lookupAddr(srvVal.addr).receiveRequest(SendVal(svc, argVals))
            Set(UnitVal)
        }
    }

    def interpSends(server: Server) {
      for (r <- server.impl.rules) {
        val canSend = matchRule(ServerVal(server.addr), r.ps, server.inbox)
        if (!canSend.isEmpty) {
          val ma = canSend.get
          val (newProg, env, newQueue) = fireRule(ServerVal(server.addr), r, ma, server.inbox)
          server.inbox = newQueue
          interp(newProg, env)
          return
        }
      }
    }

    def matchRule(server: ServerVal, pats: Bag[Pattern], sends: Bag[ISendVal]): Option[Match] =
      if (pats.isEmpty)
        Some(Match(Map(), Bag()))
      else {
        val name = pats.head.name
        val params = pats.head.params
        val matchingSends = sends.filter({
          case SendVal(ServiceVal(`server`, `name`), args) => params.size == args.size
          case _ => false
        })

        if (matchingSends.isEmpty)
          None
        else {
          val matchingSend = matchingSends.head
          matchRule(server, pats.tail, sends - matchingSend).
            map (p => Match(p.subst ++ (params zip matchingSend.args), p.used + matchingSend))
        }
      }

    def fireRule(server: ServerVal, rule: Rule, ma: Match, oldQueue: Bag[ISendVal]): (Exp, Env, Bag[ISendVal]) = {
      val s = router.lookupServer(server.addr)
      val env = s.env ++ ma.subst + ('this -> server)
      val newQueue = oldQueue -- ma.used
      (Par(rule.p), env, newQueue)
    }
  }
}