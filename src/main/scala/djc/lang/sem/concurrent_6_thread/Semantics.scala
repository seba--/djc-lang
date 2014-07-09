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
  def interpSends(server: Server, currentThread: ServerThread): Boolean
}

object SemanticsFactory extends ISemanticsFactory[Value] {
  
  def newInstance() = {
    val router = new Router
    val data = new Data(router)
    new Semantics(router, data)
  }

  class Semantics(val router: Router, val data: Data) extends AbstractSemantics[Value] with ISemantics {
    import data._

    type Res[T] = T
    def resToSet[T](res: Res[T]) = Set(res)

    // all data is in the global state
    def normalizeVal(v: Val) = {
      val sends = ((Bag() ++ router.runningServers) map (_.normalizeVal)).flatten
      sends.map(s => resolveExp(s).asInstanceOf[Send])
    }

    val isFullyNondeterministic = false

    override def interp(p: Par): Res[Val] = {
      var res: Res[Val] = null
      try {
        res = interp(p, Map(), null)
        ServerThread.waitUntilStable(router)
      } finally {
        router.runningServers.map(_.waitForTermination())
      }
      println(s"ServerThread instances: ${ServerThread.instanceCounter}")
      res
    }

    def interp(p: Exp, env: Env, currentThread: ServerThread): Res[Val] = p match {
      case BaseCall(b, es) => {
        val vs = es map (interp(_, env, currentThread))
        b.reduce(vs) match {
          case Left(v) => v
          case Right(e) => interp(e, env, currentThread)
        }
      }

      case Var(y) if env.isDefinedAt(y) =>
        env(y)

      case impl@ServerImpl(_) =>
        ServerClosure(impl, env)

      case Spawn(local, e) =>
        interp(e, env, currentThread) match {
          case ServerClosure(impl, senv) =>
            if (local && currentThread != null) {
              val server = new Server(this, impl, senv, currentThread)
              val serverAddr = currentThread.registerServer(server)
              ServerVal(serverAddr)
            }
            else {
              val serverThread = new ServerThread
              val addr = router.registerServer(serverThread)
              serverThread.addr = addr
              val server = new Server(this, impl, senv, serverThread)
              val serverAddr = serverThread.registerServer(server)
              serverThread.start()
              ServerVal(serverAddr)
            }
        }

      case ServiceRef(srv, x) =>
        interp(srv, env, currentThread) match {
          case sval@ServerVal(addr) => ServiceVal(sval, x)
        }


      case Par(ps) =>
        flattenPars(ps).map(interp(_, env, currentThread)).foldLeft[Res[Val]](UnitVal) ((p1,p2) => (p1, p2) match {case (UnitVal,UnitVal) => UnitVal})

      case Send(rcv, args) =>
        interp(rcv, env, currentThread) match {
          case svc@ServiceVal(srvVal, x) =>
            router.lookupAddr(srvVal.addr)
            val argVals = args map (interp(_, env, currentThread))
            router.lookupAddr(srvVal.addr).receiveRequest(SendVal(svc, argVals))
            UnitVal
        }
    }

    def interpSends(server: Server, currentThread: ServerThread): Boolean = {
      for (r <- server.impl.rules) {
        val canSend = matchRule(ServerVal(server.addr), r.ps, server.inbox)
        if (!canSend.isEmpty) {
          val ma = canSend.get
          val (newProg, env, newQueue) = fireRule(ServerVal(server.addr), r, ma, server.inbox)
          server.inbox = newQueue
          interp(newProg, env, currentThread)
          return true
        }
      }
      false
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