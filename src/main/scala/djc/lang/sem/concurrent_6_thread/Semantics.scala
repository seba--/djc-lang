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
    new Semantics(router)
  }

  class Semantics(val router: Router) extends AbstractSemantics[Value] with ISemantics {
    import Data._

    type Res[T] = T
    def resToSet[T](res: Res[T]) = Set(res)

    // all data is in the global state
    def normalizeVal(v: Val) = {
      val sends = ((Bag() ++ router.runningServers) map (_.normalizeVal)).flatten
      sends.map(s => resolveExp(router)(s).asInstanceOf[Send])
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
      case NULL => NULLVal

      case BaseCall(b, es) => {
        val vs = es map (interp(_, env, currentThread))
        b.reduce(vs) match {
          case Left(v) => v
          case Right(e) => interp(e, env, currentThread)
        }
      }

      case Var(y) if env.isDefinedAt(y) =>
        env(y)

      case Addr(i) => ServerVal(i)

      case impl@ServerImpl(_) =>
        ServerClosure(impl, env)

      case Snap(e) => interp(e, env, currentThread) match {
        case ServerVal(i) =>
          val Some((addr, port)) = ServerAddr.getAddrWithPort(i.name)
          router.lookupAddr(addr).lookupServer(port).snapshot
      }

      case Repl(e1, e2) => interp(e1, env, currentThread) match {
        case ServerVal(i) =>
          val Some((addr, port)) = ServerAddr.getAddrWithPort(i.name)
          interp(e1, env, currentThread) match {
            case img@ImgVal(_,_) =>
              router.lookupAddr(addr).lookupServer(port).become(img)
          }

      }

      case Img(e1, reqs) => interp(e1, env, currentThread) match {
        case sc@ServerClosure(_,_) =>
          ImgVal(sc, reqs)
      }

      case Spawn(local, e) =>
        interp(e, env, currentThread) match {
          case ImgVal(ServerClosure(impl, senv), buffer) =>
            if (local && currentThread != null) {
              val server = Server(this, impl, senv, currentThread, buffer)
              val serverAddr = currentThread.registerServer(server)
              ServerVal(serverAddr.i)
            }
            else {
              val serverThread = new ServerThread
              val addr = router.registerServer(serverThread)
              serverThread.addr = addr
              val server = Server(this, impl, senv, serverThread, buffer)
              val serverAddr = serverThread.registerServer(server)
              serverThread.start()
              ServerVal(serverAddr.i)
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
            router.lookupAddr(Addr(srvVal.addr))
            val argVals = args map (interp(_, env, currentThread))
            router.lookupAddr(Addr(srvVal.addr)).receiveRequest(Addr(srvVal.addr), Request(x, argVals))
            UnitVal
        }
    }

    def interpSends(server: Server, currentThread: ServerThread): Boolean = {
      for (r <- server.impl.rules) {
        val canSend = matchRule(ServerVal(server.addr.i), r.ps, server.inbox)
        if (!canSend.isEmpty) {
          val ma = canSend.get
          val (newProg, env, newQueue) = fireRule(ServerVal(server.addr.i), r, ma, server.inbox)
          server.inbox = newQueue
          interp(newProg, env, currentThread)
          return true
        }
      }
      false
    }

    def matchRule(server: ServerVal, pats: Bag[Pattern], sends: Bag[Request]): Option[Match] =
      if (pats.isEmpty)
        Some(Match(Map(), Bag()))
      else {
        val name = pats.head.name
        val params = pats.head.params
        val matchingSends = sends.filter({
          case Request(`name`, args) => params.size == args.size
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

    def fireRule(server: ServerVal, rule: Rule, ma: Match, oldQueue: Bag[Request]): (Exp, Env, Bag[Request]) = {
      val s = router.lookupServer(Addr(server.addr))
      val env = s.env ++ ma.subst + ('this -> server)
      val newQueue = oldQueue -- ma.used
      (Par(rule.p), env, newQueue)
    }
  }
}