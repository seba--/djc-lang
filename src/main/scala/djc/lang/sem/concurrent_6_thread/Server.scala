package djc.lang.sem.concurrent_6_thread

import util.Bag

import djc.lang.Syntax._
import djc.lang.sem.concurrent_6_thread.Data._

import Router._

class Server(val sem: ISemantics, impl_ : ServerImpl, env_ : Env, currentThread: ServerThread) {
  import Data._
  var addr: ServerAddr = null

  var _impl: ServerImpl = impl_
  var _env: Env = env_
  def impl = _impl
  def env = _env

  private var dirty = false
  var inbox = Bag[Request]()
  private var newMessages = Bag[Request]()

  def hasNewMessages = synchronized (dirty)

  def receiveRequest(cl: Request) {
    synchronized {
      newMessages += cl
      dirty = true
    }
  }

  def tryFireRules() {
    if (dirty) {
      synchronized {
        inbox ++= newMessages
        newMessages = Bag()
        dirty = false
      }
      while(sem.interpSends(this, currentThread)) {}
    }
  }

  def snapshot: ImgVal = synchronized {
    inbox ++= newMessages
    ImgVal(ServerClosure(impl, env), inbox)
  }

  def become(img: ImgVal) = synchronized {
    val ServerClosure(impl1, env1) = img.sc
    _impl = impl1
    _env = env1
    inbox = img.buffer
    UnitVal
  }

  def normalizeVal: Bag[Send] =
    synchronized {
      (inbox ++ newMessages) map {
        case Request(svc, args) =>
          Send(ServiceRef(addr, svc), args map (_.toExp))
      }
    }

  override def hashCode =
    synchronized { inbox.hashCode() * 31 + newMessages.hashCode() }

  override def equals(a: Any) = {
    if (!a.isInstanceOf[Server])
      false
    else {
      val other = a.asInstanceOf[Server]
      synchronized { inbox == other.inbox && newMessages == other.newMessages }
    }
  }
}

object Server {
  def apply(sem: ISemantics, impl: ServerImpl, env: Env, currentThread: ServerThread): Server = new Server(sem, impl, env, currentThread)

  def apply(sem: ISemantics, impl: ServerImpl, env: Env, currentThread: ServerThread, initialInbox: Bag[Request]): Server = {
    val srv = new Server(sem, impl, env, currentThread)
    srv.inbox = initialInbox
    srv
  }
}