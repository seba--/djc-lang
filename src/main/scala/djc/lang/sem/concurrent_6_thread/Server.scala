package djc.lang.sem.concurrent_6_thread

import util.Bag

import djc.lang.Syntax._
import Data._

import Router._

class Server(sem: ISemantics, val impl: ServerImpl, val env: Env, currentThread: ServerThread) {

  var addr: ServerAddr = null

  private var dirty = false
  var inbox = Bag[ISendVal]()
  private var newMessages = Bag[ISendVal]()

  def hasNewMessages = synchronized (dirty)

  def receiveRequest(cl: ISendVal) {
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
      val matched = sem.interpSends(this, currentThread)
      synchronized{
        dirty = matched
      }
    }
  }

  def normalizeVal: Bag[Send] =
    synchronized {
      (inbox map (_.toNormalizedResolvedProg)) ++ (newMessages map (_.toNormalizedResolvedProg))
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