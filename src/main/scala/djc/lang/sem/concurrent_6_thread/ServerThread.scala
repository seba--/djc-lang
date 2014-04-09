package djc.lang.sem.concurrent_6_thread

import util.Bag

import djc.lang._
import Data._

class ServerThread(val impl: ServerImpl, val env: Env) extends Thread {

  var addr: ServerAddr = null

  var dirty = false
  var inbox = Bag[Closure]()
  var newMessages = Bag[Closure]()
  var terminate = false

  def sendRequest(cl: Closure) {
    synchronized {
      newMessages += cl
      dirty = true
    }
  }

  override def run() {
    while (!terminate) {
      if (dirty) {
        synchronized {
          inbox ++= newMessages
          newMessages = Bag()
          dirty = false
        }
        tryFireRule()
      }
      else
        Thread.sleep(1)
    }
  }

  def tryFireRule() {
    import Semantics._

    for (r <- impl.rules) {
      val canSend = matchRule(r.ps, inbox)
      if (!canSend.isEmpty) {
        val (subst, used) = canSend.head
        val (newProg, newInbox) = fireRule(addr, r, subst, used, inbox)
        inbox = newInbox
        interp(newProg, env)
        return
      }

    }
  }

  def normalizeVal: Bag[Send] = {
    var res: Bag[Send] = null
    synchronized {
      res = (inbox map (_.normalize)) ++ (newMessages map (_.normalize))
    }
    res
  }
}