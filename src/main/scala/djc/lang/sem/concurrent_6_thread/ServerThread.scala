package djc.lang.sem.concurrent_6_thread

import util.Bag

import djc.lang.Syntax._
import Data._

class ServerThread(val impl: ServerImpl, val env: Env) extends Thread {

  var addr: ServerAddr = null

  var dirty = false
  var inbox = Bag[SendVal]()
  var newMessages = Bag[SendVal]()
  var terminate = false
  var terminated = false

  def sendRequest(cl: SendVal) {
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
        Semantics.interpSends(this)
      }
      else
        Thread.sleep(1)
    }
    terminated = true
  }

  def normalizeVal: Bag[Send] = {
    var res: Bag[Send] = null
    synchronized {
      res = (inbox map (_.toNormalizedProg)) ++ (newMessages map (_.toNormalizedProg))
    }
    res
  }
  
  def waitForTermination() = {
    terminate = true
    while (!terminated)
      Thread.sleep(2)
  }

  override def hashCode =
    if (terminated)
      0
    else
      inbox.hashCode() * 31 + newMessages.hashCode()
}

object ServerThread {
  def waitUntilStable(ss: Iterable[ServerThread]) {
    var last = ssHash(ss)
    while (true) {
      Thread.sleep(50)
      val hash = ssHash(ss)
      if (hash == last)
        return
      last = hash
    }
  }

  def ssHash(ss: Iterable[ServerThread]) = ss.foldLeft(0)((h, s) => h + s.hashCode)
}