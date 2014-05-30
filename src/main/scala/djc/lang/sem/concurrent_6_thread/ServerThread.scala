package djc.lang.sem.concurrent_6_thread

import util.Bag

import djc.lang.Syntax._
import Data._

import Router._

class ServerThread(sem: SemanticsInner, val impl: ServerImpl, val env: Env) extends Thread {

  var addr: ServerAddr = null

  var dirty = false
  var inbox = Bag[ISendVal]()
  var newMessages = Bag[ISendVal]()
  private var terminate = false
  var terminated = false

  def sendRequest(cl: ISendVal) {
    if (terminate || terminated)
      throw new IllegalStateException(s"Received send after server termination ($terminate,$terminated): $cl")
    synchronized {
//      println(s"**RECEIVE: $cl")
      newMessages += cl
      dirty = true
    }
  }

  override def run() {
    while (!terminate || dirty) {
      if (dirty) {
        synchronized {
//          newMessages map (m => println(s"**PROCESS: $m"))
          inbox ++= newMessages
          newMessages = Bag()
          dirty = false
        }
        sem.interpSends(this)
      }
      else
        Thread.sleep(1)
    }
    if (!terminate)
      throw new IllegalStateException(s"Unexpected server termination")
    terminated = true
  }

  def normalizeVal: Bag[Send] = {
    var res: Bag[Send] = null
    synchronized {
      res = (inbox map (_.toNormalizedResolvedProg)) ++ (newMessages map (_.toNormalizedResolvedProg))
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
    var last = ss.hashCode()
    while (true) {
      Thread.sleep(1000)
      val hash = ss.hashCode()
      if (hash == last)
        return
      last = hash
    }
  }
}