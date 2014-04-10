package djc.lang.sem.concurrent_6_thread

import util.Bag

import djc.lang.Syntax._
import Data._

class ServerThread(val impl: ServerImpl, val env: Env) extends Thread {

  var addr: ServerAddr = null

  var dirty = false
  var inbox = Bag[SendClosure]()
  var newMessages = Bag[SendClosure]()
  var terminate = false

  def sendRequest(cl: SendClosure) {
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
  }

  def normalizeVal: Bag[Send] = {
    var res: Bag[Send] = null
    synchronized {
      res = (inbox map (_.normalize)) ++ (newMessages map (_.normalize))
    }
    res
  }
}