package djc.lang.sem.concurrent_6_thread

import util.Bag

import djc.lang.FlatSyntax._
import Data._

class ServerThread(val impl: ServerImpl, val env: Env) extends Thread {

  var addr: ServerAddr = null

  var dirty = false
  var inbox = Bag[SendVal]()
  var newMessages = Bag[SendVal]()
  var terminate = false

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
  }

  def normalizeVal: Bag[Send] = {
    var res: Bag[Send] = null
    synchronized {
      res = (inbox map (_.toNormalizedProg)) ++ (newMessages map (_.toNormalizedProg))
    }
    res
  }
}