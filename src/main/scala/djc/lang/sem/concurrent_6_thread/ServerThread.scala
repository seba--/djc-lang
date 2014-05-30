package djc.lang.sem.concurrent_6_thread

import util.Bag

import djc.lang.Syntax._
import Data._

import Router._

class ServerThread extends Thread {

  ServerThread.incCounter()

  var addr: Addr = null
  private var nextPort = 0
  private val servers: collection.mutable.Map[Router.Port, Server] = collection.mutable.Map()

  private var terminate = false
  var terminated = false

  def receiveRequest(cl: ISendVal) {
    if (terminate || terminated)
      throw new IllegalStateException(s"Received send after server termination ($terminate,$terminated): $cl")

    cl.rcvAddr match {
      case ServerAddr(addr, port) => servers(port).receiveRequest(cl)
    }
  }

  override def run() {
    while (!terminate) {
      val ss = synchronized {servers.values}
      ss map (_.tryFireRules())
      Thread.sleep(1)
    }
    if (!terminate)
      throw new IllegalStateException(s"Unexpected server termination")
    terminated = true
  }

  def normalizeVal: Bag[Send] = {
    val ss = synchronized {servers.values}
    Bag() ++ servers.values flatMap (_.normalizeVal)
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
      synchronized { servers.hashCode }

  override def equals(a: Any) =
    a.isInstanceOf[ServerThread] && synchronized { a.asInstanceOf[ServerThread].servers == servers }

  def lookupServer(port: Port) =
    synchronized { servers(port) }

  def registerServer(server: Server): ServerAddr = {
    synchronized { servers += (nextPort -> server) }
    nextPort += 1

    val a = ServerAddr(addr, nextPort - 1)
    server.addr = a
    a

  }
}

object ServerThread {
  var counter = 0
  def incCounter() = synchronized( counter += 1 )

  def waitUntilStable(ss: Iterable[ServerThread]) {
    var last = ss.hashCode()
    while (true) {
      Thread.sleep(2000)
      val hash = ss.hashCode()
      if (hash == last)
        return
      last = hash
    }
  }
}