package djc.lang.sem.concurrent_6_thread

import util.Bag

import djc.lang.Syntax._
import Data._

import Router._

class ServerThread extends Thread {

  ServerThread.incInstanceCounter()

  var addr: Addr = null
  private var nextPort = 0
  private val servers: collection.mutable.Map[Router.Port, Server] = collection.mutable.Map()

  private var terminate = false
  var terminated = false
  private var _requestCounter = 0

  def requestCounter = synchronized(_requestCounter)

  def receiveRequest(cl: ISendVal) {
    synchronized { _requestCounter += 1 }

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
  var instanceCounter = 0
  def incInstanceCounter() = synchronized( instanceCounter += 1 )

  def waitUntilStable(ss: Iterable[ServerThread]) {
    var last = ss map (_.requestCounter)
    while (true) {
      Thread.sleep(50)
      val next = ss map (_.requestCounter)
      if (next == last)
        return
      last = next
    }
  }
}