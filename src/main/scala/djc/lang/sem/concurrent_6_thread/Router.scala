package djc.lang.sem.concurrent_6_thread

import djc.lang.Syntax
import djc.lang.Syntax.Var

/**
 * Created by seba on 09/04/14.
 */
object Router {
  type Addr = String
  type Port = Int

  type ServerAddr = Syntax.Addr

  object ServerAddr {
    val prefix = "ADDR:"
    val portSep = "::"

    def apply(addr: Addr, port: Port) = new ServerAddr(Symbol(prefix + addr + portSep + port))

    def unapply(s: ServerAddr): Option[(Addr,Port)] = getAddrWithPort(s.i.name)

    def getAddrWithPort(name: String): Option[(Addr,Port)] =
      if (name.startsWith(prefix)) {
        val addrWithPort = name.substring(prefix.length)
        val sepIndex = addrWithPort.indexOf(portSep)
        val addr = addrWithPort.substring(0, sepIndex)
        val port = addrWithPort.substring(sepIndex + portSep.length)
        Some((addr, port.toInt))
      }
      else
        None
  }
}
import Router._

class Router {

  private var routeTable: collection.mutable.Map[Addr, ServerThread] = collection.mutable.Map()

  def runningServers = synchronized (routeTable.values)

  private var addrNum = 0
  private val addrPrefix = "Server@"
  private def nextAddr: Addr = {
    addrNum += 1
    val addr = addrPrefix + addrNum
    if (!routeTable.isDefinedAt(addr))
      addr
    else
      nextAddr
  }

  def registerServer(s: ServerThread): Addr = {
    synchronized {
      val addr = nextAddr
      routeTable += (addr -> s)
      addr
    }
  }

  def lookupAddr(addr: Addr): ServerThread = synchronized(routeTable(addr))
  def lookupAddr(a: ServerAddr): ServerThread = a match {
    case ServerAddr(addr, port) => lookupAddr(addr)
    case _ => throw new IllegalArgumentException(s"Not a server address: $a")
  }
  def lookupServer(a: ServerAddr): Server = a match {
    case ServerAddr(addr, port) => lookupAddr(addr).lookupServer(port)
    case _ => throw new IllegalArgumentException(s"Not a server address: $a")
  }

}
