package djc.lang.sem.concurrent_6_thread

import djc.lang.Syntax.Var

/**
 * Created by seba on 09/04/14.
 */
object Router {
  type Addr = String
  type Port = Int

  type ServerAddr = Var

  object ServerAddr {
    val prefix = "ADDR:"
    val portSep = "::"

    def apply(addr: Addr, port: Port) = new ServerAddr(Symbol(prefix + addr + portSep + port))

    def unapply(s: Var): Option[(Addr,Port)] = getAddrWithPort(s.x.name)

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

  var routeTable: collection.mutable.Map[Addr, ServerThread] = collection.mutable.Map()

  var addrNum = 0
  val addrPrefix = "Server@"
  def nextAddr: Addr = {
    addrNum += 1
    val addr = addrPrefix + addrNum
    if (!routeTable.isDefinedAt(addr))
      addr
    else
      nextAddr
  }

  def registerServer(s: ServerThread): Addr = {
    val addr = nextAddr
    routeTable += (addr -> s)
    addr
  }

  def lookupAddr(addr: Addr): ServerThread = routeTable(addr)
  def lookupAddr(a: ServerAddr): ServerThread = a match {
    case ServerAddr(addr, port) => lookupAddr(addr)
    case _ => throw new IllegalArgumentException(s"Not a server address: $a")
  }
  def lookupServer(a: ServerAddr): Server = a match {
    case ServerAddr(addr, port) => lookupAddr(addr).lookupServer(port)
    case _ => throw new IllegalArgumentException(s"Not a server address: $a")
  }

}
