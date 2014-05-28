package djc.lang.sem.nondeterm_5_parallel

import Data._
import djc.lang.Syntax.Var

/**
 * Created by seba on 01/04/14.
 */

object Router {
  type Addr = String

  type ServerAddr = Var

  object ServerAddr {
    val prefix = "ADDR:"

    def apply(addr: Router.Addr) = new ServerAddr(Symbol(prefix + addr))

    def unapply(s: Var): Option[Router.Addr] = getAddr(s.x.name)

    def getAddr(name: String): Option[Router.Addr] =
      if (name.startsWith(prefix))
        Some(name.substring(prefix.length))
      else
        None
  }
}
import Router._

class Router {

  var routeTable: collection.mutable.Map[Addr, ServerClosure] = collection.mutable.Map()

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

  def registerServer(s: ServerClosure): Addr = {
    val addr = nextAddr
    routeTable += (addr -> s)
    addr
  }

  def lookupAddr(addr: Addr): ServerClosure = routeTable(addr)
  def lookupAddr(a: ServerAddr): ServerClosure = a match {
    case ServerAddr(addr) => lookupAddr(addr)
    case _ => throw new IllegalArgumentException(s"Not a server address: $a")
  }

}
