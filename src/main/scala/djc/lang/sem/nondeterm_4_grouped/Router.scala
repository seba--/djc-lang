package djc.lang.sem.nondeterm_4_grouped

import Data._

object Router {
  type Addr = String

  var routeTable: collection.mutable.Map[Addr, ServerClosure] = null

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
    s.addr = addr
    addr
  }

  def lookupAddr(addr: Addr): ServerClosure = routeTable(addr)
}
