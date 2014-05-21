package djc.lang.sem.concurrent_6_thread

/**
 * Created by seba on 09/04/14.
 */
object Router {
  type Addr = String

  var routeTable: collection.mutable.Map[Addr, ServerThread] = null

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
}
