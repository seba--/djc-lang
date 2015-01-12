package util

import scala.collection.immutable.Queue

object ListOps {
  implicit class LazyFoldList[S](val l: List[S]) extends AnyVal {
    def lazyFoldr[T](base: => T)(f: (S, => T) => T): T = l match {
      case Nil => base
      case x :: xs => f(x, xs.lazyFoldr(base)(f))
    }
  }
  implicit class LazyFoldQ[S](val q: Queue[S]) extends AnyVal {
    def lazyFoldr[T](base: => T)(f: (S, => T) => T): T = {
      if (q.isEmpty)
        base
      else {
        val (hd, tl) = q.dequeue
        f(hd, tl.lazyFoldr(base)(f))
      }
    }
  }
}
