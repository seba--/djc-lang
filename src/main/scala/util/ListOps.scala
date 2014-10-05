package util

object ListOps {
  implicit class LazyFoldList[S](val l: List[S]) extends AnyVal {
    def lazyFoldr[T](base: => T)(f: (S, => T) => T): T = l match {
      case Nil => base
      case x :: xs => f(x, xs.lazyFoldr(base)(f))
    }
  }
}
