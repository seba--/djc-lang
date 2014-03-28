package djc.lang

import util.Bag


object Crossproduct {
  def crossProduct[T](tss: Bag[Set[Bag[T]]]): Set[Bag[T]] =
    if (tss.isEmpty)
      throw new IllegalArgumentException("Cross product requires non-empty input list")
    else if (tss.tail.isEmpty)
      tss.head
    else {
      val rest = crossProduct(tss.tail)
      for (prod <- rest;
           ts <- tss.head;
           t <- ts)
      yield prod + t
    }
}