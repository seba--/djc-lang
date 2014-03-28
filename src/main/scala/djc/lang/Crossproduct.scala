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

  def crossProductMap[K,V](tss: Bag[Set[Map[K,Bag[V]]]]): Set[Map[K,Bag[V]]] =
    if (tss.isEmpty)
      throw new IllegalArgumentException("Cross product requires non-empty input list")
    else if (tss.tail.isEmpty)
      tss.head
    else {
      val rest = crossProductMap(tss.tail)
      for (prod <- rest;
           ts <- tss.head)
      yield mergeMaps(prod, ts)
    }

  def mergeMaps[K,V](m1: Map[K,Bag[V]], m2: Map[K,Bag[V]]) = {
    var m = Map[K, Bag[V]]()
    for (k <- m1.keys)
      m = m + (k -> (m1(k) ++ m2(k)))
    m
  }
}