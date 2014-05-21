package djc.lang.sem

import util.Bag


object Crossproduct {
  def nondeterministic[T,U](ts: Set[T], f: T => Set[U]): Set[U] =
    (ts map f).flatten

  def crossProduct[T](tss: Bag[Set[Bag[T]]]): Set[Bag[T]] =
    if (tss.isEmpty)
      throw new IllegalArgumentException("Cross product requires non-empty input list")
    else if (tss.tail.isEmpty)
      tss.head
    else {
      val rest = crossProduct(tss.tail)
      if (tss.head.isEmpty)
        rest
      else
        for (prod <- rest;
             ts <- tss.head;
             t <- ts)
        yield prod + t
    }

  def crossProductAlt[T](tss: Bag[Set[T]]): Set[Bag[T]] =
    if (tss.isEmpty)
      throw new IllegalArgumentException("Cross product requires non-empty input list")
    else if (tss.tail.isEmpty)
      tss.head map (Bag(_))
    else {
      val rest = crossProductAlt(tss.tail)
      for (prod <- rest;
           t <- tss.head)
      yield prod + t
    }

  def crossProductList[T](tss: List[Set[T]]): Set[List[T]] =
    if (tss.isEmpty)
      Set(List())
    else if (tss.tail.isEmpty)
      tss.head map (List(_))
    else {
      val rest = crossProductList(tss.tail)
      for (prod <- rest;
           t <- tss.head)
      yield t :: prod
    }

  def crossProductMap[K,V](tss: Bag[Set[Map[K,Bag[V]]]]): Set[Map[K,Bag[V]]] =
    if (tss.isEmpty)
      Set()
    else if (tss.tail.isEmpty)
      tss.head
    else {
      val rest = crossProductMap(tss.tail)
      for (prod <- rest;
           ts <- tss.head)
      yield mergeMaps(prod, ts)
    }


  implicit class MapOps[K,V](m : Map[K,Bag[V]]) {
    def merge(that: Map[K,Bag[V]]) = mergeMaps(m, that)
    def &&&(that: Map[K,Bag[V]]) = mergeMaps(m, that)
  }

  def mergeMaps[K,V](m1: Map[K,Bag[V]], m2: Map[K,Bag[V]]) = {
    var m = Map[K, Bag[V]]()
    for(k <- m1.keySet & m2.keySet)
      m = m + (k -> (m1(k) ++ m2(k)))
    for(k <- m1.keySet &~ m2.keySet)
      m = m + (k -> m1(k))
    for(k <- m2.keySet &~ m1.keySet)
      m = m + (k -> m2(k))
    m
  }


}