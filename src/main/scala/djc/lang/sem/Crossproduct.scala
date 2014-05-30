package djc.lang.sem

import util.Bag


object Crossproduct {
  def nondeterministic[T,U](ts: Set[T], f: T => Set[U]): Set[U] =
    (ts map f).flatten

  def crossProduct[T](tss: Bag[Set[Bag[T]]]): Set[Bag[T]] =
    if (tss.isEmpty)
      Set(Bag())
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
           ts <- tss.head)
      yield ts :: prod
    }

  def crossProductMap[K,V](tss: Bag[Set[Map[K,Bag[V]]]]): Set[Map[K,Bag[V]]] =
    if (tss.isEmpty)
      Set(Map())
    else if (tss.tail.isEmpty)
      tss.head
    else {
      val rest = crossProductMap(tss.tail)
      for (prod <- rest;
           ts <- tss.head)
      yield mergeMaps(ts, prod)
    }


  implicit class MapOps[K,V](m : Map[K,Bag[V]]) {
    def merge(that: Map[K,Bag[V]]) = mergeMaps(m, that)
    def &&&(that: Map[K,Bag[V]]) = mergeMaps(m, that)
  }

  def mergeMaps[K,V](m1: Map[K,Bag[V]], m2: Map[K,Bag[V]]) = {
    var m = m2
    for((k,v) <- m1)
      m2.get(k) match {
        case None => m += k -> v
        case Some(v2) => m += k -> (v ++ v2)
      }
    m
  }


}