package djc.lang.sem

import util.Bag
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder


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
      Set(Bag())
    else if (tss.tail.isEmpty)
      tss.head map (Bag(_))
    else {
      val rest = crossProductAlt(tss.tail)
      val hd = tss.head
      if (hd.isEmpty)
        rest
      else if (hd.size == 1) {
        val hdhd = hd.head
        rest map (_ + hdhd)
      }
      else
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

  def crossProductMap2inner[K,V](tss: Bag[Set[Map[K,Bag[V]]]]): Set[List[Map[K,Bag[V]]]] =
    if (tss.isEmpty)
      Set(List(Map()))
    else if (tss.tail.isEmpty)
      tss.head map (List(_))
    else {
      val rest = crossProductMap2inner(tss.tail)
      for (prod <- rest;
           ts <- tss.head)
      yield ts :: prod
    }

  def crossProductMap2[K,V](tss: Bag[Set[Map[K,Bag[V]]]]): Set[Map[K,Bag[V]]] =
    crossProductMap2inner(tss) map (mergeAllMaps(_))



  def crossProductNew[K,V](tss: Map[K, Set[V]]): Set[Map[K,V]] =
    if (tss.isEmpty)
      Set(Map[K,V]())
    else if (tss.tail.isEmpty) {
      val (k,set) = tss.head
      set map (v => Map(k -> v))
    }
    else {
      val rest = crossProductNew(tss.tail)
      val (k,set) = tss.head
      for (prod <- rest;
           v <- set)
        yield prod + ((k,v))
    }


  implicit class MapOps[K,V](m : Map[K,Bag[V]]) {
    def merge(that: Map[K,Bag[V]]) = mergeMaps(m, that)
    def &&&(that: Map[K,Bag[V]]) = mergeMaps(m, that)
  }

  def mergeAllMaps[K,V](ms: List[Map[K,Bag[V]]]) = {
    var m = ms.head
    for(m2 <- ms.tail;
        (k,v) <- m2)
      m.get(k) match {
        case None => m += k -> v
        case Some(v2) => m += k -> (v ++ v2)
      }
    m
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

  def mergeIntoMap[K,V](m: Map[K,Bag[V]], newVals: Bag[(K,V)]) = {
    var res = m
    for((k,v) <- newVals)
      res.get(k) match {
        case None => res += k -> Bag(v)
        case Some(vs) => res += k -> (vs + v)
      }
    res
  }


  implicit def buildSetFromMap[Val] = new CanBuildFrom[Map[_,_], Val, Set [Val]] {
    def apply(from: Map[_,_]) = Set.newBuilder[Val]
    def apply() = Set.newBuilder[Val]
  }

}