package util

import scala.collection.mutable.{ListBuffer, Builder}
import scala.collection.immutable.ListMap
import scala.collection.{GenSet, TraversableLike, SetLike}
import scala.collection.generic._
import scala.Iterator

class Bag[T](m: ListMap[T, Int]) extends scala.collection.immutable.Set[T]
                                    with GenericSetTemplate[T, Bag]
                                    with SetLike[T, Bag[T]] {

  override def companion: GenericCompanion[Bag] = Bag

  override def seq: Bag[T] = this

  override def +(t: T): Bag[T] = new Bag(m + (t -> m.getOrElse(t, 1)))

  override def -(t: T) = {
    val count = m.getOrElse(t, 0)
    if (count > 1)
      new Bag(m updated (t, count-1))
    else
      new Bag(m - t)
  }

  // TODO: need those?
//  override def equals(that: Any) =
//    that.isInstanceOf[Bag[T]] && m == that.asInstanceOf[this.type].m
//
//  override def hashCode = m.hashCode

  override def contains(t: T) = m.contains(t)

  /* Need to override string, so that it's not the Function1's string that gets mixed in.
   */
  override def toString = super[SetLike].toString

  override def iterator = new Iterator[T]() {
    val mapIterator = m.iterator
    var elem: T = null.asInstanceOf[T]
    var count = -1
    def hasNext = count > 0 || mapIterator.hasNext
    def next() = {
      if (count > 0) {
        count -= 1
        elem
      }
      else {
        val p = mapIterator.next()
        elem = p._1
        count = p._2 - 1
        elem
      }
    }
  }
}
object Bag extends SetFactory[Bag] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Bag[A]] = setCanBuildFrom[A]
  override def newBuilder[A]: Builder[A, Bag[A]] = new BagBuilder[A](ListBuffer())
}

class BagBuilder[T](l: ListBuffer[T]) extends Builder[T, Bag[T]] {
  def clear() = l.clear()
  def +=(t: T) = {
    l+=t
    this
  }
  def result = {
    var bag = new Bag[T](ListMap())
    for (t <- l)
      bag = bag + t
    bag
  }
}

