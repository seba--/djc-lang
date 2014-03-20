package djc.lang

import scala.language.higherKinds

import util.Bag

trait AbstractSemantics[D] {
  type Val // abstract value type
  def emptyVal: Val
  def normalizeVal(v: Val): Bag[Send]
  def valData(v: Val): Bag[D]
  def addSend(v: Val, s: D): Val

  type Res[T] = Set[T] // nondeterminstic result as set of values

  def interp(p: Prog): Res[Val]

  def nondeterministic[T,U](ts: Res[T], f: T => Res[U]): Res[U] =
    (ts map f).flatten

  def crossProduct(tss: Bag[Res[Val]]): Res[Val] =
    if (tss.isEmpty)
      throw new IllegalArgumentException("Cross product requires non-empty input list")
    else if (tss.tail.isEmpty)
      tss.head
    else {
      val rest = crossProduct(tss.tail)
      for (prod <- rest;
           ts <- tss.head;
           t <- valData(ts))
      yield addSend(prod, t)
    }
}