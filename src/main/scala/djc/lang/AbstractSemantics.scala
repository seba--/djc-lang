package djc.lang

import util.Bag

trait AbstractSemantics {
  type Val // abstract value type
  def emptyVal: Val
  def normalizeVal(v: Val): Bag[Send]

  type Res[T] = Set[T] // nondeterminstic result as set of values

  def interp(p: Prog): Res[Val]

  def nondeterministic[T,U](ts: Res[T], f: T => Res[U]): Res[U] =
    (ts map f).flatten

  def crossProduct[T](tss: Bag[Res[Bag[T]]]): Res[Bag[T]] =
    if (tss.isEmpty)
      Set(Bag())
    else {
      val rest = crossProduct(tss.tail)
      for (ts <- tss.head;
           t <- ts;
           prod <- rest)
      yield prod + t
    }

}