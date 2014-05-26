package djc.lang

object Gensym {
  def gensyms(l: List[Symbol], used: Set[Symbol]): List[Symbol] = l match {
    case Nil => Nil
    case s :: ss =>
      val sfresh = gensym(s, used)
      sfresh :: gensyms(ss, used + sfresh)
  }
  def gensym(x: Symbol, used: Set[Symbol]): Symbol = gensym(x, 0, used)
  def gensym(x: Symbol, i: Int, used: Set[Symbol]): Symbol = {
    val s = Symbol(s"${x.name}_$i")
    if (used contains s)
      gensym(x, i+1, used)
    else
      s
  }
}