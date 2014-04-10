package djc.lang.sem

import util.Bag
import djc.lang.FlatSyntax._

object FlatSubstitution {
  def subst(x: Symbol, p: Prog): Mapper.Type = {
    (Some((x2: Symbol, s2: Prog, p2: Prog) => substDef(x, p, x2, s2, p2)), // Def
     None, // Par
     None, // Send
     Some((x2: Symbol) => if (x == x2) p else Var(x2)), // Var
     None, // ServiceRef
     None, // ServerImpl
     Some((ps: Bag[Pattern], rp: Prog) => substRule(x, p, ps, rp)), // Rule
     None) // Pattern
  }
  def substRule(x: Symbol, repl: Prog, ps: Bag[Pattern], p: Prog): Rule = {
    val patVars = (ps map (_.fold(freeVars))).flatten
    if (patVars contains x)
      Rule(ps, p)
    else
      Rule(ps, p.map(subst(x, repl)))
  }

  def substDef(x: Symbol, repl: Prog, x2: Symbol, s2: Prog, p2: Prog) = {
    val isThis = x == 'this
    val replVars = repl.fold(freeVars)
    val captureAvoiding = !replVars.contains(x2)
    lazy val x2fresh = gensym(x2, replVars)
    lazy val p2fresh = p2.map(subst(x2, Var(x2fresh)))
    val (x2res, p2res) = if (captureAvoiding) (x2,p2) else (x2fresh,p2fresh)

    if (isThis)
      Def(x2res, s2, p2res.map(subst(x, repl)))
    else if (x == x2)
      Def(x2, s2.map(subst(x, repl)), p2)
    else
      Def(x2res, s2.map(subst(x, repl)), p2res.map(subst(x, repl)))
  }


  val freeVars: Folder.Type[Set[Symbol]] = {
    type R = Set[Symbol]
    ((x: Symbol, ds: R, ps: R) => (ds-'this) ++ (ps-x), // Def
     (xs: Bag[R]) => xs.flatten, // Par
     (srv: R, args: List[R]) => srv ++ args.flatten, // Send
     (x: Symbol) => Set(x), // Var
     (srv: R, x: Symbol) => srv, // ServiceRef
     (xs: Bag[R]) => xs.flatten, // ServerImpl
     (ps: Bag[R], p: R) => p -- ps.flatten, // Rule
     (name: Symbol, params: List[Symbol]) => params.toSet) // Pattern
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