package djc.lang.typ.inference

import djc.lang.TypedSyntaxFamily
import util.Bag
import djc.lang.typ.TypeFamily
import djc.lang.typ.TypedSyntaxOps
import djc.lang.typ.DefaultTypedSyntaxOps


/**
 * Additional syntax which may omit type annotations.
 *
 * None of these implement type erasure. Type inference must map them to
 * annotated syntax.
 */
trait ExtSyntaxFamily extends TypedSyntaxFamily {
  typ: TypeFamily with TypedSyntaxOps =>
    
  abstract class ExtExp extends Exp {
    def eraseType = ???
    override def toFamily(TF: TTF) = ???
  }

  case class UTApp(e: Exp) extends ExtExp

  case class UServerImpl(rules: List[URule]) extends ExtExp {
    //services + arity
    lazy val services: Map[Symbol, Int] = {
      import collection.mutable.{HashMap, MultiMap, Set}
      val mm = new HashMap[Symbol, Set[Int]] with MultiMap[Symbol, Int]
      for (URule(ps, _) <- rules; UPattern(svcname, params) <- ps)
        mm.addBinding(svcname, params.length)

      val m =
        mm.foldLeft(Map[Symbol, Int]()) {
          (m, kv) =>
            val (svcname, arities) = kv
            if (arities.size > 1)
              throw SyntaxException(s"inconsistent arity for service $svcname : $arities")
            m + (svcname -> arities.last)
        }

        m
     }
  }

  case class URule(ps: Bag[UPattern], p: Exp) {
    lazy val rcvars: List[Symbol] = {
      val m = List[Symbol]()
      ps.foldLeft(m)(_ ++ _.params)
    }

    override def toString = s"URule($ps  =>  $p)"
  }
  object URule {
    def apply(ps: UPattern*)(p: Exp): URule = URule(Bag(ps:_*), p)
  }

  case class UPattern(name: Symbol, params: List[Symbol])
  object UPattern {
    def apply(name: Symbol, param: Symbol*) = new UPattern(name, List(param: _*))
  }

  implicit def singletonUPattern(p: UPattern): Bag[UPattern] = Bag(p)
}

//TODO make ExtSyntax and TypedSyntax share the exact same type family
object ExtLanguage extends TypeFamily with ExtSyntaxFamily with DefaultTypedSyntaxOps {
  implicit class UPatternSymbol(val s: Symbol) extends AnyVal {
    def ?(ps: Symbol*) = UPattern(s, List(ps:_*))
  }
  implicit class InfixUPattern(val p1: Bag[UPattern]) extends AnyVal {
    def &&(ps: Bag[UPattern]): Bag[UPattern] = p1 ++ ps
    def &&(p2: UPattern): Bag[UPattern] = p1 + p2
  }
}