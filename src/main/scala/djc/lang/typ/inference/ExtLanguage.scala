package djc.lang.typ.inference

import djc.lang.Gensym._
import djc.lang.TypedSyntaxFamily
import util.Bag
import util.ListOps._
import djc.lang.typ.{Types, TypedSyntaxOps}


/**
 * Additional syntax which may omit type annotations.
 *
 * None of these implement type erasure. Type inference must map them to
 * annotated syntax.
 */
trait ExtSyntaxFamily extends TypedSyntaxFamily {
  self =>
  val op: TypedSyntaxOps { val syntax: self.type }

  abstract class ExtExp extends Exp {
    override val family: ExtSyntaxFamily = self
    def eraseType = ???
    override def toFamily(TF: TSF) = ???
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
  object UServerImpl {
    def apply(rules: URule*): UServerImpl = UServerImpl(List(rules:_*))
  }

  case class URule(ps: Bag[UPattern], p: Exp) {
    val family: ExtSyntaxFamily = self
    lazy val rcvars: List[Symbol] = {
      val m = List[Symbol]()
      ps.foldLeft(m)(_ ++ _.params)
    }

    override def toString = s"URule($ps  =>  $p)"
  }
  object URule {
    def apply(ps: UPattern*)(p: Exp): URule = URule(Bag(ps:_*), p)
  }

  case class UPattern(name: Symbol, params: List[Symbol]) {
    val family: ExtSyntaxFamily = self
  }
  object UPattern {
    def apply(name: Symbol, param: Symbol*) = new UPattern(name, List(param: _*))
  }

  implicit def singletonUPattern(p: UPattern): Bag[UPattern] = Bag(p)

  object USpawn {
    def apply(e: UServerImpl): Spawn = Spawn(false, e)
  }
  object USpawnLocal {
    def apply(e: UServerImpl): Spawn = Spawn(true, e)
  }
  object UServer {
    def apply(rules: List[URule]): Spawn = Spawn(false, UServerImpl(rules))
    def apply(rules: URule*): Spawn = Spawn(false, UServerImpl(List(rules: _*)))
  }
  object ULocalServer {
    def apply(rules: List[URule]): Spawn = Spawn(true, UServerImpl(rules))
    def apply(rules: URule*): Spawn = Spawn(true, UServerImpl(List(rules: _*)))
  }


  trait Mapper extends super.Mapper {
    override def map: TMapE = {
      case UTApp(e) =>
        UTApp(map(e))
      case UServerImpl(rules) =>
        UServerImpl(rules map mapURule)
      case e => super.map(e)
    }

    def mapURule(rule: URule): URule = {
      val URule(ps, e) = rule
      URule(ps map mapUPattern, map(e))
    }

    def mapUPattern(p: UPattern): UPattern = p
  }

  trait StrictFold[T] extends super.StrictFold[T] {
    override def fold(init: T): FoldE = {
      case UTApp(e) =>
        fold(init)(e)
      case UServerImpl(rules) =>
        rules.foldLeft(init)(foldURule(_)(_))
      case e => super.fold(init)(e)
    }

    def foldURule(init: T)(r: URule): T = {
      val URule(ps, e) = r
      fold(ps.foldLeft(init)(foldUPattern(_)(_)))(e)
    }

    def foldUPattern(init: T)(p: UPattern): T = init
  }

  trait LazyFold[T] extends super.LazyFold[T] {
    override def fold(init: => T): FoldE = {
      case UTApp(e) =>
        fold(init)(e)
      case UServerImpl(rules) =>
        rules.lazyFoldr(init)((e,t) => foldURule(t)(e))
      case e => super.fold(init)(e)
    }

    def foldURule(init: => T)(r: URule): T = {
      val URule(ps, e) = r
      ps.lazyFoldr(fold(init)(e))((e, t) => foldUPattern(t)(e))
    }

    def foldUPattern(init: => T)(p: UPattern): T = init
  }
}

trait ExtTypedSyntaxOps extends TypedSyntaxOps {
  val syntax: ExtSyntaxFamily
  import syntax.types.Type
  import syntax._

  trait FreeVars extends super.FreeVars with syntax.StrictFold[Set[Symbol]] {
    override def fold(init: Set[Symbol]): FoldE = {
      case srv@UServerImpl(rules) =>
        super.fold(init)(srv) - 'this
      case e => super.fold(init)(e)
    }

    override def foldURule(init: Set[Symbol])(r: URule): Set[Symbol] = {
      super.foldURule(init)(r) -- r.rcvars.toSet
    }
  }

  class Subst(x: Symbol, repl: Exp) extends super.Subst(x, repl) with syntax.Mapper {
    override def map: TMapE = {
      case srv@UServerImpl(rules) if x == 'this =>
        srv
      case e => super.map(e)
    }

    override def mapURule(rule: URule): URule = {
      val URule(ps, prog) = rule
      val boundNames = rule.rcvars
      val conflictingNames = boundNames filter replVars
      val captureAvoiding = conflictingNames.isEmpty

      lazy val replacements = conflictingNames zip gensyms(conflictingNames, replVars)
      lazy val progfresh = replacements.foldLeft(prog) {
        (p, kv) => substExp(kv._1, Var(kv._2))(p)
      }
      lazy val rename: Symbol => Symbol = replacements.toMap orElse {
        case s: Symbol => s
      }
      lazy val psfresh = ps map {
        pat =>
          UPattern(pat.name, pat.params map rename)
      }
      val (psres, progres) = if (captureAvoiding) (ps, prog) else (psfresh, progfresh)

      if (boundNames contains x)
        rule
      else
        URule(psres, map(progres))
    }
  }

  def freeTypeVars = FreeTypeVars
  def freeVars = FreeVars
  def substExp(x: Symbol, repl: Exp) = new Subst(x, repl)
  def substType(sub: Map[Symbol, Type]) = new SubstType {
    val typeMapper = syntax.types.op.substType(sub)
  }

  protected object FreeVars extends FreeVars
  protected object FreeTypeVars extends FreeTypeVars
}

object ExtLanguage extends ExtSyntaxFamily {
  val types = Types
  val op = new ExtTypedSyntaxOps {
    val syntax = ExtLanguage
  }

  implicit def upatternSingletonBag(p: UPattern): Bag[UPattern] = Bag(p)

  implicit class UPatternSymbol(val s: Symbol) extends AnyVal {
    def ??(ps: Symbol*) = UPattern(s, List(ps:_*))
  }
  implicit class InfixUPattern(val p1: Bag[UPattern]) extends AnyVal {
    def &&(ps: Bag[UPattern]): Bag[UPattern] = p1 ++ ps
    def &&(p2: UPattern): Bag[UPattern] = p1 + p2
  }
}