package djc.lang

import org.scalatest.FunSuite
import djc.lang.sem.{ISemanticsFactory, Substitution, AbstractSemantics}
import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived._
import util.Bag

import djc.lang.typ.Checker._
import djc.lang.typ.Types._
import djc.lang.sem.concurrent_6_thread.ServerThread


abstract class AbstractTest[V](semFactory: ISemanticsFactory[V], nondeterm: Boolean = true) extends FunSuite {
  private[this] val _PRINT_SERVER = ServerImpl(Bag(Rule(Bag(Pattern('PRINT)), Par())))
  def PRINT_SERVER(t: Type) = TApp(TAbs('V, TCast(_PRINT_SERVER, TSrv('PRINT -> ?(TVar('V))))), t)
  val PRINT_SERVER_NO = PRINT_SERVER(?()).eraseType
  val CONST_SERVER = ServerImpl(Bag(Rule(Bag(Pattern('CONST)), Par())))
  val CONST_SERVER_NO = CONST_SERVER.eraseType
  val sigmap = Substitution('Print, _PRINT_SERVER.eraseType)
  val sigmac = Substitution('Const, CONST_SERVER.eraseType)

  def PRINT(e: Exp) = PRINT_SERVER(?())~>'PRINT !! (e)

  def withPrintServer(p: Exp) = Def('Print, TSrv('PRINT -> TSvc()), _PRINT_SERVER, p)
  def withConstServer(p: Exp) = Def('Const, TSrv('CONST -> TSvc()), CONST_SERVER, p)

  def testInterp(s: String, p: TypedSyntax.Par, expected: AbstractSemantics.Res[Bag[TypedSyntax.Send]]): Unit =
    testInterpUntyped(s, p.eraseType, expected map (_.map(_.eraseType)))

  def testInterpUntyped(s: String, p: Syntax.Par, expected: AbstractSemantics.Res[Bag[Syntax.Send]]): Unit = {
    test(s ++ "-interp") {
      val sem = semFactory.newInstance()
      val res = sem.interp(p)
      val norm = res map (sem.normalizeVal(_))
      val normExpected = expected map (bag => bag.map(s => sigmap(sigmac(s)).asInstanceOf[Syntax.Send]))
      println(s"ServerThread instances: ${ServerThread.instanceCounter}")
      if (nondeterm)
        assert(norm == normExpected) //, s"Was $norm, expected $expected")
      else {
        assert(!norm.isEmpty, s"No result found, expected one of $normExpected")
        assert(norm.size == 1, s"Too many results found $norm, expected one of $normExpected")
        assert(normExpected.contains(norm.head), s"Was $norm, expected one of $normExpected")
      }
    }
  }

  def testType(s: String, p: Exp, expected: Type, gamma: Context=Map(), boundTv: Set[Symbol]=Set()) = {
    test(s ++ "-type") {
      val tpe = typeCheck(gamma, boundTv, p)
      assert(tpe == expected)
    }
  }
}