package djc.lang

import org.scalatest.FunSuite
import djc.lang.sem.{ISemanticsFactory, Substitution, AbstractSemantics}
import djc.lang.TypedSyntax._
import djc.lang.TypedSyntaxDerived._
import util.Bag

import djc.lang.typ.Checker._
import djc.lang.typ.Types._
import djc.lang.sem.concurrent_6_thread.ServerThread


abstract class AbstractTest[V](semFactory: ISemanticsFactory[V]) extends FunSuite {
  private[this] val _PRINT_SERVER = ServerImpl(Rule(Bag(Pattern('PRINT)), Par()))
  val PRINT_SERVER = TAbs('V, UnsafeCast(_PRINT_SERVER, TSrvRep('PRINT -> ?(TVar('V)))))
  val PRINT_SERVER_NO = PRINT_SERVER.eraseType
  val CONST_SERVER = TAbs('V, UnsafeCast(ServerImpl(Rule(Bag(Pattern('CONST)), Par())), TSrvRep('CONST -> ?(TVar('V)))))
  val CONST_SERVER_NO = CONST_SERVER.eraseType
  val sigmap = Substitution('Print, Spawn(_PRINT_SERVER).eraseType)
  val sigmac = Substitution('Const, Spawn(CONST_SERVER).eraseType)

  def PRINT(e: Exp) = Spawn(PRINT_SERVER(?()))~>'PRINT !! (e)
  def PRINT(t: Type, e: Exp) = Spawn(PRINT_SERVER(t))~>'PRINT !! (e)
  def PRINT_NO(e: Syntax.Exp) = Syntax.Send(Syntax.ServiceRef(Syntax.Spawn(PRINT_SERVER_NO), 'PRINT), e)

  def testInterp(s: String, p: TypedSyntax.Par, expected: Bag[Syntax.Send] => Boolean): Unit =
    testInterp(s, p.eraseType, expected)

  def testInterp(s: String, p: Syntax.Par, expected: Bag[Syntax.Send] => Boolean): Unit =
    test(s ++ "-interp") {
      val sem = semFactory.newInstance()
      val res = sem.resToSet[V](sem.interp(p))
      val norm = res map (sem.normalizeVal(_))
//      val normExpected = expected map (bag => bag.map(s => sigmap(sigmac(s)).asInstanceOf[Syntax.Send]))
      norm map (bag => assert(expected(bag)))
    }
//    testInterpUntyped(s, p.eraseType, expected map (_.map(_.eraseType)), ignore)

  def testInterp(s: String, p: TypedSyntax.Par, expected: AbstractSemantics.Res[Bag[TypedSyntax.Send]],
                 ignore: Syntax.Send => Boolean = (s => false)): Unit =
    testInterpUntyped(s, p.eraseType, expected map (_.map(_.eraseType)), ignore)

  def testInterpUntyped(s: String, p: Syntax.Par, expected: AbstractSemantics.Res[Bag[Syntax.Send]],
                        ignore: Syntax.Send => Boolean = (s => false)): Unit = {
    test(s ++ "-interp") {
      val sem = semFactory.newInstance()
      val res = sem.resToSet[V](sem.interp(p))
      val norm = res map (sem.normalizeVal(_))
      val normFiltered = norm map (ss => ss filter (!ignore(_)))
      val normExpected = expected map (bag => bag.map(s => sigmap(sigmac(s)).asInstanceOf[Syntax.Send]))
      if (sem.isFullyNondeterministic)
        assert(normFiltered == normExpected) //, s"Was $norm, expected $expected")
      else {
        for (result <- normFiltered)
          assert(normExpected.contains(result), s"Was $normFiltered, expected one of $normExpected")
      }
    }
  }

  def testType(s: String, p: Exp, expected: Type, gamma: Context=Map(), tgamma: TVarContext=Map()) = {
    test(s ++ "-type") {
      try {
        val tpe = typeCheck(gamma, tgamma, p)
        assert(subtype(tgamma)(tpe,expected), s"subtype is wrong: $tpe </< $expected")
      } catch {
        case TypeCheckException(msg) => assert(false, msg)
      }
    }
  }
}