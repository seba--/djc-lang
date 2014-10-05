package djc.lang.sem.typ

import djc.lang.{Syntax, AbstractTest}
import djc.lang.sem._
import djc.lang.TypedLanguage._
import djc.lang.TypedLanguage.types._
import djc.lang.TypedSyntaxDerived._
import djc.lang.base.Integer._
import djc.lang.base.IntegerCompare._
import util.Bag

class TestDesugarings1 extends TestDesugarings(nondeterm_1_subst.Semantics)
class TestDesugarings2 extends TestDesugarings(nondeterm_2_env.Semantics)
class TestDesugarings3 extends TestDesugarings(nondeterm_3_routed.SemanticsFactory)
class TestDesugarings4 extends TestDesugarings(nondeterm_4_grouped.SemanticsFactory)
class TestDesugarings5 extends TestDesugarings(nondeterm_5_parallel.SemanticsFactory)
class TestDesugarings6 extends TestDesugarings(concurrent_6_thread.SemanticsFactory)

class TestDesugarings[V](sem: ISemanticsFactory[V]) extends AbstractTest(sem)  {

  val TTestSrv = TSrv(TSrvRep('echo -> ?(TInteger), 'test -> ?()))
  def testSrv(testBody: Exp) = {
    Let('print, TSrv(TSrvRep('PRINT -> ?(TInteger))), Spawn(TApp(PRINT_SERVER, TInteger))) {
      Server(Rule(Bag(Pattern('echo, 'n -> TInteger)),
        'print~>'PRINT !! 'n),
        Rule(Bag(Pattern('test)), testBody)) ~> 'test !!()
    }
  }

  val testLetSingle = testSrv(Let(TTestSrv)('x, TInteger, 1) {
    'this ~> 'echo !! 'x
  })
  testInterp("let single", Par(testLetSingle),  Set(Bag(PRINT(1))))
  testType("let single type", testLetSingle, Unit)

  val testLetNested = testSrv(Let(TTestSrv)('x, TInteger, 1) {
    Par( Let('y, TInteger, 2) {
           'this~>'echo!!'y
         },
         Let('z, TInteger, 3) {
           Let('x, TInteger, 4) {
             Par('this~>'echo!!'z, 'this~>'echo!!'x)
           }
         },
         'this~>'echo!!'x
    )
  })

  val testLetNestedRes: AbstractSemantics.Res[Bag[Send]] = Set(Bag((for(i <- 1 to 4) yield PRINT(i)):_*))
  testInterp("let nested", Par(testLetNested), testLetNestedRes)
  testType("let nested type", testLetNested, Unit)

  val plus = Lambda(List('x -> TInteger, 'y -> TInteger), BaseCall(djc.lang.base.Integer.Plus, Var('x), Var('y)), TInteger)
  def testSrvWithPlus(testBody: Exp) =
    Let('plus, TFun(TInteger, TInteger, TInteger), plus) {
      testSrv(testBody)
    }

  val testLetkSingle = testSrvWithPlus(
      Letk(TTestSrv)('x, TInteger, 'plus!!(1,2)) {
        'this~>'echo!!'x
      })
  testInterp("letk single", Par(testLetkSingle), Set(Bag(PRINT(3))))
  testType("letk single type", testLetkSingle, Unit)

  val testLetkNested = testSrvWithPlus(Letk(TTestSrv)('x, TInteger, 'plus!!(0,1)) {
    Par( Letk('y, TInteger, 'plus!!(1,'x)) {
      'this~>'echo!!'y
    },
      Letk('z, TInteger, 'plus!!(2,'x)) {
        Letk('x, TInteger, 'plus!!('x,'z)) {
          Par('this~>'echo!!'z, 'this~>'echo!!'x)
        }
      },
      'this~>'echo!!'x
    )
  })

  val testLetkNestedRes:  AbstractSemantics.Res[Bag[Send]] = Set(Bag((for(i <- 1 to 4) yield PRINT(i)):_*))
  testInterp("letk nested", Par(testLetkNested), testLetkNestedRes)
  testType("letk nested type", testLetkNested, Unit)

  val testIfc = testSrv(
    Ifc(TTestSrv)(mkIntLit(0) <== 1) {
       'this~>'echo!!(1)
    } Else {
      'this~>'echo!!(0)
    }
  )
  val testIfc2 = testSrv(
    Ifc(TTestSrv)(mkIntLit(1) <== 0) {
      'this~>'echo!!(1)
    } Else {
      'this~>'echo!!(0)
    }
  )
  testInterp("Ifc simple then", Par(testIfc), Set(Bag(PRINT(1))))
  testInterp("Ifc simple else", Par(testIfc2), Set(Bag(PRINT(0))))
  testType("Ifc simple type", testIfc, Unit)

  def testIfcNested(i: Int) = testSrv(
    Ifc(TTestSrv)(mkIntLit(i) <== 4) {
      Ifc(mkIntLit(i) <== 2) {
        'this~>'echo!!(1)
      } Else {
        'this~>'echo!!(3)
      }
    } Else {
      Ifc(mkIntLit(i) <== 5) {
        'this~>'echo!!(5)
      } Else {
        'this~>'echo!!(6)
      }
    }
  )

  for(i <- List(1, 3, 5, 6))
    testInterp(s"Ifc nested $i", Par(testIfcNested(i)), Set(Bag(PRINT(i))))
  testType("Ifc nested type", testIfcNested(1), Unit)

  def testMixed(i: Int) = testSrvWithPlus(
    Let(TTestSrv)('x, TInteger, i) {
      Par(
        Letk('y, TInteger, 'plus!!('x, 1)) {
          Ifc(Var('y) === 2) {
            'this~>'echo!!(1)
          } Else {
            'this~>'echo!!(2)
          }
        },

        Letk('z, TInteger, 'plus!!('x,'x)) {
          Ifc(Var('z) === 2) {
            'this~>'echo!!(3)
          } Else {
            'this~>'echo!!(4)
          }
        }
      )
    }
  )

  for(i <- List(1,2))
    testInterp(s"Mixed $i", Par(testMixed(i)), Set(Bag(PRINT(i), PRINT(i+2))))
  testType("Mixed type", testMixed(1), Unit)
}
