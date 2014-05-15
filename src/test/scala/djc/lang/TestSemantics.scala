package djc.lang

import org.scalatest.FunSuite
import util.Bag
import FlatSyntax._

import djc.lang.sem.AbstractSemantics
import djc.lang.sem.nondeterm_1_subst
import djc.lang.sem.FlatSubstitution.Subst

/*import djc.lang.sem.nondeterm_2_env
import djc.lang.sem.nondeterm_3_routed
import djc.lang.sem.nondeterm_4_grouped
import djc.lang.sem.nondeterm_5_parallel
import djc.lang.sem.concurrent_6_thread
import djc.lang.sem.concurrent_6_thread.Semantics*/


class TestSemantics_nondeterm_subst extends TestSemantics(nondeterm_1_subst.Semantics)
/*class TestSemantics_nondeterm_env extends TestSemantics(nondeterm_2_env.Semantics)
class TestSemantics_nondeterm_routed extends TestSemantics(nondeterm_3_routed.Semantics)
class TestSemantics_nondeterm_grouped extends TestSemantics(nondeterm_4_grouped.Semantics)
class TestSemantics_nondeterm_parallel extends TestSemantics(nondeterm_5_parallel.Semantics)
class TestSemantics_concurrent_thread extends TestSemantics(concurrent_6_thread.Semantics, false)*/

abstract class TestSemantics[V](sem: AbstractSemantics[V], nondeterm: Boolean = true) extends FunSuite {
  val PRINT_SERVER = ServerImpl(Bag(Rule(Bag(Pattern('PRINT)), Par())))
  val CONST_SERVER = ServerImpl(Bag(Rule(Bag(Pattern('CONST)), Par())))
  val sigmap = Subst('Print, PRINT_SERVER)
  val sigmac = Subst('Const, CONST_SERVER)

  def withPrintServer(p: Prog) = Def('Print, PRINT_SERVER, p)
  def withConstServer(p: Prog) = Def('Const, CONST_SERVER, p)

  def testInterp(s: String, p: Prog, expected: sem.Res[Bag[Send]]): Unit =
    test(s) {
//      if (p == p5) {
        val res = sem.interp(withPrintServer(withConstServer(p)))
        val norm = res map (sem.normalizeVal(_))
        if (nondeterm)
          assert(norm == expected, s"Was $norm, expected $expected")
        else {
          assert(!norm.isEmpty, s"No result found, expected one of $expected")
          assert(norm.size == 1, s"Too many results found $norm, expected one of $expected")
          assert(expected.contains(norm.head), s"Was $norm, expected one of $expected")
        }
//      }
    }


  // single message send
  val s1 = ServerImpl(
    Bag(Rule(
      Bag(Pattern('m1, 'x)),
      Send(ServiceRef(Var('Print), 'foo), Var('x)))))

  val s1norm = sigmap(s1)

  val p1 = Def('s1, s1, Send(ServiceRef(Var('s1), 'm1), ServiceRef(CONST_SERVER, 'bar)))

  testInterp("p1",
    p1,
    Set(Bag(Send(ServiceRef(PRINT_SERVER, 'foo), ServiceRef(CONST_SERVER, 'bar))),
        Bag(Send(ServiceRef(s1norm, 'm1), ServiceRef(CONST_SERVER, 'bar)))))


  // send message to 'this
  val s2 = ServerImpl(Bag(
    Rule(
      Bag(Pattern('m1, 'x)),
      Send(ServiceRef(Var('Print), 'foo), Var('x))),
    Rule(
      Bag(Pattern('m2, 'x)),
      Send(ServiceRef(Var('this), 'm1), Var('x)))
    ))

  val s2norm = sigmap(s2)

  val p2 = Def('s2, s2, Send(ServiceRef(Var('s2), 'm2), ServiceRef(CONST_SERVER, 'bar)))

  testInterp("p2",
    p2,
    Set(Bag(Send(ServiceRef(PRINT_SERVER, 'foo), ServiceRef(CONST_SERVER, 'bar))),
        Bag(Send(ServiceRef(s2norm, 'm2), ServiceRef(CONST_SERVER, 'bar))),
        Bag(Send(ServiceRef(s2norm, 'm1), ServiceRef(CONST_SERVER, 'bar)))
    )
  )


  // send message to other server
  val s3 = ServerImpl(Bag(
    Rule(
      Bag(Pattern('m2, 'x)),
      Send(ServiceRef(Var('s1), 'm1), Var('x)))
    ))

  val s3norm = Subst('s1, s1norm)(s3)

  val p3 = Def('s1, s1, Def('s3, s3, Send(ServiceRef(Var('s3), 'm2), ServiceRef(CONST_SERVER, 'bar))))

  testInterp("p3",
    p3,
    Set(Bag(Send(ServiceRef(PRINT_SERVER, 'foo), ServiceRef(CONST_SERVER, 'bar))),
        Bag(Send(ServiceRef(s3norm, 'm2), ServiceRef(CONST_SERVER, 'bar))),
        Bag(Send(ServiceRef(s1norm, 'm1), ServiceRef(CONST_SERVER, 'bar))),
        Bag(Send(ServiceRef(PRINT_SERVER, 'foo), ServiceRef(CONST_SERVER, 'bar)))
    ))


  // join pattern
  val s4 = ServerImpl(
    Rule(
      Bag(Pattern('m1, 'x), Pattern('m2, 'y)),
      Send(ServiceRef(Var('Print), 'foo), Var('x), Var('y))))

  val s4norm = sigmap(s4)

  val p4 = Def('s4, s4, Par(
    Send(ServiceRef(Var('s4), 'm1), ServiceRef(CONST_SERVER, 'bar)),
    Send(ServiceRef(Var('s4), 'm2), ServiceRef(CONST_SERVER, 'baz))
  ))

  testInterp("p4",
    p4,
    Set(Bag(Send(ServiceRef(PRINT_SERVER, 'foo), List(ServiceRef(CONST_SERVER, 'bar), ServiceRef(CONST_SERVER, 'baz)))),
        Bag(Send(ServiceRef(s4norm, 'm1), ServiceRef(CONST_SERVER, 'bar)),  Send(ServiceRef(s4norm, 'm2), ServiceRef(CONST_SERVER, 'baz)))
    ))


  // nondeterminism
  val s5 = ServerImpl(
    Bag(Rule(
      Bag(Pattern('m1, 'x), Pattern('token)),
      Send(ServiceRef(Var('Print), 'foo), Var('x)))))

  val s5norm = sigmap(s5)

  val p5 = Def('s5, s5, Par(
    Send(ServiceRef(Var('s5), 'token)),
    Send(ServiceRef(Var('s5), 'm1), ServiceRef(CONST_SERVER, 'bar)),
    Send(ServiceRef(Var('s5), 'm1), ServiceRef(CONST_SERVER, 'baz))
  ))

  testInterp("p5",
    p5,
    Set(
      Bag(Send(ServiceRef(PRINT_SERVER, 'foo), List(ServiceRef(CONST_SERVER, 'bar))), Send(ServiceRef(s5norm, 'm1), List(ServiceRef(CONST_SERVER, 'baz)))),
      Bag(Send(ServiceRef(PRINT_SERVER, 'foo), List(ServiceRef(CONST_SERVER, 'baz))), Send(ServiceRef(s5norm, 'm1), List(ServiceRef(CONST_SERVER, 'bar)))),
      Bag(Send(ServiceRef(s5norm, 'token)),
        Send(ServiceRef(s5norm, 'm1), ServiceRef(CONST_SERVER, 'bar)),
        Send(ServiceRef(s5norm, 'm1), ServiceRef(CONST_SERVER, 'baz))))
  )
  

  // server-variable shadowing
  val s6 = ServerImpl(
    Bag(Rule(
      Bag(Pattern('m1, 'x)),
      Send(ServiceRef(Var('Print), 'foo6), Var('x)))))

  val s6norm = sigmap(s6)

  val p6 = Def('s6, s1, Def('s6, s6, Send(ServiceRef(Var('s6), 'm1), ServiceRef(CONST_SERVER, 'bar))))

  testInterp("p6",
    p6,
    Set(Bag(Send(ServiceRef(PRINT_SERVER, 'foo6), ServiceRef(CONST_SERVER, 'bar))),
        Bag(Send(ServiceRef(s6norm, 'm1), ServiceRef(CONST_SERVER, 'bar)))))

  // higher-order service: s7.m2(s1.m1, "bar") -> s1.m1("bar") -> Print.foo("bar")
  val s7 = ServerImpl(Bag(
    Rule(
      Bag(Pattern('m2, 'f, 'x)),
      Send(Var('f), Var('x)))
  ))

  val p7 = Def('s1, s1, Def('s7, s7, Send(ServiceRef(Var('s7), 'm2), ServiceRef(Var('s1), 'm1), ServiceRef(CONST_SERVER, 'bar))))

  testInterp("p7",
    p7,
    Set(Bag(Send(ServiceRef(PRINT_SERVER, 'foo), ServiceRef(CONST_SERVER, 'bar))),
        Bag(Send(ServiceRef(s7, 'm2), ServiceRef(s1norm, 'm1), ServiceRef(CONST_SERVER, 'bar))),
        Bag(Send(ServiceRef(s1norm, 'm1), ServiceRef(CONST_SERVER, 'bar)))
       ))

  //flattening of Par
  val s8 = ServerImpl(Rule(Bag(Pattern('a)), Send(ServiceRef(PRINT_SERVER, 'foo))),
    Rule(Bag(Pattern('a),Pattern('b)), Send(ServiceRef(PRINT_SERVER, 'bar))))
  val p8 = Def('srv, s8,
                     Par(Send(ServiceRef(Var('srv),'b)),  Par(Send(ServiceRef(Var('srv),'a))))  )

  testInterp("p8", p8, Set(Bag(Send(ServiceRef(s8,'b)), Send(ServiceRef(PRINT_SERVER, 'foo))),
                           Bag(Send(ServiceRef(PRINT_SERVER, 'bar))),
                           Bag(Send(ServiceRef(s8,'b)), Send(ServiceRef(s8,'a))  )))
}
