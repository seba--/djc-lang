package djc.lang

import org.scalatest.FunSuite
import util.Bag
import Syntax._
import SyntaxDerived._

import djc.lang.sem._
import djc.lang.sem.Substitution


class TestSemantics_nondeterm_subst extends TestSemantics(nondeterm_1_subst.Semantics)
class TestSemantics_nondeterm_env extends TestSemantics(nondeterm_2_env.Semantics)
class TestSemantics_nondeterm_routed extends TestSemantics(nondeterm_3_routed.SemanticsFactory)
class TestSemantics_nondeterm_grouped extends TestSemantics(nondeterm_4_grouped.SemanticsFactory)
class TestSemantics_nondeterm_parallel extends TestSemantics(nondeterm_5_parallel.SemanticsFactory)
class TestSemantics_concurrent_thread extends TestSemantics(concurrent_6_thread.SemanticsFactory)

abstract class TestSemantics[V](sem: ISemanticsFactory[V]) extends AbstractTest(sem) {
  // single message send
  val s1 = ServerImpl(
    Bag(Rule(
      Bag(Pattern('m1, 'x)),
      Send(ServiceRef(PRINT_SERVER_NO, 'foo), Var('x)))))

  val p1 = Def('s1, s1, Send(ServiceRef(Var('s1), 'm1), ServiceRef(CONST_SERVER_NO, 'bar)))

  Set(
    Bag(
      Send(
        ServiceRef(PRINT_SERVER_NO,'foo),
        List(ServiceRef(ServerImpl(Bag(Rule(Bag(Pattern('CONST,List())),Par()))),'bar)))),
    Bag(
      Send(
        ServiceRef(s1,'m1),
        List(ServiceRef(ServerImpl(Bag(Rule(Bag(Pattern('CONST,List())),Par()))),'bar)))),
    Bag(
      Send(ServiceRef(ServerImpl(Bag(Rule(Bag(Pattern('def,List('s1))),Send(ServiceRef(Var('s1),'m1),List(ServiceRef(ServerImpl(Bag(Rule(Bag(Pattern('CONST,List())),Par()))),'bar)))))),'def),List(ServerImpl(Bag(Rule(Bag(Pattern('m1,List('x))),Send(ServiceRef(ServerImpl(Bag(Rule(Bag(Pattern('PRINT,List())),Par()))),'foo),List(Var('x))))))))))

  testInterpUntyped("p1",
    Par(p1),
    Set(Bag(Send(ServiceRef(PRINT_SERVER_NO, 'foo), ServiceRef(CONST_SERVER_NO, 'bar)))))


  // send message to self
  val s2 = ServerImpl(Bag(
    Rule(
      Bag(Pattern('m1, 'x)),
      Send(ServiceRef(PRINT_SERVER_NO, 'foo), Var('x))),
    Rule(
      Bag(Pattern('m2, 'x)),
      Send(ServiceRef(Var('this),'m1), Var('x)))
    ))

  val p2 = Def('s2, s2, Send(ServiceRef(Var('s2), 'm2), ServiceRef(CONST_SERVER_NO, 'bar)))

  testInterpUntyped("p2",
    p2,
    Set(Bag(Send(ServiceRef(PRINT_SERVER_NO, 'foo), ServiceRef(CONST_SERVER_NO, 'bar)))
    )
  )

  // send message to other server
  val s3 = ServerImpl(Bag(
    Rule(
      Bag(Pattern('m2, 'x)),
      Send(ServiceRef(Var('s1), 'm1), Var('x)))
    ))

  val p3 = Def('s1, s1, Def('s3, s3, Send(ServiceRef(Var('s3), 'm2), ServiceRef(CONST_SERVER_NO, 'bar))))

  testInterpUntyped("p3",
    p3,
    Set(Bag(Send(ServiceRef(PRINT_SERVER_NO, 'foo), ServiceRef(CONST_SERVER_NO, 'bar))),
        Bag(Send(ServiceRef(PRINT_SERVER_NO, 'foo), ServiceRef(CONST_SERVER_NO, 'bar)))
    ))


  // join pattern
  val s4 = ServerImpl(
    Rule(
      Bag(Pattern('m1, 'x), Pattern('m2, 'y)),
      Send(ServiceRef(PRINT_SERVER_NO, 'foo), Var('x), Var('y))))

  val p4 = Def('s4, s4, Par(
    Send(ServiceRef(Var('s4), 'm1), ServiceRef(CONST_SERVER_NO, 'bar)),
    Send(ServiceRef(Var('s4), 'm2), ServiceRef(CONST_SERVER_NO, 'baz))
  ))

  testInterpUntyped("p4",
    p4,
    Set(Bag(Send(ServiceRef(PRINT_SERVER_NO, 'foo), List(ServiceRef(CONST_SERVER_NO, 'bar), ServiceRef(CONST_SERVER_NO, 'baz))))
  ))


  Set(
    Bag(
      Send(
        ServiceRef(
          ServerImpl(Bag(Rule(Bag(Pattern('m1,List('x)), Pattern('m2,List('y))),Send(ServiceRef(ServerImpl(Bag(Rule(Bag(Pattern('PRINT,List())),Par()))),'foo),List(Var('x), Var('y)))))),
          'm2),
        List(ServiceRef(ServerImpl(Bag(Rule(Bag(Pattern('CONST,List())),Par()))),'baz))),
      Send(
        ServiceRef(
          ServerImpl(Bag(Rule(Bag(Pattern('m1,List('x)), Pattern('m2,List('y))),Send(ServiceRef(ServerImpl(Bag(Rule(Bag(Pattern('PRINT,List())),Par()))),'foo),List(Var('x), Var('y)))))),
          'm1),
        List(ServiceRef(ServerImpl(Bag(Rule(Bag(Pattern('CONST,List())),Par()))),'bar)))),
    Bag(
      Send(
        ServiceRef(
          ServerImpl(Bag(Rule(Bag(Pattern('PRINT,List())),Par()))),
          'foo),
        List(ServiceRef(ServerImpl(Bag(Rule(Bag(Pattern('CONST,List())),Par()))),'bar), ServiceRef(ServerImpl(Bag(Rule(Bag(Pattern('CONST,List())),Par()))),'baz)))))

  // nondeterminism
  val s5 = ServerImpl(
    Bag(Rule(
      Bag(Pattern('m1, 'x), Pattern('token)),
      Send(ServiceRef(PRINT_SERVER_NO, 'foo), Var('x)))))

  val p5 = Def('s5, s5, Par(
    Send(ServiceRef(Var('s5), 'token)),
    Send(ServiceRef(Var('s5), 'm1), ServiceRef(CONST_SERVER_NO, 'bar)),
    Send(ServiceRef(Var('s5), 'm1), ServiceRef(CONST_SERVER_NO, 'baz))
  ))

  testInterpUntyped("p5",
    p5,
    Set(
      Bag(Send(ServiceRef(PRINT_SERVER_NO, 'foo), List(ServiceRef(CONST_SERVER_NO, 'bar))), Send(ServiceRef(s5, 'm1), List(ServiceRef(CONST_SERVER_NO, 'baz)))),
      Bag(Send(ServiceRef(PRINT_SERVER_NO, 'foo), List(ServiceRef(CONST_SERVER_NO, 'baz))), Send(ServiceRef(s5, 'm1), List(ServiceRef(CONST_SERVER_NO, 'bar))))
  ))


  // server-variable shadowing
  val s6 = ServerImpl(
    Bag(Rule(
      Bag(Pattern('m1, 'x)),
      Send(ServiceRef(PRINT_SERVER_NO, 'foo6), Var('x)))))

  val p6 = Def('s6, s1, Def('s6, s6, Send(ServiceRef(Var('s6), 'm1), ServiceRef(CONST_SERVER_NO, 'bar))))

  testInterpUntyped("p6",
    p6,
    Set(Bag(Send(ServiceRef(PRINT_SERVER_NO, 'foo6), ServiceRef(CONST_SERVER_NO, 'bar)))))

  // higher-order service: s7.m2(s1.m1, "bar") -> s1.m1("bar") -> Print.foo("bar")
  val s7 = ServerImpl(Bag(
    Rule(
      Bag(Pattern('m2, 'f, 'x)),
      Send(Var('f), Var('x)))
  ))

  val p7 = Def('s1, s1, Def('s7, s7, Send(ServiceRef(Var('s7), 'm2), ServiceRef(Var('s1), 'm1), ServiceRef(CONST_SERVER_NO, 'bar))))

  testInterpUntyped("p7",
    p7,
    Set(Bag(Send(ServiceRef(PRINT_SERVER_NO, 'foo), ServiceRef(CONST_SERVER_NO, 'bar)))
  ))

  //flattening of Par
  val s8 = ServerImpl(Rule(Bag(Pattern('a)), Send(ServiceRef(PRINT_SERVER_NO, 'foo))),
    Rule(Bag(Pattern('a),Pattern('b)), Send(ServiceRef(PRINT_SERVER_NO, 'bar))))
  val p8 = Def('srv, s8,
                     Par(Send(ServiceRef(Var('srv),'b)),  Par(Send(ServiceRef(Var('srv),'a)))))

  testInterpUntyped("p8", p8,
    Set(Bag(Send(ServiceRef(s8,'b)), Send(ServiceRef(PRINT_SERVER_NO, 'foo))),
        Bag(Send(ServiceRef(PRINT_SERVER_NO, 'bar)))))
}
