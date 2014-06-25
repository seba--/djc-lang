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
  val s1 =
    ServerImpl(Rule(
      Bag(Pattern('m1, 'x)),
      Send(ServiceRef(Var('Print), 'foo), Var('x))))

  val p1 =
    Let('Print, Spawn(PRINT_SERVER_NO),
      Let('Const, Spawn(CONST_SERVER_NO),
        Let('s1, Spawn(s1),
          Send(ServiceRef(Var('s1), 'm1), ServiceRef(Var('Const), 'bar)))))

  testInterpUntyped("p1",
    Par(p1),
    Set(Bag(Send(ServiceRef(Spawn(PRINT_SERVER_NO), 'foo), ServiceRef(Spawn(CONST_SERVER_NO), 'bar)))))


  // send message to self
  val s2 = ServerImpl(
    Rule(
      Bag(Pattern('m1, 'x)),
      Send(ServiceRef(Var('Print), 'foo), Var('x))),
    Rule(
      Bag(Pattern('m2, 'x)),
      Send(ServiceRef(Var('this),'m1), Var('x)))
    )

  val p2 =
    Let('Print, Spawn(PRINT_SERVER_NO),
      Let('Const, Spawn(CONST_SERVER_NO),
        Let('s2, Spawn(s2),
          Send(ServiceRef(Var('s2), 'm2), ServiceRef(Var('Const), 'bar)))))

  testInterpUntyped("p2",
    Par(p2),
    Set(Bag(Send(ServiceRef(Spawn(PRINT_SERVER_NO), 'foo), ServiceRef(Spawn(CONST_SERVER_NO), 'bar)))
    )
  )

  // send message to other server
  val s3 = ServerImpl(
    Rule(
      Bag(Pattern('m2, 'x)),
      Send(ServiceRef(Var('s1), 'm1), Var('x)))
    )

  val p3 =
    Let('Print, Spawn(PRINT_SERVER_NO),
      Let('Const, Spawn(CONST_SERVER_NO),
        Let('s1, Spawn(s1),
          Let('s3, Spawn(s3),
            Send(ServiceRef(Var('s3), 'm2), ServiceRef(Var('Const), 'bar))))))

  testInterpUntyped("p3",
    Par(p3),
    Set(Bag(Send(ServiceRef(Spawn(PRINT_SERVER_NO), 'foo), ServiceRef(Spawn(CONST_SERVER_NO), 'bar))),
        Bag(Send(ServiceRef(Spawn(PRINT_SERVER_NO), 'foo), ServiceRef(Spawn(CONST_SERVER_NO), 'bar)))
    ))


  // join pattern
  val s4 = ServerImpl(
    Rule(
      Bag(Pattern('m1, 'x), Pattern('m2, 'y)),
      Send(ServiceRef(Var('Print), 'foo), Var('x), Var('y))))

  val p4 =
    Let('Print, Spawn(PRINT_SERVER_NO),
      Let('Const, Spawn(CONST_SERVER_NO),
        Let('s4, Spawn(s4),
          Par(
            Send(ServiceRef(Var('s4), 'm1), ServiceRef(Var('Const), 'bar)),
            Send(ServiceRef(Var('s4), 'm2), ServiceRef(Var('Const), 'baz))))))

  testInterpUntyped("p4",
    Par(p4),
    Set(Bag(Send(ServiceRef(Spawn(PRINT_SERVER_NO), 'foo), List(ServiceRef(Spawn(CONST_SERVER_NO), 'bar), ServiceRef(Spawn(CONST_SERVER_NO), 'baz))))
  ))

  // nondeterminism
  val s5 = ServerImpl(
    Rule(
      Bag(Pattern('m1, 'x), Pattern('token)),
      Send(ServiceRef(Var('Print), 'foo), Var('x))))

  val p5 =
    Let('Print, Spawn(PRINT_SERVER_NO),
      Let('Const, Spawn(CONST_SERVER_NO),
        Let('s5, Spawn(s5),
          Par(
            Send(ServiceRef(Var('s5), 'token)),
            Send(ServiceRef(Var('s5), 'm1), ServiceRef(Var('Const), 'bar)),
            Send(ServiceRef(Var('s5), 'm1), ServiceRef(Var('Const), 'baz))))))

  testInterpUntyped("p5",
    Par(p5),
    Set(
      Bag(Send(ServiceRef(Spawn(PRINT_SERVER_NO), 'foo), List(ServiceRef(Spawn(CONST_SERVER_NO), 'bar))), Send(ServiceRef(Spawn(s5), 'm1), List(ServiceRef(Spawn(CONST_SERVER_NO), 'baz)))),
      Bag(Send(ServiceRef(Spawn(PRINT_SERVER_NO), 'foo), List(ServiceRef(Spawn(CONST_SERVER_NO), 'baz))), Send(ServiceRef(Spawn(s5), 'm1), List(ServiceRef(Spawn(CONST_SERVER_NO), 'bar))))
  ))



  // server-variable shadowing
  val s6 = ServerImpl(
    Rule(
      Bag(Pattern('m1, 'x)),
      Send(ServiceRef(Var('Print), 'foo6), Var('x))))

  val p6 =
    Let('Print, Spawn(PRINT_SERVER_NO),
      Let('Const, Spawn(CONST_SERVER_NO),
        Let('s6, Spawn(s1),
          Let('s6, Spawn(s6),
            Send(ServiceRef(Var('s6), 'm1), ServiceRef(Var('Const), 'bar))))))

  testInterpUntyped("p6",
    Par(p6),
    Set(Bag(Send(ServiceRef(Spawn(PRINT_SERVER_NO), 'foo6), ServiceRef(Spawn(CONST_SERVER_NO), 'bar)))))

  // higher-order service: s7.m2(s1.m1, "bar") -> s1.m1("bar") -> Print.foo("bar")
  val s7 = ServerImpl(
    Rule(
      Bag(Pattern('m2, 'f, 'x)),
      Send(Var('f), Var('x)))
  )

  val p7 =
    Let('Print, Spawn(PRINT_SERVER_NO),
      Let('Const, Spawn(CONST_SERVER_NO),
        Let('s1, Spawn(s1),
          Let('s7, Spawn(s7),
            Send(ServiceRef(Var('s7), 'm2), ServiceRef(Var('s1), 'm1), ServiceRef(Var('Const), 'bar))))))

  testInterpUntyped("p7",
    Par(p7),
    Set(Bag(Send(ServiceRef(Spawn(PRINT_SERVER_NO), 'foo), ServiceRef(Spawn(CONST_SERVER_NO), 'bar)))
  ))

  //flattening of Par
  val s8 = ServerImpl(
    Rule(Bag(Pattern('a)), Send(ServiceRef(Var('Print), 'foo))),
    Rule(Bag(Pattern('a),Pattern('b)), Send(ServiceRef(Var('Print), 'bar))))
  val p8 =
    Let('Print, Spawn(PRINT_SERVER_NO),
      Let('Const, Spawn(CONST_SERVER_NO),
        Let('srv, Spawn(s8),
          Par(Send(ServiceRef(Var('srv),'b)),  Par(Send(ServiceRef(Var('srv),'a)))))))

  testInterpUntyped("p8",
    Par(p8),
    Set(Bag(Send(ServiceRef(Spawn(s8),'b)), Send(ServiceRef(Spawn(PRINT_SERVER_NO), 'foo))),
        Bag(Send(ServiceRef(Spawn(PRINT_SERVER_NO), 'bar)))))
}
