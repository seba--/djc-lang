package djc.lang

import org.scalatest.FunSuite
import util.Bag


class TestSemantics_SubstNondeterm extends TestSemantics(Semantics_SubstNondeterm)
class TestSemantics_EnvironmentNondeterm extends TestSemantics(Semantics_EnvironmentNondeterm)



abstract class TestSemantics(sem: AbstractSemantics) extends FunSuite {
  def testInterp(s: String, p: Prog, expected: sem.Res[Bag[Send]]): Unit =
    test(s) {
      val res = sem.interp(p)
      val norm = res map (sem.normalizeVal(_))
      assert (norm == expected, s"Was $norm, expected $expected")
    }


  // single message send
  val s1 = ServerImpl(
    Bag(Rule(
      Bag(Pattern('m1, List('x))),
      Send(ServiceRef(ServerVar('Print), 'foo), List(ServiceVar('x))))))

  val p1 = Def('s1, s1, Send(ServiceRef(ServerVar('s1), 'm1), List(ServiceVar('bar))))

  testInterp("p1",
    p1,
    Set(Bag(Send(ServiceRef(ServerVar('Print), 'foo), List(ServiceVar('bar))))))


  // send message to 'this
  val s2 = ServerImpl(Bag(
    Rule(
      Bag(Pattern('m1, List('x))),
      Send(ServiceRef(ServerVar('Print), 'foo), List(ServiceVar('x)))),
    Rule(
      Bag(Pattern('m2, List('x))),
      Send(ServiceRef(ServerVar('this), 'm1), List(ServiceVar('x))))
    ))

  val p2 = Def('s2, s2, Send(ServiceRef(ServerVar('s2), 'm2), List(ServiceVar('bar))))

  testInterp("p2",
    p2,
    Set(Bag(Send(ServiceRef(ServerVar('Print), 'foo), List(ServiceVar('bar))))))


  // send message to other server
  val s3 = ServerImpl(Bag(
    Rule(
      Bag(Pattern('m2, List('x))),
      Send(ServiceRef(ServerVar('s1), 'm1), List(ServiceVar('x))))
    ))

  val p3 = Def('s1, s1, Def('s3, s3, Send(ServiceRef(ServerVar('s3), 'm2), List(ServiceVar('bar)))))

  testInterp("p3",
    p3,
    Set(Bag(Send(ServiceRef(ServerVar('Print), 'foo), List(ServiceVar('bar))))))


  // join pattern
  val s4 = ServerImpl(
    Bag(Rule(
      Bag(Pattern('m1, List('x)), Pattern('m2, List('y))),
      Send(ServiceRef(ServerVar('Print), 'foo), List(ServiceVar('x), ServiceVar('y))))))

  val p4 = Def('s4, s4, Par(Bag(
    Send(ServiceRef(ServerVar('s4), 'm1), List(ServiceVar('bar))),
    Send(ServiceRef(ServerVar('s4), 'm2), List(ServiceVar('baz))))
  ))

  testInterp("p4",
    p4,
    Set(Bag(Send(ServiceRef(ServerVar('Print), 'foo), List(ServiceVar('bar), ServiceVar('baz))))))


  // nondeterminism
  val s5 = ServerImpl(
    Bag(Rule(
      Bag(Pattern('m1, List('x)), Pattern('token, List())),
      Send(ServiceRef(ServerVar('Print), 'foo), List(ServiceVar('x))))))

  val p5 = Def('s5, s5, Par(Bag(
    Send(ServiceRef(ServerVar('s5), 'token), List()),
    Send(ServiceRef(ServerVar('s5), 'm1), List(ServiceVar('bar))),
    Send(ServiceRef(ServerVar('s5), 'm1), List(ServiceVar('baz))))
  ))

  // server-variable shadowing
  testInterp("p5",
    p5,
    Set(
      Bag(Send(ServiceRef(ServerVar('Print), 'foo), List(ServiceVar('bar))), Send(ServiceRef(s5, 'm1), List(ServiceVar('baz)))),
      Bag(Send(ServiceRef(ServerVar('Print), 'foo), List(ServiceVar('baz))), Send(ServiceRef(s5, 'm1), List(ServiceVar('bar))))))

  val s6 = ServerImpl(
    Bag(Rule(
      Bag(Pattern('m1, List('x))),
      Send(ServiceRef(ServerVar('Print), 'foo6), List(ServiceVar('x))))))

  val p6 = Def('s6, s1, Def('s6, s6, Send(ServiceRef(ServerVar('s6), 'm1), List(ServiceVar('bar)))))

  testInterp("p6",
    p6,
    Set(Bag(Send(ServiceRef(ServerVar('Print), 'foo6), List(ServiceVar('bar))))))
}
