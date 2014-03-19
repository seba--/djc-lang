package djc.lang

import org.scalatest.FunSuite
import util.Bag


class TestSemantics_InterpSubst extends FunSuite {

  def testInterp(s: String, p: Prog, expected: Semantics_InterpSubst.Res[Semantics_InterpSubst.Val]): Unit =
    test(s) {
      val res = Semantics_InterpSubst.interp(p)
      assert (res == expected, s"Was $res, expected $expected")
    }


  // single message send
  val s1 = ServerImpl(
    List(Rule(
      List(Pattern('m1, List('x))),
      Send(ServiceRef(ServerVar('Print), 'foo), List(ServiceVar('x))))))

  val p1 = Def('s1, s1, Send(ServiceRef(ServerVar('s1), 'm1), List(ServiceVar('bar))))

  testInterp("p1",
    p1,
    Set(Bag(Send(ServiceRef(ServerVar('Print), 'foo), List(ServiceVar('bar))))))


  // send message to 'this
  val s2 = ServerImpl(List(
    Rule(
      List(Pattern('m1, List('x))),
      Send(ServiceRef(ServerVar('Print), 'foo), List(ServiceVar('x)))),
    Rule(
      List(Pattern('m2, List('x))),
      Send(ServiceRef(ServerVar('this), 'm1), List(ServiceVar('x))))
    ))

  val p2 = Def('s2, s2, Send(ServiceRef(ServerVar('s2), 'm2), List(ServiceVar('bar))))

  testInterp("p2",
    p2,
    Set(Bag(Send(ServiceRef(ServerVar('Print), 'foo), List(ServiceVar('bar))))))


  // send message to other server
  val s3 = ServerImpl(List(
    Rule(
      List(Pattern('m2, List('x))),
      Send(ServiceRef(ServerVar('s1), 'm1), List(ServiceVar('x))))
    ))

  val p3 = Def('s1, s1, Def('s3, s3, Send(ServiceRef(ServerVar('s3), 'm2), List(ServiceVar('bar)))))

  testInterp("p3",
    p3,
    Set(Bag(Send(ServiceRef(ServerVar('Print), 'foo), List(ServiceVar('bar))))))


  // join pattern
  val s4 = ServerImpl(
    List(Rule(
      List(Pattern('m1, List('x)), Pattern('m2, List('y))),
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
    List(Rule(
      List(Pattern('m1, List('x)), Pattern('token, List())),
      Send(ServiceRef(ServerVar('Print), 'foo), List(ServiceVar('x))))))

  val p5 = Def('s5, s5, Par(Bag(
    Send(ServiceRef(ServerVar('s5), 'token), List()),
    Send(ServiceRef(ServerVar('s5), 'm1), List(ServiceVar('bar))),
    Send(ServiceRef(ServerVar('s5), 'm1), List(ServiceVar('baz))))
  ))

  testInterp("p5",
    p5,
    Set(
      Bag(Send(ServiceRef(ServerVar('Print), 'foo), List(ServiceVar('bar))), Send(ServiceRef(s5, 'm1), List(ServiceVar('baz)))),
      Bag(Send(ServiceRef(ServerVar('Print), 'foo), List(ServiceVar('baz))), Send(ServiceRef(s5, 'm1), List(ServiceVar('bar))))))

}
