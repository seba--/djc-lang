package util.vis

import djc.lang.TypedSyntaxFamily
import djc.lang.typ.TypeFamily
import djc.lang.typ.inference.ExtSyntaxFamily
import org.kiama.output.PrettyPrinter
import scala.collection.immutable.Seq
import util.Bag

object HTMLPrettyPrint extends PrettyPrinter {
  override val defaultWidth = 100
  override val defaultIndent = 2
  def apply(a: Any): Doc = any(a)
  override def any(a: Any): Doc = {
    if (a == null)
      span("null", "hljs-keyword")
    else a match {
      case b: Bag[_] => list (b.toList, "Bag", any)
      case s: String => span(super.any(s), "hljs-string")
      case s: Symbol => span(super.any(s), "hljs-string")
      case b: Boolean => span(super.any(b), "hljs-keyword")
      case Nil => span(super.any(Nil), "hljs-type")
      case e:ExtSyntaxFamily#ExtExp => extexp(e)
      case r:ExtSyntaxFamily#URule => urule(r)
      case p:ExtSyntaxFamily#UPattern => upattern(p)
      case t:TypeFamily#Type => tpe(t)
      case e:TypedSyntaxFamily#Exp => exp(e)
      case r:TypedSyntaxFamily#Rule => rule(r)
      case p:TypedSyntaxFamily#Pattern => pattern(p)
      case b:TypedSyntaxFamily#BaseOp => baseop(b)
      case p: Product if p.productArity == 0 =>
        span(p.productPrefix, "hljs-type")
      case _ => super.any(a)
    }
  }

  def extexp[XF <: ExtSyntaxFamily](e: XF#ExtExp): Doc = {
    import e.family._
    e match {
      case UTApp(e) => seq(List(e), "UTApp", any)
      case UServerImpl(rules) =>
        seq(rules, "UServerImpl", urule)
      case _ => super.any(e)
    }
  }

  def exp[TSF <: TypedSyntaxFamily](e: TSF#Exp): Doc = {
    import e.family._
    e match {
      case Par(ps) => seq(ps.toList, "Par", exp)
      case Send(rcv, args) =>
        seq(rcv :: args, "Send", exp)
      case Var(x) =>
        span("Var", "hljs-type") <> parens(any(x))
      case ServiceRef(srv, x) =>
        seq(List(srv, x), "ServiceRef", any)
      case ServerImpl(rules) =>
        seq(rules, "ServerImpl", rule)
      case Spawn(local, e) =>
        seq(List(local, e), "Spawn", any)
      case TApp(p, t) =>
        seq(List(p,t), "TApp", any)
      case TAbs(alpha, bound, p) =>
        seq(List(alpha, bound, p), "TAbs", any)
        val bnd = subtype(any(alpha), tpe(bound))
        span(span("TAbs", "fold", "hljs-type") <> parens(htmlChildSpan(group(nest(lsep(List(bnd, exp(p)) , comma))))), "node")
      case UnsafeCast(e, t) =>
        seq(List(e,t), "UnsafeCast")
      case UpCast(e, t) =>
        seq(List(e,t), "UpCast", any)
      case BaseCall(b, ts, es) =>
        seq(b :: ts ::: es, "BaseCall", any)
      case _ => super.any(e)
    }
  }

  def urule[XF <: ExtSyntaxFamily](r: XF#URule): Doc = {
    seq(r.ps.toList :+ r.p, "URule", any)
  }

  def rule[TSF <: TypedSyntaxFamily](r: TSF#Rule): Doc =
    seq(r.ps.toList :+ r.p, "Rule", any)

  def pattern[TSF <: TypedSyntaxFamily](p: TSF#Pattern): Doc = {
    val s: String = s"<span class='hljs-string'>${p.name}</span><span class='hljs-type'>?</span>"
    seq(p.params.toList, s, any)
  }

  def upattern[XF <: ExtSyntaxFamily](p: XF#UPattern): Doc = {
    val s: String = s"<span class='hljs-string'>${p.name}</span><span class='hljs-type'>?</span>"
    seq(p.params.toList, s, any)
  }

  def baseop[TSF <: TypedSyntaxFamily](b: TSF#BaseOp): Doc = {
    val base = htmlFoldSpan("BaseOp")
    val targs = if (b.targs.isEmpty)
      empty
    else
      brackets(lsep(b.targs.map {case  (s,t) => subtype(any(s), tpe(t))}, comma))
      span(base <> parens(htmlChildSpan(targs <\> parens(lsep (b.ts.map(tpe), comma)) <+> span("=&gt;", "hljs-type") <+> tpe(b.res))), "node")
  }

  def subtype(d1: Doc, d2: Doc): Doc = {
    d1 <+> "&lt;:" <+> d2
  }

  def tpe[TF <: TypeFamily](t: TF#Type): Doc = {
    import t.family._
    t match {
      case Top | Bot | Unit =>
        span(t.toString, "hljs-type type")
      case TSvc(params) =>
        seqcss("hljs-type", "type")(params, "TSvc", tpe)
      case TSrvRep(svcs) =>
        seqcss("hljs-type", "type")(svcs.toList, "TSrvRep", any)
      case TSrv(rep) =>
        seqcss("hljs-type", "type")(List(rep), "TSrv", tpe)
      case TVar(alpha) =>
        span("TVar", "hljs-type type") <> parens(any(alpha))
      case TBase(name, ts) =>
        seqcss("hljs-type", "type")(name :: ts, "TBase", any)
      case TUniv(alpha, bound, tp) =>
        val bnd = subtype(any(alpha), tpe(bound))
        span(span("TUniv", "fold", "hljs-type", "type") <> parens(htmlChildSpan(group(nest(lsep(List(bnd, tpe(tp)) , comma))))), "node")
      case _ => super.any(t)
    }
  }


  def seqcss[T](classes: String*)(l : Seq[T], prefix : String = "Seq",
                                  elemToDoc : T => Doc = (x : T) => value (x),
                                  sep : Doc = comma,
                                  sepfn : (Seq[Doc], Doc) => Doc = lsep) : Doc = {
    if (l.isEmpty)
      span(prefix, classes.mkString(" ")) <> parens (empty)
    else
      span(span(prefix, "fold " + classes.mkString(" ")) <> parens (htmlChildSpan(group (nest (sepfn (l map elemToDoc, sep))))), "node")
  }

  override def seq[T] (l : Seq[T], prefix : String = "Seq",
                       elemToDoc : T => Doc = (x : T) => value (x),
                       sep : Doc = comma,
                       sepfn : (Seq[Doc], Doc) => Doc = lsep) : Doc = {
      if (l.isEmpty)
        span(prefix, "hljs-type") <> parens (empty)
      else
        span(htmlFoldSpan(prefix) <> parens (htmlChildSpan(group (nest (sepfn (l map elemToDoc, sep))))), "node")
  }

  def htmlFoldSpan(d: Doc): Doc = span(d, "hljs-type", "fold")

  def htmlChildSpan(d: Doc): Doc = span(d, "child")

  def span(d: Doc, classes: String*): Doc = {
    enclose(text(s"<span${if (classes.nonEmpty) " class=\"" + classes.mkString(" ") + "\">"}"), d, text("</span>"))
  }
}
