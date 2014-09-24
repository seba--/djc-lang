package util.vis

import djc.lang.TypedSyntaxFamily
import djc.lang.typ.TypeFamily

import scala.collection.immutable.ListMap
import scalafx.beans.property.{ObjectProperty, StringProperty}
import scalafx.collections.ObservableBuffer


sealed trait Model

/**
 * Model of case classes which should appear as nodes in the tree view of the inspector,
 * e.g. expressions and types.
 *
 * @param name constructor name of the case class
 * @param fields map of field names to their Model representation for display in the table view
 * @param rep the object that this node represents
 */
case class ModelNode(var name: String, children: List[ModelNode], fields: ListMap[String, Any], rep: Any = ()) extends Model {
  val fieldSeq = ObservableBuffer(fields.toSeq)
  val repString = StringProperty(rep.toString)
}
object ModelNode {
  def apply(name: String, rep: Any): ModelNode = ModelNode(name, Nil, ListMap(), rep)
  def apply(rep: Any): ModelNode = ModelNode(rep.toString, Nil, ListMap(), rep)
}



//TODO make something more sophisticated based on reflection
object Model {
  def apply[TF <: TypeFamily](tpe: TF#Type): ModelNode = {
    import tpe.family._
    tpe match {
      case tv@TVar(alpha) =>
        ModelNode(s"${tv.productPrefix}(${alpha.name})", Nil, ListMap("alpha" -> alpha), tv)
      case Bot =>
        ModelNode(Bot.productPrefix, Nil,  ListMap(), Bot)
      case Top =>
        ModelNode(Top.productPrefix, Nil, ListMap(), Top)
      case Unit =>
        ModelNode(Unit.productPrefix, Nil, ListMap(), Unit)
      case tb@TBase(name, ts) =>
        ModelNode(s"${tb.productPrefix}($name)", ts.map(Model(_)), ListMap("name" -> name), tb)
      case tsrv@TSrv(rep) =>
        ModelNode(tsrv.productPrefix, List(Model(rep)), ListMap(), tsrv)
      case rep@TSrvRep(svcs) =>
        val svcsmodel = svcs.toList.map {
          case (sv, t) =>
            val ModelNode(name, children, fields, rep) = Model(t)
             ModelNode(s"${sv.name}: $name", children, fields, rep)
        }
        ModelNode(rep.productPrefix, svcsmodel, ListMap(), rep)
      case tsvc@TSvc(params) =>
        ModelNode(tsvc.productPrefix, params.map(Model(_)), ListMap(), tsvc)
      case tuniv@TUniv(alpha, bound, tpe) =>
        ModelNode(s"${tuniv.productPrefix} ${alpha.name}", List(Model(bound), Model(tpe)),
                                                  ListMap("alpha" -> alpha), tuniv)
      case x: Any =>
        apply(x)
    }
  }

  def apply[SF <: TypedSyntaxFamily](exp: SF#Exp): ModelNode = {
    import exp.family._
    exp match {
      case par@Par(ps) =>
        ModelNode(par.productPrefix, ps.toList.map(Model(_)), ListMap(), par)
      case send@Send(rcv, args) =>
        val rvcm = Model(rcv)
        rvcm.name = "rcv: " + rvcm.name
        val argsm = args.map(Model(_)).zip(0 until args.length).map { case (m, n) => m.name = s"arg$n: ${m.name}"; m }
        ModelNode(send.productPrefix, rvcm :: argsm, ListMap(), send)
      case v@Var(x) =>
        ModelNode(s"${v.productPrefix}(${x.name})", Nil, ListMap("x" -> x), v)
      case ref@ServiceRef(srv, x) =>
        ModelNode(s"ServiceRef#${x.name}", List(Model(srv)), ListMap("x" -> x), ref)
      case srv@ServerImpl(rules) =>  //TODO format rules
        ModelNode(srv.productPrefix, rules.map(Model(_)), ListMap(srv.signature.svcs.map {case (k,v) => (k.toString(), v)}.toSeq:_*), srv)
      case spwn@Spawn(local, e) =>
        ModelNode(spwn.productPrefix, List(Model(e)), ListMap("local" -> local), spwn)
      case tapp@TApp(p, t) =>
        ModelNode(tapp.productPrefix, List(Model(p), Model(t)), ListMap(), tapp)
      case tabs@TAbs(alpha, bound, p) =>
        ModelNode(s"TAbs(${alpha.name})", List(Model(bound), Model(p)), ListMap("alpha" -> alpha), tabs)
      case uns@UnsafeCast(e, t) =>
        ModelNode(uns.productPrefix, List(Model(e), Model(t)), ListMap(), uns)
      case up@UpCast(e, t) =>
        ModelNode(up.productPrefix, List(Model(e), Model(t)), ListMap(), up)
      case bc@BaseCall(b, ts, es) => //TODO format baseops
        ModelNode(bc.productPrefix, Model(b) :: ts.map(Model(_)) ::: es.map(Model(_)), ListMap(), bc)
      case x: Any => apply(x)
    }
  }

  def apply(x: Any): ModelNode = x match {
    case m: Map[_,_] =>
      ModelNode("Map", Nil, ListMap("#" -> m), m)
    case l: List[_] =>
      ModelNode("List", Nil, ListMap("#" -> l), l)
    case p: Product =>
      ModelNode(p.productPrefix, p.productIterator.map(Model(_)).toList, ListMap(), p)
    case _ => ModelNode("???", Nil, ListMap("#" -> x), x)
  }
}
