package djc.lang.typ.inference

import djc.lang.TypedSyntax._
import djc.lang.typ._
import djc.lang.typ.Types._

object TypeInference {
  import Checker.Context
  import Checker.TVarContext

  import ProtoTypes._
  import Constraints._

  def infer(proto: Type, gamma: Context, tgamma: TVarContext, exp: Exp): Type = ???

  case class InferenceException(msg: String) extends RuntimeException(msg)
}
