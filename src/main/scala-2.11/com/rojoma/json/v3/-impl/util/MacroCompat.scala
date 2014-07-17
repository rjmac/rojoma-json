package com.rojoma.json.v3
package `-impl`.util

// 2.11 version of MacroCompat

trait MacroCompat {
  import MacroCompat._

  val c: Context
  import c.universe._

  def toTermName(s: String) = TermName(s)

  def findValue[T](ann: Annotation): Option[Any] =
    ann.tree.children.tail.collect {
      case AssignOrNamedArg(Ident(n), Literal(Constant(v))) if n.toString == "value" => v
    }.headOption
}

object MacroCompat {
  type Context = scala.reflect.macros.whitebox.Context
}
