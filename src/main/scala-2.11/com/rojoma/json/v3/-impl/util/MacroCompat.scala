package com.rojoma.json.v3
package `-impl`.util

// 2.11 version of MacroCompat

trait MacroCompat {
  import MacroCompat._

  val c: Context
  import c.universe._

  def toTermName(s: String) = TermName(s)
  def toTypeName(s: String) = TypeName(s)

  // ann.tree.pos doesn't point at the annotation, oddly, so we'll point at the parameter instead
  def posOf(param: Symbol, ann: Annotation) = param.pos // ann.tree.pos

  def isDefined(pos: Position) = pos != NoPosition

  def findValue[T](ann: Annotation): Option[Any] =
    ann.tree.children.tail.collect {
      case AssignOrNamedArg(Ident(n), Literal(Constant(v))) if n.toString == "value" => v
    }.headOption

  lazy val syntheticFlag = Flag.SYNTHETIC
}

object MacroCompat {
  type Context = scala.reflect.macros.whitebox.Context
  type compileTimeOnly = scala.annotation.compileTimeOnly
}
