package com.rojoma.json.v3
package `-impl`.util

// 2.10 version of MacroCompat

trait MacroCompat {
  import MacroCompat._

  val c: Context
  import c.universe._

  def toTermName(s: String) = newTermName(s)
  def toTypeName(s: String) = newTypeName(s)

  def posOf(param: Symbol, ann: Annotation) = param.pos

  implicit class EnhContext(underlying: Context) {
    def freshName() = underlying.fresh()
  }

  implicit class EnhMethodSymbolApi(ms: MethodSymbol) {
    def paramLists = ms.paramss
  }

  implicit class EnhAnnotation(t: Annotation) {
    def tree = t
  }

  def findValue[T](ann: Annotation): Option[Any] = {
    ann.javaArgs.get(newTermName("value")) match {
      case Some(LiteralArgument(Constant(v))) => Some(v)
      case Some(ArrayArgument(others)) => Some(others.collect { case LiteralArgument(Constant(v : String)) => v }.toArray)
      case _ => None
    }
  }

  object termNames {
    val ROOTPKG = "_root_"
  }
}

object MacroCompat {
  type Context = scala.reflect.macros.Context
}
