package com.rojoma.json.v3
package `-impl`.util

// 2.10 version of MacroCompat

trait MacroCompat {
  import MacroCompat._

  val c: Context
  import c.universe._

  def toTermName(s: String) = newTermName(s)
  def toTypeName(s: String) = newTypeName(s)

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
      case _ => None
    }
  }


  implicit class EnhPositionApi(p: Position) {
    def offset =
      if(p.isRange) throw new UnsupportedOperationException("Position.point on class scala.reflect.internal.util.RangePosition")
      else p.startOrPoint
  }


  object termNames {
    val ROOTPKG = "_root_"
  }
}

object MacroCompat {
  type Context = scala.reflect.macros.Context
}
