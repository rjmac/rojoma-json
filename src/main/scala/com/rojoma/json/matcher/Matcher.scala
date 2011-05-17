package com.rojoma.json
package matcher

import ast._

sealed trait Pattern {
  def matches(x: JValue) = Pattern.matches(x, this)
}
object Pattern {
  implicit def litify(x: JValue): Pattern = Literal(x)
  implicit def litify(x: Long): Pattern = Literal(JNumber(x))
  implicit def litify(x: Double): Pattern = Literal(JNumber(x))
  implicit def litify(x: String): Pattern = Literal(JString(x))
  implicit def litify(x: Boolean): Pattern = Literal(JBoolean(x))

  private [matcher] def matches(x: JValue, pattern: Pattern): Boolean = pattern match {
    case Literal(atom: JAtom) =>
      x == atom
    case Literal(pat: JArray) => // matches if x is a sequence and pat is a prefix of that sequence
      x.cast[JArray] map { lit =>
        pat.length <= lit.length && pat.zip(lit).forall { case (a,b) => a == b }
      } getOrElse(false)
    case Literal(pat: JObject) => // matches if x is an object and pat is a subset of that object
      x.cast[JObject] map { lit =>
        pat.forall { case (k, v) => lit.get(k) == Some(v) }
      } getOrElse(false)
    case v@Variable() =>
      v.maybeFill(x)
    case VArray(subPatterns @ _*) =>
      x.cast[JArray] map { arr =>
        subPatterns.length <= arr.length && subPatterns.zip(arr).forall { case (a,b) => matches(b, a) }
      } getOrElse(false)
    case VObject(subPatterns @ _*) =>
      x.cast[JObject] map { obj =>
        subPatterns forall {
          case (k, v) =>
            obj.contains(k) && matches(obj(k), v)
        }
      } getOrElse(false)
  }
}
case class Literal(underlying: JValue) extends Pattern
case class Variable[T <: JValue : ClassManifest]() extends Pattern {
  var result: T = _
  private [matcher] def maybeFill(x: JValue): Boolean = {
    x.cast[T] match {
      case None =>
        false
      case Some(r) =>
        result = r
        true
    }
  }
}
case class VArray(subPatterns: Pattern*) extends Pattern
case class VObject(subPatterns: (String, Pattern)*) extends Pattern
