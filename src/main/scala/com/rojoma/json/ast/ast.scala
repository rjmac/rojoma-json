package com.rojoma.json
package ast

import scala.{collection => sc}

sealed abstract class JsonInvalidValue(msg: String) extends RuntimeException(msg) {
  def value: Any
}
case class JsonInvalidFloat(value: Float) extends JsonInvalidValue("Attempted to place a NaN or infinite value into a JSON AST.")
case class JsonInvalidDouble(value: Double) extends JsonInvalidValue("Attempted to place a NaN or infinite value into a JSON AST.")

/** A JSON datum.  This can be safely downcast to a more-specific type
  * using the `cast` method which is implicitly added to this class
  * in the companion object.*/
sealed trait JValue {
  override def toString = io.PrettyJsonWriter.toString(this)
}

object JValue {
  /** Safe downcast with a fairly nice syntax.
    * This will statically prevent attempting to cast anywhere except
    * a subclass of the value's static type.
    *
    * It can be used for navigation in a JSON tree:
    * {{{
    *   for {
    *     JObject(foo) <- raw.cast[JObject]
    *     JArray(elems) <- foo.get("foo").flatMap(_.cast[JArray])
    *   } yield {
    *     ...something with elems...
    *   } getOrElse(throw "couldn't find interesting elements")
    * }}}
    */
  implicit def toCastable[T <: JValue](x: T) = new com.rojoma.`json-impl`.DownCaster(x)
}

/** A JSON "atom" — anything except arrays or objects.  This and [[com.rojoma.json.ast.JCompound]] form
  * a partition of the set of valid [[com.rojoma.json.ast.JValue]]s. */
sealed abstract class JAtom extends JValue

/** A number. */
case class JNumber(number: math.BigDecimal) extends JAtom {
  def toByte = toBigDecimal.toByte
  def toByteExact = toBigDecimal.toByteExact
  def toShort = toBigDecimal.toShort
  def toShortExact = toBigDecimal.toShortExact
  def toInt = toBigDecimal.toInt
  def toIntExact = toBigDecimal.toIntExact
  def toLong = toBigDecimal.toLong
  def toLongExact = toBigDecimal.toLong
  def toBigInt = toBigDecimal.toBigInt
  def toBigIntExact = toBigDecimal.toBigIntExact

  def toDouble = toBigDecimal.toDouble
  def toFloat = toBigDecimal.toFloat

  def toBigDecimal = number
}

object JNumber {
  import math._

  def apply(b: Byte): JNumber = new JNumber(BigDecimal(b, java.math.MathContext.UNLIMITED))
  def apply(s: Short): JNumber = new JNumber(BigDecimal(s, java.math.MathContext.UNLIMITED))
  def apply(i: Int): JNumber = new JNumber(BigDecimal(i, java.math.MathContext.UNLIMITED))
  def apply(l: Long): JNumber = new JNumber(BigDecimal(l, java.math.MathContext.UNLIMITED))
  def apply(bi: BigInt): JNumber = new JNumber(BigDecimal(bi, java.math.MathContext.UNLIMITED))

  def apply(f: Float): JNumber = {
    if(f.isNaN || f.isInfinite) throw JsonInvalidFloat(f)
    new JNumber(BigDecimal(f))
  }
  def apply(d: Double): JNumber = {
    if(d.isNaN || d.isInfinite) throw JsonInvalidDouble(d)
    new JNumber(BigDecimal(d))
  }
}

/** A JSON string.  This does not yet enforce well-formedness with
  * respect to surrogate pairs, but it probably should. */
case class JString(string: String) extends JAtom

/** A boolean */
case class JBoolean(boolean: Boolean) extends JAtom

/** Null. */
sealed abstract class JNull extends JAtom // so the object has a nameable type
case object JNull extends JNull

/** The common superclass of arrays and objects.  This and [[com.rojoma.json.ast.JAtom]] form
  * a partition of the set of valid [[com.rojoma.json.ast.JValue]]s. */
sealed trait JCompound extends JValue

/** A JSON array, implemented as a thin wrapper around a sequence of [[com.rojoma.json.ast.JValue]]s.
  * In many ways this can be treated as a `Seq`, but it is in fact not one. */
case class JArray(override val toSeq: sc.Seq[JValue]) extends Iterable[JValue] with PartialFunction[Int, JValue] with JCompound {
  override def size = toSeq.size
  def length = size
  override def toIndexedSeq[B >: JValue] = toSeq.toIndexedSeq[B]
  override def toList = toSeq.toList
  override def toStream = toSeq.toStream
  override def toArray[B >: JValue : ClassManifest] = toSeq.toArray[B]

  def apply(idx: Int) = toSeq(idx)
  def isDefinedAt(idx: Int) = toSeq.isDefinedAt(idx)
  def iterator = toSeq.iterator
}

/** A JSON object, implemented as a thin wrapper around a map from `String` to [[com.rojoma.json.ast.JValue]].
  * In many ways this can be treated as a `Map`, but it is in fact not one. */
case class JObject(val fields: sc.Map[String, JValue]) extends Iterable[(String, JValue)] with PartialFunction[String, JValue] with JCompound {
  override def size = fields.size
  def contains(s: String) = fields.contains(s)
  def apply(key: String) = fields(key)
  def get(key: String) = fields.get(key)
  def getOrElse[B1 >: JValue](key: String, default: => B1): B1 = fields.getOrElse(key, default)
  def isDefinedAt(key: String) = fields.isDefinedAt(key)
  def iterator = fields.iterator
  def keys = fields.keys
  def keysIterator = fields.keysIterator
  def keySet = fields.keySet
  def values = fields.values
  def valuesIterator = fields.valuesIterator
  def mapValues[C](f: JValue => C): sc.Map[String, C] = fields.mapValues(f)
  override def toSeq = fields.toSeq
  override def toMap[T, U] (implicit ev: <:<[(String, JValue), (T, U)]): Map[T, U] = fields.toMap
}
