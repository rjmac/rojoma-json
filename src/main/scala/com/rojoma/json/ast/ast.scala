package com.rojoma
package json
package ast

import scala.language.implicitConversions

import scala.{collection => sc}
import scala.reflect.ClassTag

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

  /** Forces this [[com.rojoma.json.ast.JValue]] to be fully evaluated.  In particular, the
    * compound [[com.rojoma.json.codec.JsonCodec]]s will produce
    * views of their inputs instead of fully-evaluated [[com.rojoma.json.ast.JValue]]s.  This
    * can be problematic if the underlying structure can be mutated
    * before this object is used, or if this object is passed to
    * another thread.
    *
    * What is or is not copied is not defined; the only postcondition is that
    * there are no lazy values left in the returned tree.
    * 
    * @return An equal [[com.rojoma.json.ast.JValue]] with strict values. */
  def forced: JValue

  /** On scala 2.9 with -Xexperimental enabled, produces a dynamically
    * typed view of this `JValue` which can be descended using dot-notation
    * for field names or apply-type syntax for arrays.  It can be turned back
    * into a `JValue` with the `static` method.  On scala 2.8, calling this is
    * not an error but using it to descend into fields is a compile-time error.
    *
    * Note that certain field-names (the names common to all objects plus `static`,
    * `apply`, and `applyDynamic` cannot be accessed with simple field-notation.
    * Instead, pass them as strings to the `apply` method. */
  def dynamic = json.dynamic.DynamicJValue(this)
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
sealed abstract class JAtom extends JValue {
  def forced: this.type = this
}

/** A number. */
case class JNumber(number: BigDecimal) extends JAtom {
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
  def apply(b: Byte): JNumber = new JNumber(BigDecimal(b, java.math.MathContext.UNLIMITED))
  def apply(s: Short): JNumber = new JNumber(BigDecimal(s, java.math.MathContext.UNLIMITED))
  def apply(i: Int): JNumber = new JNumber(BigDecimal(i, java.math.MathContext.UNLIMITED))
  def apply(l: Long): JNumber = new JNumber(BigDecimal(l, java.math.MathContext.UNLIMITED))
  def apply(bi: BigInt): JNumber = new JNumber(BigDecimal(bi, java.math.MathContext.UNLIMITED))

  def apply(f: Float): JNumber = {
    if(f.isNaN || f.isInfinite) throw JsonInvalidFloat(f)
    new JNumber(BigDecimal(f.toDouble))
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

object JBoolean extends scala.runtime.AbstractFunction1[Boolean, JBoolean] {
  // wish I could override apply(Boolean) to use these.  At least
  // JsonReader will, though.
  val canonicalTrue = JBoolean(true)
  val canonicalFalse = JBoolean(false)

  override final def toString = "JBoolean"
}

/** Null. */
sealed abstract class JNull extends JAtom // so the object has a nameable type
case object JNull extends JNull

/** The common superclass of arrays and objects.  This and [[com.rojoma.json.ast.JAtom]] form
  * a partition of the set of valid [[com.rojoma.json.ast.JValue]]s. */
sealed trait JCompound extends JValue {
  def forced: JCompound
}

/** A JSON array, implemented as a thin wrapper around a sequence of [[com.rojoma.json.ast.JValue]]s.
  * In many ways this can be treated as a `Seq`, but it is in fact not one. */
case class JArray(elems: sc.Seq[JValue]) extends Iterable[JValue] with PartialFunction[Int, JValue] with JCompound {
  import com.rojoma.`json-impl`.AnnoyingJArrayHack._

  override def size = elems.size
  def length = elems.length
  override def toList = elems.toList
  override def toStream = elems.toStream
  override def toArray[B >: JValue : ClassTag] = elems.toArray[B]

  def apply(idx: Int) = elems(idx)
  def isDefinedAt(idx: Int) = elems.isDefinedAt(idx)
  def iterator = elems.iterator

  override def toSeq = elems
  override def toIndexedSeq = elems.toIndexedSeq

  def forced: JArray = {
    // not just "toSeq.map(_forced)" because the seq might be a Stream or view
    val forcedArray: Vector[JValue] = elems.map(_.forced)(sc.breakOut)
    new JArray(forcedArray) {
      override def forced = this
    }
  }

  override def equals(o: Any): Boolean = {
    o match {
      case that: JArray =>
        this.elems == that.elems
      case _ =>
        false
    }
  }
}

object JArray extends scala.runtime.AbstractFunction1[sc.Seq[JValue], JArray] {
  val canonicalEmpty = JArray(Vector.empty) // Vector because JsonReader is guaranteed to return JArrays which contain Vectors.
  override final def toString = "JArray"
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

  def forced: JObject = {
    // would be nice to freeze this into an actual immutable Map in
    // order to preserve ordering and yet be actually unchangable, but
    // instead we'll just trust that the Bad People who are relying on
    // the ordering of fields in their JSON objects are not *so* bad
    // that they'll downcast an sc.Map to an scm.Map.  Unchangability
    // of the result isn't part of "forced"'s contract anyway; merely
    // full evaluation.
    val forcedMap = new sc.mutable.LinkedHashMap[String, JValue]
    for((k, v) <- fields) forcedMap += k -> v.forced
    new JObject(forcedMap) {
      override def forced = this
    }
  }
}

object JObject extends scala.runtime.AbstractFunction1[sc.Map[String, JValue], JObject] {
  val canonicalEmpty = JObject(Map.empty) // _Not_ LinkedHashMap because all JsonReader guarantees is ordering of elements, which this satisfies.
  override final def toString = "JObject"
}
