package com.rojoma
package json.v2
package ast

import scala.language.implicitConversions

import scala.{collection => sc}
import scala.reflect.ClassTag

sealed abstract class JsonInvalidValue(msg: String) extends RuntimeException(msg) {
  def value: Any
}
case class JsonInvalidFloat(value: Float) extends JsonInvalidValue("Attempted to place a NaN or infinite value into a JSON AST.")
case class JsonInvalidDouble(value: Double) extends JsonInvalidValue("Attempted to place a NaN or infinite value into a JSON AST.")

sealed trait JValueType

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
  def dynamic = json.v2.dynamic.DynamicJValue(this)

  def jvalueType: JValueType
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

/** A JSON "atom" — anything except arrays or objects.  This and [[com.rojoma.json.v2.ast.JCompound]] form
  * a partition of the set of valid [[com.rojoma.json.v2.ast.JValue]]s. */
sealed abstract class JAtom extends JValue {
  def forced: this.type = this
}

/** A number. */
sealed abstract class JNumber extends JAtom {
  def toByte: Byte
  def toShort: Short
  def toInt: Int
  def toLong: Long
  def toBigInt: BigInt
  def toFloat: Float
  def toDouble: Double
  def toBigDecimal: BigDecimal

  def jvalueType = JNumber

  override def equals(o: Any) = o match {
    case that: JNumber => this.toBigDecimal == that.toBigDecimal
    case _ => false
  }

  override final lazy val hashCode = toBigDecimal.hashCode
}

private[ast] final class JIntNumber(val toInt: Int) extends JNumber {
  def toByte: Byte = toInt.toByte
  def toShort: Short = toInt.toShort
  def toLong: Long = toInt.toLong
  def toFloat: Float = toInt.toFloat
  def toDouble: Double = toInt.toDouble
  lazy val toBigInt: BigInt = BigInt(toInt)
  lazy val toBigDecimal = BigDecimal(toInt, java.math.MathContext.UNLIMITED)

  override def toString = toInt.toString
  override def equals(o: Any) = o match {
    case that: JIntNumber => this.toInt == that.toInt
    case that: JLongNumber => this.toLong == that.toLong
    case that: JBigIntNumber => this.toBigInt == that.toBigInt
    case _ => super.equals(o)
  }
}

private[ast] final class JLongNumber(val toLong: Long) extends JNumber {
  def toByte: Byte = toLong.toByte
  def toShort: Short = toLong.toShort
  def toInt: Int = toLong.toInt
  def toFloat: Float = toLong.toFloat
  def toDouble: Double = toLong.toDouble
  lazy val toBigInt: BigInt = BigInt(toLong)
  lazy val toBigDecimal = BigDecimal(toLong, java.math.MathContext.UNLIMITED)

  override def toString = toLong.toString
  override def equals(o: Any) = o match {
    case that: JLongNumber => this.toLong == that.toLong
    case that: JIntNumber => this.toLong == that.toLong
    case that: JBigIntNumber => this.toBigInt == that.toBigInt
    case _ => super.equals(o)
  }
}

private[ast] final class JFloatNumber(val toFloat: Float) extends JNumber {
  require(!toFloat.isNaN, "A JNumber cannot be a NaN")
  require(!toFloat.isInfinite, "A JNumber cannot be infinite")

  def toByte: Byte = toFloat.toByte
  def toShort: Short = toFloat.toShort
  def toInt: Int = toFloat.toInt
  def toLong: Long = toFloat.toLong
  def toDouble: Double = toFloat.toDouble
  def toBigInt: BigInt = toBigDecimal.toBigInt
  lazy val toBigDecimal = BigDecimal(toDouble, java.math.MathContext.UNLIMITED)

  override def toString = toFloat.toString
}

private[ast] final class JDoubleNumber(val toDouble: Double) extends JNumber {
  require(!toDouble.isNaN, "A JNumber cannot be a NaN")
  require(!toDouble.isInfinite, "A JNumber cannot be infinite")

  def toByte: Byte = toDouble.toByte
  def toShort: Short = toDouble.toShort
  def toInt: Int = toDouble.toInt
  def toLong: Long = toDouble.toLong
  def toFloat: Float = toDouble.toFloat
  def toBigInt: BigInt = toBigDecimal.toBigInt
  lazy val toBigDecimal = BigDecimal(toDouble, java.math.MathContext.UNLIMITED)

  override def toString = toDouble.toString
}

private[ast] final class JBigIntNumber(val toBigInt: BigInt) extends JNumber {
  def toByte: Byte = toBigInt.toByte
  def toShort: Short = toBigInt.toShort
  def toInt: Int = toBigInt.toInt
  def toLong: Long = toBigInt.toLong
  def toFloat: Float = toBigInt.toFloat
  def toDouble: Double = toBigInt.toDouble
  lazy val toBigDecimal = BigDecimal(toDouble, java.math.MathContext.UNLIMITED)

  override def toString = toBigInt.toString
  override def equals(o: Any) = o match {
    case that: JBigIntNumber => this.toBigInt == that.toBigInt
    case that: JIntNumber => this.toBigInt == that.toBigInt
    case that: JLongNumber => this.toBigInt == that.toBigInt
    case _ => super.equals(o)
  }
}

private[ast] final class JBigDecimalNumber(val toBigDecimal: BigDecimal) extends JNumber {
  def toByte: Byte = toBigDecimal.toByte
  def toShort: Short = toBigDecimal.toShort
  def toInt: Int = toBigDecimal.toInt
  def toLong: Long = toBigDecimal.toLong
  def toFloat: Float = toBigDecimal.toFloat
  def toDouble: Double = toBigDecimal.toDouble
  val toBigInt: BigInt = toBigDecimal.toBigInt
}

private[ast] final class JUncheckedStringNumber(override val toString: String) extends JNumber {
  def toByte: Byte = try { toString.toByte} catch { case _: NumberFormatException => toBigDecimal.toByte }
  def toShort: Short = try { toString.toShort } catch { case _: NumberFormatException => toBigDecimal.toShort }
  def toInt: Int = try { toString.toInt } catch { case _: NumberFormatException => toBigDecimal.toInt }
  def toLong: Long = try { toString.toLong } catch { case _: NumberFormatException => toBigDecimal.toLong }
  def toFloat: Float = try { toString.toFloat } catch { case _: NumberFormatException => toBigDecimal.toFloat }
  def toDouble: Double = try { toString.toDouble } catch { case _: NumberFormatException => toBigDecimal.toDouble }
  def toBigInt = try { BigInt(toString) } catch { case _: NumberFormatException => toBigDecimal.toBigInt }
  lazy val toBigDecimal = BigDecimal(toDouble, java.math.MathContext.UNLIMITED)
}

object JNumber extends JValueType {
  def apply(b: Byte): JNumber = new JIntNumber(b)
  def apply(s: Short): JNumber = new JIntNumber(s)
  def apply(i: Int): JNumber = new JIntNumber(i)
  def apply(l: Long): JNumber = new JLongNumber(l)
  def apply(f: Float): JNumber = new JFloatNumber(f)
  def apply(d: Double): JNumber = new JDoubleNumber(d)
  def apply(bi: BigInt): JNumber = new JBigIntNumber(bi)
  def apply(bd: BigDecimal): JNumber = new JBigDecimalNumber(bd)

  def unsafeFromString(s: String): JNumber = new JUncheckedStringNumber(s)

  override final def toString = "JNumber"
}

/** A JSON string.  This does not yet enforce well-formedness with
  * respect to surrogate pairs, but it probably should. */
case class JString(string: String) extends JAtom {
  def jvalueType = JString
}

object JString extends scala.runtime.AbstractFunction1[String, JString] with JValueType {
  override final def toString = "JBoolean"
}

/** A boolean */
case class JBoolean(boolean: Boolean) extends JAtom {
  def jvalueType = JBoolean
}

object JBoolean extends scala.runtime.AbstractFunction1[Boolean, JBoolean] with JValueType {
  // wish I could override apply(Boolean) to use these.  At least
  // JsonReader will, though.
  val canonicalTrue = JBoolean(true)
  val canonicalFalse = JBoolean(false)

  override final def toString = "JBoolean"
}

/** Null. */
sealed abstract class JNull extends JAtom { // so the object has a nameable type
  def jvalueType = JNull
}
case object JNull extends JNull with JValueType

/** The common superclass of arrays and objects.  This and [[com.rojoma.json.v2.ast.JAtom]] form
  * a partition of the set of valid [[com.rojoma.json.v2.ast.JValue]]s. */
sealed trait JCompound extends JValue {
  def forced: JCompound
}

/** A JSON array, implemented as a thin wrapper around a sequence of [[com.rojoma.json.v2.ast.JValue]]s.
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

  def jvalueType = JArray
}

object JArray extends scala.runtime.AbstractFunction1[sc.Seq[JValue], JArray] with JValueType {
  val canonicalEmpty = JArray(Vector.empty) // Vector because JsonReader is guaranteed to return JArrays which contain Vectors.
  override final def toString = "JArray"
}

/** A JSON object, implemented as a thin wrapper around a map from `String` to [[com.rojoma.v2.json.ast.JValue]].
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

  def jvalueType = JObject
}

object JObject extends scala.runtime.AbstractFunction1[sc.Map[String, JValue], JObject] with JValueType {
  val canonicalEmpty = JObject(Map.empty) // _Not_ LinkedHashMap because all JsonReader guarantees is ordering of elements, which this satisfies.
  override final def toString = "JObject"
}
