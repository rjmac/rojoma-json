package com.rojoma.json.v3
package ast

import scala.language.implicitConversions
import scala.{collection => sc}
import scala.collection.immutable.SeqMap
import scala.reflect.ClassTag
import java.math.{BigInteger, BigDecimal => JBigDecimal}
import java.io.Writer

import extensions.DoubleWriter

sealed abstract class JsonInvalidValue(msg: String) extends IllegalArgumentException(msg) {
  def value: Any
}
case class JsonInvalidFloat(value: Float) extends JsonInvalidValue("Attempted to place a NaN or infinite value into a JSON AST.")
case class JsonInvalidDouble(value: Double) extends JsonInvalidValue("Attempted to place a NaN or infinite value into a JSON AST.")

// Closed typeclass to find out what concrete types a subclass of JValue represents
sealed abstract class Json[T <: JValue] {
  val jsonTypes : Set[JsonType]
  override lazy val toString = jsonTypes.toSeq.map(_.toString).sorted.mkString(",")
}

sealed trait JsonType
object JsonType {
  import codec._
  implicit val jCodec: JsonEncode[JsonType] with JsonDecode[JsonType] = new JsonEncode[JsonType] with JsonDecode[JsonType] {
    def encode(j: JsonType) = JString(j.toString)
    def decode(x: JValue) = x match {
      case JString(JObject.toString) => Right(JObject)
      case JString(JArray.toString) => Right(JArray)
      case JString(JString.toString) => Right(JString)
      case JString(JNumber.toString) => Right(JNumber)
      case JString(JBoolean.toString) => Right(JBoolean)
      case JString(JNull.toString) => Right(JNull)
      case s: JString => Left(DecodeError.InvalidValue(s))
      case other => Left(DecodeError.InvalidType(other.jsonType, JString))
    }
  }
}

/** A JSON datum.  This can be safely downcast to a more-specific type
  * using the `cast` method which is implicitly added to this class
  * in the companion object.*/
sealed trait JValue {
  override def toString = io.PrettyJsonWriter.toString(this)

  /** Forces this [[com.rojoma.json.v3.ast.JValue]] to be fully
    * evaluated.  In particular, the compound
    * [[com.rojoma.json.v3.codec.JsonEncode]]s will produce views of their
    * inputs instead of fully-evaluated
    * [[com.rojoma.json.v3.ast.JValue]]s.  This can be problematic if
    * the underlying structure can be mutated before this object is
    * used, or if this object is passed to another thread.
    *
    * What is or is not copied is not defined; the only postcondition
    * is that there are no lazy values left in the returned tree.
    * 
    * @return An equal [[com.rojoma.json.v3.ast.JValue]] with strict values. */
  def forced: JValue

  /** Produces a dynamically typed view of this `JValue` which can be
    * descended using dot-notation for field names or apply-type
    * syntax for arrays.  It can be turned back into a `JValue` with
    * the `!` or `?` methods.
    *
    * Note that certain field-names (the names common to all objects
    * plus `apply`, `applyDynamic`, and `selectDynamic` cannot be accessed
    * with simple field-notation.  Instead, pass them as strings to
    * the `apply` method. */
  @deprecated(message = "Prefer `dyn`", since = "3.1.1")
  def dynamic = new com.rojoma.json.v3.dynamic.DynamicJValue(Some(this))

  /** Produces a dynamically typed view of this `JValue` which can be
    * descended using dot-notation for field names or apply-type
    * syntax for arrays.  It can be turned back into a `JValue` with
    * the `!` or `?` methods.
    *
    * Note that certain field-names (the names common to all `Objects`
    * plus `apply`, `applyDynamic`, and `selectDynamic` cannot be accessed
    * with simple field-notation.  Instead, pass them as strings to
    * the `apply` method. */
  def dyn = com.rojoma.json.v3.dynamic.InformationalDynamicJValue(this)

  /* The concrete type of this value. */
  def jsonType: JsonType
}

object JValue {
  final override val toString = "value"

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
  implicit def toCastable[T <: JValue](x: T) = new `-impl`.ast.DownCaster(x)

  implicit object Concrete extends Json[JValue] {
    val jsonTypes = JAtom.Concrete.jsonTypes ++ JCompound.Concrete.jsonTypes
  }
}

/** A [[com.rojoma.json.v3.ast.JValue]] that is not a [[com.rojoma.json.v3.ast.JNull]]. */
sealed trait JNotNullValue extends JValue

/** A JSON "atom" — anything except arrays or objects.  This and [[com.rojoma.json.v3.ast.JCompound]] form
  * a partition of the set of valid [[com.rojoma.json.v3.ast.JValue]]s. */
sealed abstract class JAtom extends JValue {
  final def forced: this.type = this
}

object JAtom {
  final override val toString = "atom"

  implicit object Concrete extends Json[JAtom] {
    val jsonTypes = Set[JsonType](JNumber, JString, JBoolean, JNull)
  }
}

/** A number. */
sealed abstract class JNumber extends JAtom with JNotNullValue {
  def toByte: Byte
  def toShort: Short
  def toInt: Int
  def toLong: Long
  def toBigInt: BigInt
  def toBigInteger: BigInteger
  def toFloat: Float
  def toDouble: Double
  def toBigDecimal: BigDecimal
  def toJBigDecimal: JBigDecimal

  final def jsonType = JNumber

  override final def toString = asString
  protected def asString: String
  private[v3] def toWriter(w: Writer) = w.write(asString)

  override def equals(o: Any) = o match {
    case that: JNumber => this.toJBigDecimal == that.toJBigDecimal
    case _ => false
  }
  override final lazy val hashCode = toJBigDecimal.hashCode
}

object JNumber extends JsonType {
  final override val toString = "number"

  private val stdCtx = java.math.MathContext.UNLIMITED

  private val doubleWriter = DoubleWriter.doubleWriter

  private class JIntNumber(val toInt: Int) extends JNumber {
    def toByte = toInt.toByte
    def toShort = toInt.toShort
    def toLong = toInt.toLong
    def toBigInt = BigInt(toInt)
    def toBigInteger = BigInteger.valueOf(toInt)

    def toFloat = toInt.toFloat
    def toDouble = toInt.toDouble
    def toBigDecimal = BigDecimal(toInt, stdCtx)
    lazy val toJBigDecimal = new JBigDecimal(toInt, stdCtx)

    def asString = toInt.toString

    override def equals(o: Any) = o match {
      case that: JIntNumber => this.toInt == that.toInt
      case that: JLongNumber => this.toLong == that.toLong
      case that: JBigIntNumber => this.toBigInt == that.toBigInt
      case that: JBigIntegerNumber => this.toBigInteger == that.toBigInteger
      case other => super.equals(other)
    }
  }

  private class JLongNumber(val toLong: Long) extends JNumber {
    def toByte = toLong.toByte
    def toShort = toLong.toShort
    def toInt = toLong.toInt
    def toBigInt = BigInt(toLong)
    def toBigInteger = BigInteger.valueOf(toLong)

    def toFloat = toLong.toFloat
    def toDouble = toLong.toDouble
    def toBigDecimal = BigDecimal(toLong, stdCtx)
    lazy val toJBigDecimal = new JBigDecimal(toLong, stdCtx)

    def asString = toLong.toString

    override def equals(o: Any) = o match {
      case that: JIntNumber => this.toLong == that.toLong
      case that: JLongNumber => this.toLong == that.toLong
      case that: JBigIntNumber => this.toBigInt == that.toBigInt
      case that: JBigIntegerNumber => this.toBigInt == that.toBigInt
      case _ => super.equals(o)
    }
  }

  private class JBigIntNumber(val toBigInt: BigInt) extends JNumber {
    def toByte: Byte = toBigInt.toByte
    def toShort: Short = toBigInt.toShort
    def toInt: Int = toBigInt.toInt
    def toLong: Long = toBigInt.toLong
    def toBigInteger: BigInteger = toBigInt.underlying()

    def toFloat: Float = toBigInt.toFloat
    def toDouble: Double = toBigInt.toDouble
    def toBigDecimal = BigDecimal(toBigInt, stdCtx)
    def toJBigDecimal = new JBigDecimal(toBigInteger, stdCtx)

    def asString = toBigInt.toString

    override def equals(o: Any) = o match {
      case that: JIntNumber => this.toBigInt == that.toBigInt
      case that: JLongNumber => this.toBigInt == that.toBigInt
      case that: JBigIntNumber => this.toBigInt == that.toBigInt
      case that: JBigIntegerNumber => this.toBigInt == that.toBigInt
      case other => super.equals(other)
    }
  }

  private class JBigIntegerNumber(val toBigInteger: BigInteger) extends JNumber {
    def toByte: Byte = toBigInteger.byteValue
    def toShort: Short = toBigInteger.shortValue
    def toInt: Int = toBigInteger.intValue
    def toLong: Long = toBigInteger.longValue
    def toBigInt: BigInt = BigInt(toBigInteger)

    def toFloat: Float = toBigInteger.floatValue
    def toDouble: Double = toBigInteger.doubleValue
    def toBigDecimal = BigDecimal(toBigInt, stdCtx)
    lazy val toJBigDecimal = new JBigDecimal(toBigInteger, stdCtx)

    def asString = toBigInt.toString

    override def equals(o: Any) = o match {
      case that: JIntNumber => this.toBigInteger == that.toBigInteger
      case that: JLongNumber => this.toBigInteger == that.toBigInteger
      case that: JBigIntNumber => this.toBigInteger == that.toBigInteger
      case that: JBigIntegerNumber => this.toBigInteger == that.toBigInteger
      case other => super.equals(other)
    }
  }

  private class JFloatNumber(val toFloat: Float) extends JNumber {
    if(toFloat.isNaN || toFloat.isInfinite) throw JsonInvalidFloat(toFloat)

    def toByte = toFloat.toByte
    def toShort: Short = toFloat.toShort
    def toInt: Int = toFloat.toInt
    def toLong: Long = toFloat.toLong
    def toBigInt: BigInt = toBigDecimal.toBigInt
    def toBigInteger: BigInteger = toJBigDecimal.toBigInteger

    def toDouble: Double = toFloat.toDouble
    def toBigDecimal = BigDecimal(toDouble, stdCtx)
    lazy val toJBigDecimal = new JBigDecimal(toDouble, stdCtx)

    def asString = toFloat.toString
  }

  private class JDoubleNumber(val toDouble: Double) extends JNumber {
    if(toDouble.isNaN || toDouble.isInfinite) throw JsonInvalidDouble(toDouble)

    def toByte: Byte = toDouble.toByte
    def toShort: Short = toDouble.toShort
    def toInt: Int = toDouble.toInt
    def toLong: Long = toDouble.toLong
    def toBigInt: BigInt = toBigDecimal.toBigInt
    def toBigInteger: BigInteger = toJBigDecimal.toBigInteger

    def toFloat: Float = toDouble.toFloat
    def toBigDecimal = BigDecimal(toDouble, stdCtx)
    lazy val toJBigDecimal = new JBigDecimal(toDouble, stdCtx)

    def asString = doubleWriter.toString(toDouble)
    override def toWriter(w: Writer) = doubleWriter.toWriter(w, toDouble)
  }

  private class JBigDecimalNumber(val toBigDecimal: BigDecimal) extends JNumber {
    def toByte: Byte = toBigDecimal.toByte
    def toShort: Short = toBigDecimal.toShort
    def toInt: Int = toBigDecimal.toInt
    def toLong: Long = toBigDecimal.toLong
    def toBigInt: BigInt = toBigDecimal.toBigInt
    def toBigInteger: BigInteger = toJBigDecimal.toBigInteger

    def toFloat: Float = toBigDecimal.toFloat
    def toDouble: Double = toBigDecimal.toDouble
    lazy val toJBigDecimal = toBigDecimal.underlying()

    def asString = toBigDecimal.toString
  }

  private class JJBigDecimalNumber(val toJBigDecimal: JBigDecimal) extends JNumber {
    def toByte: Byte = toJBigDecimal.byteValue
    def toShort: Short = toJBigDecimal.shortValue
    def toInt: Int = toJBigDecimal.intValue
    def toLong: Long = toJBigDecimal.longValue
    def toBigInt: BigInt = BigInt(toBigInteger)
    def toBigInteger: BigInteger = toJBigDecimal.toBigInteger

    def toFloat: Float = toBigDecimal.floatValue
    def toDouble: Double = toBigDecimal.doubleValue
    def toBigDecimal: BigDecimal = BigDecimal(toJBigDecimal)

    def asString = toJBigDecimal.toString
  }

  private class JUncheckedStringNumber(override val asString: String) extends JNumber {
    def toByte: Byte = try { asString.toByte} catch { case _: NumberFormatException => toBigDecimal.toByte }
    def toShort: Short = try { asString.toShort } catch { case _: NumberFormatException => toBigDecimal.toShort }
    def toInt: Int = try { asString.toInt } catch { case _: NumberFormatException => toBigDecimal.toInt }
    def toLong: Long = try { asString.toLong } catch { case _: NumberFormatException => toBigDecimal.toLong }
    def toBigInt = try { BigInt(asString) } catch { case _: NumberFormatException => toBigDecimal.toBigInt }
    def toBigInteger = try { new BigInteger(asString) } catch { case _: NumberFormatException => toJBigDecimal.toBigInteger }

    def toFloat: Float = try { asString.toFloat } catch { case _: NumberFormatException => toBigDecimal.toFloat }
    def toDouble: Double = try { asString.toDouble } catch { case _: NumberFormatException => toBigDecimal.toDouble }
    def toBigDecimal = BigDecimal(asString, stdCtx)
    lazy val toJBigDecimal = new JBigDecimal(asString, stdCtx)
  }

  def apply(b: Byte): JNumber = new JIntNumber(b)
  def apply(s: Short): JNumber = new JIntNumber(s)
  def apply(i: Int): JNumber = new JIntNumber(i)
  def apply(l: Long): JNumber = new JLongNumber(l)
  def apply(bi: BigInt): JNumber = new JBigIntNumber(bi)
  def apply(bi: BigInteger): JNumber = new JBigIntegerNumber(bi)
  def apply(f: Float): JNumber = new JFloatNumber(f)
  def apply(d: Double): JNumber = new JDoubleNumber(d)
  def apply(bd: BigDecimal): JNumber = new JBigDecimalNumber(bd)
  def apply(bd: JBigDecimal): JNumber = new JJBigDecimalNumber(bd)

  def unsafeFromString(s: String): JNumber = new JUncheckedStringNumber(s)

  implicit object Concrete extends Json[JNumber] {
    val jsonTypes = Set[JsonType](JNumber)
  }
}

/** A JSON string.  This does not yet enforce well-formedness with
  * respect to surrogate pairs, but it probably should. */
case class JString(string: String) extends JAtom with JNotNullValue {
  def jsonType = JString
}

object JString extends scala.runtime.AbstractFunction1[String, JString] with JsonType {
  override final val toString = "string"

  implicit object Concrete extends Json[JString] {
    val jsonTypes = Set[JsonType](JString)
  }
}

/** A boolean */
case class JBoolean(boolean: Boolean) extends JAtom with JNotNullValue {
  def jsonType = JBoolean
}

object JBoolean extends scala.runtime.AbstractFunction1[Boolean, JBoolean] with JsonType {
  val canonicalTrue = new JBoolean(true)
  val canonicalFalse = new JBoolean(false)

  def apply(boolean: Boolean) = if(boolean) canonicalTrue else canonicalFalse

  override final val toString = "boolean"

  implicit object Concrete extends Json[JBoolean] {
    val jsonTypes = Set[JsonType](JBoolean)
  }
}

/** Null. */
sealed abstract class JNull extends JAtom // so the object has a nameable type
case object JNull extends JNull with JsonType {
  final override val toString = "null"

  def jsonType = JNull

  implicit object Concrete extends Json[JNull] {
    val jsonTypes = Set[JsonType](JNull)
  }
}

/** The common superclass of arrays and objects.  This and [[com.rojoma.json.v3.ast.JAtom]] form
  * a partition of the set of valid [[com.rojoma.json.v3.ast.JValue]]s. */
sealed trait JCompound extends JNotNullValue {
  def forced: JCompound
  def size: Int
}

object JCompound {
  final override val toString = "compound"

  implicit object Concrete extends Json[JCompound] {
    val jsonTypes = JArray.Concrete.jsonTypes ++ JObject.Concrete.jsonTypes
  }
}

/** A JSON array, which is a `scala.collection.Seq` of JValues. */
case class JArray(elems: sc.Seq[JValue]) extends sc.Seq[JValue] with JCompound {
  override def length = elems.length
  override def apply(idx: Int) = elems(idx)
  override def iterator = elems.iterator

  // overloads of final methods to produce JArrays if statically determinable
  final def ++(suffix: IterableOnce[JValue]) = JArray(elems ++ suffix)
  final def ++:(prefix: IterableOnce[JValue]) = JArray(prefix ++: elems)
  final def +:(elem: JValue) = JArray(elem +: elems)
  final def :+(elem: JValue) = JArray(elems :+ elem)
  final def :++(suffix: IterableOnce[JValue]) = JArray(elems :++ suffix)
  final def concat(suffix: sc.Seq[JValue]) = JArray(elems.concat(suffix))

  // And now we delegate all Seq members to elems, rather than letting
  // default implementations provide them in terms of the above three
  override def appended[B >: JValue](elem: B): sc.Seq[B] = elems.appended(elem)
  final def appended(elem: JValue) = JArray(elems.appended(elem))

  override def appendedAll[B >: JValue](elems: IterableOnce[B]): sc.Seq[B] = this.elems.appendedAll(elems)
  final def appendedAll(elems: IterableOnce[JValue]) = JArray(this.elems.appendedAll(elems))

  override def applyOrElse[A1 <: Int, B1 >: JValue](x: A1, default: A1 => B1) = elems.applyOrElse(x, default)
  override def canEqual(that: Any) = elems.canEqual(that)
  override def collect[B](pf: PartialFunction[JValue, B]) = elems.collect(pf)
  final def collect(pf: PartialFunction[JValue, JValue]) = JArray(elems.collect(pf))
  override def collectFirst[B](pf: PartialFunction[JValue, B]) = elems.collectFirst(pf)
  override def combinations(n: Int): Iterator[JArray] = elems.combinations(n).map(JArray)
  override def compose[R](k: PartialFunction[R, Int]) = elems.compose(k)
  override def contains[A1 >: JValue](elem: A1) = elems.contains(elem)
  override def containsSlice[B](that: sc.Seq[B]) = elems.containsSlice(that)
  override def copyToArray[B >: JValue](xs: Array[B], start: Int, len: Int) = elems.copyToArray(xs, start, len)
  override def copyToArray[B >: JValue](xs: Array[B], start: Int) = elems.copyToArray(xs, start)
  override def copyToArray[B >: JValue](xs: Array[B]) = elems.copyToArray(xs)
  override def corresponds[B](that: sc.Seq[B])(p: (JValue, B) => Boolean) = elems.corresponds(that)(p)
  override def corresponds[B](that: IterableOnce[B])(p: (JValue, B) => Boolean) = elems.corresponds(that)(p)
  override def count(p: JValue => Boolean) = elems.count(p)
  override def diff[B >: JValue](that: sc.Seq[B]) = elems.diff(that)
  override def distinct = elems.distinct
  override def distinctBy[B](f: JValue => B) = elems.distinctBy(f)
  override def drop(n: Int) = JArray(elems.drop(n))
  override def dropRight(n: Int) = JArray(elems.dropRight(n))
  override def dropWhile(p: JValue => Boolean) = JArray(elems.dropWhile(p))
  override def elementWise = elems.elementWise
  override def empty = JArray.empty
  override def endsWith[B >: JValue](that: Iterable[B]) = elems.endsWith(that)
  override def equals(o: Any) = elems.equals(o)
  override def exists(p: JValue => Boolean) = elems.exists(p)
  override def filter(p: JValue => Boolean) = JArray(elems.filter(p))
  override def filterNot(p: JValue => Boolean) = JArray(elems.filterNot(p))
  override def find(p: JValue => Boolean) = elems.find(p)
  override def findLast(p: JValue => Boolean) = elems.findLast(p)
  override def flatMap[B](f: JValue => IterableOnce[B]) = elems.flatMap(f)
  final def flatMap(f: JValue => IterableOnce[JValue]) = JArray(elems.flatMap(f))
  override def flatten[B](implicit asIterable: JValue => IterableOnce[B]) = elems.flatten
  final def flatten[B](implicit asIterable: JValue => IterableOnce[JValue]) = JArray(elems.flatten)
  override def fold[A >: JValue](z: A)(op: (A, A) => A) = elems.fold(z)(op)
  override def foldLeft[B](z: B)(op: (B, JValue) => B) = elems.foldLeft(z)(op)
  override def foldRight[B](z: B)(op: (JValue, B) => B) = elems.foldRight(z)(op)
  override def forall(p: JValue => Boolean) = elems.forall(p)
  override def foreach[U](f: JValue => U) = elems.foreach(f)
  override def groupBy[K](f: JValue => K) = elems.groupBy(f).view.mapValues(JArray).to(SeqMap)
  override def groupMap[K, B](key: JValue => K)(f: JValue => B) = elems.groupMap(key)(f)
  final def groupMap[K](key: JValue => K)(f: JValue => JValue) = elems.groupMap(key)(f).view.mapValues(JArray).to(SeqMap)
  override def groupMapReduce[K, B](key: JValue => K)(f: JValue => B)(reduce: (B, B) => B) = elems.groupMapReduce(key)(f)(reduce)
  override def grouped(size: Int) = elems.grouped(size).map(JArray)
  override def hashCode() = elems.hashCode
  override def head = elems.head
  override def headOption = elems.headOption
  override def indexOf[B >: JValue](elem: B, from: Int) = elems.indexOf(elem, from)
  override def indexOfSlice[B >: JValue](that: sc.Seq[B], from: Int) = elems.indexOfSlice(that, from)
  override def indexWhere(p: JValue => Boolean, from: Int) = elems.indexWhere(p, from)
  override def indices = elems.indices
  override def init = JArray(elems.init)
  override def inits = elems.inits.map(JArray)
  override def intersect[B >: JValue](that: sc.Seq[B]) = JArray(elems.intersect(that))
  override def isDefinedAt(idx: Int) = elems.isDefinedAt(idx)
  override def isEmpty = elems.isEmpty
  override def isTraversableAgain = elems.isTraversableAgain
  override def iterableFactory = elems.iterableFactory
  override def knownSize = elems.knownSize
  override def last = elems.last
  override def lastIndexOf[B >: JValue](elem: B, end: Int = length - 1) = elems.lastIndexOf(elem, end)
  override def lastIndexOfSlice[B >: JValue](that: sc.Seq[B], end: Int) = elems.lastIndexOfSlice(that, end)
  override def lastIndexWhere(p: JValue => Boolean, end: Int) = elems.lastIndexWhere(p, end)
  override def lastOption = elems.lastOption
  // lazyZip we'll allow to be inherited
  override def lengthCompare(that: Iterable[_]) = elems.lengthCompare(that)
  override def lengthCompare(len: Int) = elems.lengthCompare(len)
  override def lift = elems.lift
  override def map[B](f: JValue => B) = elems.map(f)
  final def map(f: JValue => JValue) = JArray(elems.map(f))
  override def max[B >: JValue : math.Ordering] = elems.max[B]
  override def maxBy[B : math.Ordering](f: JValue => B) = elems.maxBy(f)
  override def maxByOption[B : math.Ordering](f: JValue => B) = elems.maxByOption(f)
  override def maxOption[B >: JValue : math.Ordering] = elems.maxOption[B]
  override def min[B >: JValue : math.Ordering] = elems.min[B]
  override def minBy[B : math.Ordering](f: JValue => B) = elems.minBy(f)
  override def minByOption[B : math.Ordering](f: JValue => B) = elems.minByOption(f)
  override def minOption[B >: JValue : math.Ordering] = elems.minOption[B]
  override def orElse[A1 <: Int, B1 >: JValue](that: PartialFunction[A1, B1]) = elems.orElse(that)
  override def padTo[B >: JValue](len: Int, elem: B) = elems.padTo(len, elem)
  final def padTo(len: Int, elem: JValue) = JArray(elems.padTo(len, elem))
  override def partition(p: JValue => Boolean) = {
    val (a, b) = elems.partition(p)
    (JArray(a), JArray(b))
  }
  override def partitionMap[A1, A2](f: JValue => Either[A1, A2]) = elems.partitionMap(f)
  override def patch[B >: JValue](from: Int, other: IterableOnce[B], replaced: Int) = elems.patch(from, other, replaced)
  final def patch(from: Int, other: IterableOnce[JValue], replaced: Int) = JArray(elems.patch(from, other, replaced))
  override def permutations = elems.permutations.map(JArray)
  override def prepended[B >: JValue](elem: B) = elems.prepended(elem)
  final def prepended(elem: JValue) = JArray(elems.prepended(elem))
  override def prependedAll[B >: JValue](prefix: IterableOnce[B]) = elems.prependedAll(prefix)
  final def prependedAll(prefix: IterableOnce[JValue]) = JArray(elems.prependedAll(prefix))
  override def product[B >: JValue : math.Numeric] = elems.product[B]
  override def reduce[B >: JValue](op: (B, B) => B) = elems.reduce(op)
  override def reduceLeft[B >: JValue](op: (B, JValue) => B) = elems.reduceLeft(op)
  override def reduceLeftOption[B >: JValue](op: (B, JValue) => B) = elems.reduceLeftOption(op)
  override def reduceOption[B >: JValue](op: (B, B) => B) = elems.reduceOption(op)
  override def reduceRight[B >: JValue](op: (JValue, B) => B) = elems.reduceRight(op)
  override def reduceRightOption[B >: JValue](op: (JValue, B) => B) = elems.reduceRightOption(op)
  override def reverse = JArray(elems.reverse)
  override def reverseIterator = elems.reverseIterator
  override def runWith[U](action: JValue => U) = elems.runWith(action)
  override def sameElements[B >: JValue](that: IterableOnce[B]) = elems.sameElements(that)
  // scans deliberately do not have JArray-specific overrides because
  // it's not clear to me that it would be a good idea.
  override def scan[B >: JValue](z: B)(op: (B, B) => B) = elems.scan(z)(op)
  override def scanLeft[B](z: B)(op: (B, JValue) => B) = elems.scanLeft(z)(op)
  override def scanRight[B](z: B)(op: (JValue, B) => B) = elems.scanRight(z)(op)
  override def search[B >: JValue : Ordering](elem: B, from: Int, to: Int) = elems.search(elem, from, to)
  override def search[B >: JValue : Ordering](elem: B) = elems.search(elem)
  override def segmentLength(p: JValue => Boolean, from: Int) = elems.segmentLength(p, from)
  override def slice(from: Int, until: Int) = JArray(elems.slice(from, until))
  override def sliding(size: Int, step: Int) = elems.sliding(size, step).map(JArray)
  override def sliding(size: Int) = elems.sliding(size).map(JArray)
  override def sortBy[B : Ordering](f: JValue => B) = JArray(elems.sortBy(f))
  override def sortWith(lt: (JValue, JValue) => Boolean) = JArray(elems.sortWith(lt))
  override def sorted[B >: JValue : Ordering] = elems.sorted[B]
  override def span(p: JValue => Boolean) = {
    val (a, b) = elems.span(p)
    (JArray(a), JArray(b))
  }
  override def splitAt(n: Int) = {
    val (a, b) = elems.splitAt(n)
    (JArray(a), JArray(b))
  }
  override def startsWith[B >: JValue](that: IterableOnce[B], offset: Int = 0) = elems.startsWith(that)
  override def stepper[S <: sc.Stepper[_]](implicit shape: sc.StepperShape[JValue, S]) = elems.stepper[S]
  override def sum[B >: JValue : math.Numeric] = elems.sum[B]
  override def tail = JArray(elems.tail)
  override def tails = elems.tails.map(JArray)
  override def take(n: Int) = JArray(elems.take(n))
  override def takeRight(n: Int) = JArray(elems.takeRight(n))
  override def takeWhile(p: JValue => Boolean) = JArray(elems.takeWhile(p))
  override def tapEach[U](f: JValue => U) = JArray(elems.tapEach(f))
  override def to[C1](factory: sc.Factory[JValue, C1]) = elems.to(factory)
  override def toArray[B >: JValue : ClassTag] = elems.toArray[B]
  override def toIndexedSeq = elems.toIndexedSeq
  override def toList = elems.toList
  override def toMap[K, V](implicit ev: <:<[JValue, (K, V)]) = elems.toMap
  override def toSeq = elems.toSeq
  override def toSet[B >: JValue] = elems.toSet[B]
  override def toVector = elems.toVector
  // transpose also does not have a JArray-producing override deliberately
  override def transpose[B](implicit asIterable: JValue => Iterable[B]) = elems.transpose[B]
  override def unapply(a: Int) = elems.unapply(a)
  override def unzip[A1, A2](implicit asPair: JValue => (A1, A2)) = elems.unzip[A1, A2]
  override def unzip3[A1, A2, A3](implicit asPair: JValue => (A1, A2, A3)) = elems.unzip3[A1, A2, A3]
  override def updated[B >: JValue](index: Int, elem: B) = elems.updated(index, elem)
  final def updated(index: Int, elem: JValue) = JArray(elems.updated(index, elem))
  override def view = elems.view
  override def withFilter(p: JValue => Boolean) = elems.withFilter(p)
  override def zip[B](that: IterableOnce[B]) = elems.zip(that)
  override def zipAll[A1 >: JValue, B](that: Iterable[B], thisElem: A1, thatElem: B) = elems.zipAll(that, thisElem, thatElem)
  override def zipWithIndex = elems.zipWithIndex

  def forced: JArray = {
    // not just "toSeq.map(_forced)" because the seq might be a Stream or view
    val forcedArray: Vector[JValue] =
      elems.view.map(_.forced).toVector
    new JArray(forcedArray) {
      override def forced = this
    }
  }

  def jsonType = JArray
}

object JArray extends scala.runtime.AbstractFunction1[sc.Seq[JValue], JArray] with sc.Factory[JValue, JArray] with JsonType {
  val canonicalEmpty = JArray(Vector.empty) // Vector because JsonReader is guaranteed to return JArrays which contain Vectors.
  val empty = canonicalEmpty

  override final val toString = "array"
  implicit object Concrete extends Json[JArray] {
    val jsonTypes = Set[JsonType](JArray)
  }

  private class JArrayBuilder extends sc.mutable.Builder[JValue, JArray] {
    private val underlying = Vector.newBuilder[JValue]
    override def addOne(elem: JValue) = { underlying.addOne(elem); this }
    override def clear(): Unit = underlying.clear()
    override def result(): JArray = JArray(underlying.result())
  }

  override def fromSpecific(it: IterableOnce[JValue]) = JArray(it.iterator.to(Vector))
  override def newBuilder: sc.mutable.Builder[JValue, JArray] = new JArrayBuilder

  implicit def jarrayFactory[A <: JValue]: sc.Factory[A, JArray] = this
}

/** A JSON object, which is a `scala.collection.Map` from `String` to [[com.rojoma.json.v3.ast.JValue]]. */
case class JObject(val fields: sc.Map[String, JValue]) extends sc.Map[String, JValue] with JCompound {
  override def get(key: String) = fields.get(key)
  override def iterator = fields.iterator

  @deprecated("Use - or removed on an immutable Map", "2.13.0")
  def -(key: String) = JObject(fields - key)
  @deprecated("Use -- or removedAll on an immutable Map", "2.13.0")
  def -(key1: String, key2: String, keys: String*) = JObject(fields-(key1, key2, keys : _*))

  override def ++[V2 >: JValue](xs: IterableOnce[(String, V2)]) = fields ++ xs
  override def andThen[C](k: PartialFunction[JValue, C]) = fields.andThen(k)
  override def andThen[C](k: JValue => C) = fields.andThen(k)
  override def apply(key: String) = fields(key)
  override def applyOrElse[K1 <: String, V1 >: JValue](x: K1, default: K1 => V1) = fields.applyOrElse(x, default)
  override def canEqual(that: Any) = fields.canEqual(that)
  override def collect[K2, V2](pf: PartialFunction[(String, JValue), (K2, V2)]) = fields.collect(pf)
  final def collect(pf: PartialFunction[(String, JValue), (String, JValue)]) = JObject(fields.collect(pf))
  override def collect[B](pf: PartialFunction[(String, JValue), B]) = fields.collect(pf)
  override def collectFirst[B](pf: PartialFunction[(String, JValue), B]) = fields.collectFirst(pf)
  override def compose[A](f: A => String) = fields.compose(f)
  override def concat[V2 >: JValue](suffix: IterableOnce[(String, V2)]) = fields.concat(suffix)
  final def concat[V2 <: JValue](suffix: sc.Map[String, V2]) = JObject(fields.concat(suffix))
  override def concat[B >: (String, JValue)](suffix: IterableOnce[B]) = fields.concat(suffix)
  override def contains(key: String) = fields.contains(key)
  override def copyToArray[B >: (String, JValue)](xs: Array[B], start: Int, len: Int) = fields.copyToArray(xs, start, len)
  override def copyToArray[B >: (String, JValue)](xs: Array[B], start: Int) = fields.copyToArray(xs, start)
  override def copyToArray[B >: (String, JValue)](xs: Array[B]) = fields.copyToArray(xs)
  override def corresponds[B](that: IterableOnce[B])(p: ((String, JValue), B) => Boolean) = fields.corresponds(that)(p)
  override def count(p: ((String, JValue)) => Boolean) = fields.count(p)
  override def default(key: String) = fields.default(key)
  override def drop(n: Int) = JObject(fields.drop(n))
  override def dropRight(n: Int) = JObject(fields.dropRight(n))
  override def dropWhile(p: ((String, JValue)) => Boolean) = JObject(fields.dropWhile(p))
  override def elementWise = fields.elementWise
  override def empty = JObject.empty
  override def equals(o: Any) = fields.equals(o)
  override def exists(p: ((String, JValue)) => Boolean) = fields.exists(p)
  override def filter(pred: ((String, JValue)) => Boolean) = JObject(fields.filter(pred))
  override def filterNot(pred: ((String, JValue)) => Boolean) = JObject(fields.filterNot(pred))
  override def find(p: ((String, JValue)) => Boolean) = fields.find(p)
  override def flatMap[K2, V2](f: ((String, JValue)) => IterableOnce[(K2, V2)]) = fields.flatMap(f)
  final def flatMap(f: ((String, JValue)) => IterableOnce[(String, JValue)]) = JObject(fields.flatMap(f))
  override def flatMap[B](f: ((String, JValue)) => IterableOnce[B]) = fields.flatMap(f)
  override def flatten[B](implicit asIterable: ((String, JValue)) => IterableOnce[B]) = fields.flatten[B]
  override def fold[A >: (String, JValue)](z: A)(op: (A, A) => A) = fields.fold(z)(op)
  override def foldLeft[B](z: B)(op: (B, (String, JValue)) => B) = fields.foldLeft(z)(op)
  override def foldRight[B](z: B)(op: ((String, JValue), B) => B) = fields.foldRight(z)(op)
  override def forall(p: ((String, JValue)) => Boolean) = fields.forall(p)
  override def foreach[U](f: ((String, JValue)) => U) = fields.foreach(f)
  override def foreachEntry[U](f: (String, JValue) => U) = fields.foreachEntry(f)
  override def getOrElse[V1 >: JValue](key: String, default: => V1) = fields.getOrElse(key, default)
  override def groupBy[K](f: ((String, JValue)) => K) = fields.groupBy(f).view.mapValues(JObject).to(SeqMap)
  override def groupMap[K, B](key: ((String, JValue)) => K)(f: ((String, JValue)) => B) = fields.groupMap(key)(f)
  override def groupMapReduce[K, B](key: ((String, JValue)) => K)(f: ((String, JValue)) => B)(reduce: (B, B) => B) = fields.groupMapReduce(key)(f)(reduce)
  override def grouped(size: Int) = fields.grouped(size).map(JObject)
  override def hashCode() = fields.hashCode()
  override def head = fields.head
  override def headOption = fields.headOption
  override def init = JObject(fields.init)
  override def inits = fields.inits.map(JObject)
  override def isDefinedAt(key: String) = fields.isDefinedAt(key)
  override def isEmpty = fields.isEmpty
  override def isTraversableAgain = fields.isTraversableAgain
  override def iterableFactory = fields.iterableFactory
  override def keySet = fields.keySet
  override def keyStepper[S <: sc.Stepper[_]](implicit shape: sc.StepperShape[String, S]) = fields.keyStepper[S]
  override def keys = fields.keys
  override def keysIterator = fields.keysIterator
  override def knownSize = fields.size
  override def last = fields.last
  override def lastOption = fields.lastOption
  // lazyZip gets inherited
  override def lift = fields.lift
  override def map[K2, V2](f: ((String, JValue)) => (K2, V2)) = fields.map(f)
  final def map(f: ((String, JValue)) => (String, JValue)) = JObject(fields.map(f))
  override def map[B](f: ((String, JValue)) => B) = fields.map(f)
  override def mapFactory = fields.mapFactory
  override def max[B >: (String, JValue) : math.Ordering] = fields.max[B]
  override def maxBy[B : math.Ordering](f: ((String, JValue)) => B) = fields.maxBy(f)
  override def maxByOption[B : math.Ordering](f: ((String, JValue)) => B) = fields.maxByOption(f)
  override def maxOption[B >: (String, JValue) : math.Ordering] = fields.maxOption[B]
  override def min[B >: (String, JValue) : math.Ordering] = fields.min[B]
  override def minBy[B : math.Ordering](f: ((String, JValue)) => B) = fields.minBy(f)
  override def minByOption[B : math.Ordering](f: ((String, JValue)) => B) = fields.minByOption(f)
  override def minOption[B >: (String, JValue) : math.Ordering] = fields.minOption[B]
  override def orElse[A1 <: String, B1 >: JValue](that: PartialFunction[A1, B1]) = fields.orElse(that)
  override def partition(p: ((String, JValue)) => Boolean) = {
    val (a, b) = fields.partition(p)
    (JObject(a), JObject(b))
  }
  override def partitionMap[A1, A2](f: ((String, JValue)) => Either[A1, A2]) = fields.partitionMap(f)
  override def product[B >: (String, JValue) : math.Numeric] = fields.product[B]
  override def reduce[B >: (String, JValue)](op: (B, B) => B) = fields.reduce(op)
  override def reduceLeft[B >: (String, JValue)](op: (B, (String, JValue)) => B) = fields.reduceLeft(op)
  override def reduceLeftOption[B >: (String, JValue)](op: (B, (String, JValue)) => B) = fields.reduceLeftOption(op)
  override def reduceOption[B >: (String, JValue)](op: (B, B) => B) = fields.reduceOption(op)
  override def reduceRight[B >: (String, JValue)](op: ((String, JValue), B) => B) = fields.reduceRight(op)
  override def reduceRightOption[B >: (String, JValue)](op: ((String, JValue), B) => B) = fields.reduceRightOption(op)
  override def runWith[U](action: JValue => U) = fields.runWith(action)
  override def scan[B >: (String, JValue)](z: B)(op: (B, B) => B) = fields.scan(z)(op)
  override def scanLeft[B](z: B)(op: (B, (String, JValue)) => B) = fields.scanLeft(z)(op)
  override def scanRight[B](z: B)(op: ((String, JValue), B) => B) = fields.scanRight(z)(op)
  override def size = fields.size
  override def sizeCompare(that: Iterable[_]) = fields.sizeCompare(that)
  override def sizeCompare(otherSize: Int) = fields.sizeCompare(otherSize)
  override def slice(from: Int, until: Int) = JObject(fields.slice(from, until))
  override def sliding(size: Int, step: Int) = fields.sliding(size, step).map(JObject)
  override def sliding(size: Int) = fields.sliding(size).map(JObject)
  override def span(p: ((String, JValue)) => Boolean) = {
    val (a, b) = fields.span(p)
    (JObject(a), JObject(b))
  }
  override def splitAt(n: Int) = {
    val (a, b) = fields.splitAt(n)
    (JObject(a), JObject(b))
  }
  override def stepper[S <: sc.Stepper[_]](implicit shape: sc.StepperShape[(String, JValue), S]) = fields.stepper[S]
  override def sum[B >: (String, JValue) : Numeric] = fields.sum[B]
  override def tail = JObject(fields.tail)
  override def tails = fields.tails.map(JObject)
  override def take(n: Int) = JObject(fields.take(n))
  override def takeRight(n: Int) = JObject(fields.takeRight(n))
  override def takeWhile(p: ((String, JValue)) => Boolean) = JObject(fields.takeWhile(p))
  override def tapEach[U](f: ((String, JValue)) => U) = JObject(fields.tapEach(f))
  override def to[C1](factory: sc.Factory[(String, JValue), C1]) = fields.to(factory)
  override def toArray[B >: (String, JValue) : ClassTag] = fields.toArray[B]
  override def toIndexedSeq = fields.toIndexedSeq
  override def toList = fields.toList
  override def toMap[K, V](implicit ev: <:<[(String, JValue), (K, V)]) = fields.toMap
  override def toSeq = fields.toSeq
  override def toSet[B >: (String, JValue)] = fields.toSet
  override def toVector = fields.toVector
  override def transpose[B](implicit asIterable: ((String, JValue)) => Iterable[B]) = fields.transpose[B]
  override def unapply(a: String) = fields.unapply(a)
  override def unzip[A1, A2](implicit asPair: ((String, JValue)) => ((A1, A2))) = fields.unzip[A1, A2]
  override def unzip3[A1, A2, A3](implicit asPair: ((String, JValue)) => ((A1, A2, A3))) = fields.unzip3[A1, A2, A3]
  override def valueStepper[S <: sc.Stepper[_]](implicit shape: sc.StepperShape[JValue, S]) = fields.valueStepper[S]
  override def values = fields.values
  override def valuesIterator = fields.valuesIterator
  override def view = fields.view
  override def withFilter(p: ((String, JValue)) => Boolean) = fields.withFilter(p)
  override def zip[B](that: IterableOnce[B]) = fields.zip(that)
  override def zipAll[A1 >: (String, JValue), B](that: Iterable[B], thisElem: A1, thatElem: B) = fields.zipAll(that, thisElem, thatElem)
  override def zipWithIndex = fields.zipWithIndex

  def forced: JObject = {
    new JObject(fields.view.mapValues(_.forced).to(SeqMap)) {
      override def forced = this
    }
  }

  def jsonType = JObject
}

object JObject extends scala.runtime.AbstractFunction1[sc.Map[String, JValue], JObject] with sc.Factory[(String, JValue), JObject] with JsonType {
  val canonicalEmpty = JObject(SeqMap.empty) // _Not_ LinkedHashMap because all JsonReader guarantees is ordering of elements, which this satisfies.
  val empty = canonicalEmpty
  override final val toString = "object"
  implicit object Concrete extends Json[JObject] {
    val jsonTypes = Set[JsonType](JObject)
  }

  private class JObjectBuilder extends sc.mutable.Builder[(String, JValue), JObject] {
    private val underlying = SeqMap.newBuilder[String, JValue]
    override def addOne(elem: (String, JValue)) = { underlying.addOne(elem); this }
    override def clear(): Unit = underlying.clear()
    override def result(): JObject = JObject(underlying.result())
  }

  override def fromSpecific(it: IterableOnce[(String, JValue)]) = JObject(it.iterator.to(SeqMap))
  override def newBuilder: sc.mutable.Builder[(String, JValue), JObject] = new JObjectBuilder

  implicit def jobjectFactory[A <: JValue]: sc.Factory[(String, A), JObject] = this
}
