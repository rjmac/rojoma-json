package com.rojoma.json.v3
package codec

import scala.language.higherKinds
import scala.{collection => sc}
import sc.JavaConverters._
import sc.{mutable => scm}
import scala.reflect.ClassTag
import java.{util => ju}

import ast._

trait JsonEncode[T] {
  def encode(x: T): JValue
}

/** Generally-useful json implicits. */
object JsonEncode extends com.rojoma.json.v3.`-impl`.codec.TupleEncode {
  def apply[T](implicit a: JsonEncode[T]): a.type = a
  def toJValue[T : JsonEncode](x: T): JValue = JsonEncode[T].encode(x)

  implicit def seqEncode[T, S[X] <: sc.Seq[X]](implicit tEncode: JsonEncode[T]) = new JsonEncode[S[T]] {
    def encode(x: S[T]): JValue = {
      if(x.nonEmpty)
        JArray(x.view.map(tEncode.encode))
      else
        JArray.canonicalEmpty
    }
  }

  implicit def arrayEncode[T: JsonEncode: ClassTag] = new JsonEncode[Array[T]] {
    def encode(x: Array[T]): JValue =
      if(x.length > 0)
        JArray(x.view.map(JsonEncode[T].encode))
      else
        JArray.canonicalEmpty
  }

  implicit def juListEncode[T: JsonEncode] = new JsonEncode[ju.List[T]] {
    def encode(x: ju.List[T]): JValue = {
      if(!x.isEmpty)
        JArray(x.asScala.view.map(JsonEncode[T].encode))
      else
        JArray.canonicalEmpty
    }
  }

  implicit object stringEncode extends JsonEncode[String] {
    def encode(x: String) = JString(x)
  }

  implicit object boolEncode extends JsonEncode[Boolean] {
    private val jtrue = JBoolean.canonicalTrue
    private val jfalse = JBoolean.canonicalFalse
    def encode(x: Boolean) = if(x) jtrue else jfalse
  }

  implicit object byteEncode extends JsonEncode[Byte] {
    def encode(x: Byte) = JNumber(x)
  }

  implicit object shortEncode extends JsonEncode[Short] {
    def encode(x: Short) = JNumber(x)
  }

  implicit object intEncode extends JsonEncode[Int] {
    def encode(x: Int) = JNumber(x)
  }

  implicit object longEncode extends JsonEncode[Long] {
    def encode(x: Long) = JNumber(x)
  }

  implicit object bigintEncode extends JsonEncode[BigInt] {
    def encode(x: BigInt) = JNumber(x)
  }

  implicit object bigintegerEncode extends JsonEncode[java.math.BigInteger] {
    def encode(x: java.math.BigInteger) = JNumber(new BigInt(x))
  }

  implicit object floatEncode extends JsonEncode[Float] {
    def encode(x: Float) = JNumber(x)
  }

  implicit object doubleEncode extends JsonEncode[Double] {
    def encode(x: Double) = JNumber(x)
  }

  implicit object bigdecimalEncode extends JsonEncode[BigDecimal] {
    def encode(x: BigDecimal) = JNumber(x)
  }

  implicit object jbigdecimalEncode extends JsonEncode[java.math.BigDecimal] {
    def encode(x: java.math.BigDecimal) = JNumber(BigDecimal(x))
  }

  implicit def jvalueEncode[T <: JValue] = new JsonEncode[T] {
    def encode(x: T) = x
  }

  implicit def mapEncode[T, M[U, V] <: sc.Map[U, V]](implicit tEncode: JsonEncode[T]) = new JsonEncode[M[String, T]] {
    def encode(x: M[String, T]) =
      if(x.nonEmpty) JObject(x.mapValues(tEncode.encode))
      else JObject.canonicalEmpty
  }

  implicit def juMapEncode[T: JsonEncode] = new JsonEncode[ju.Map[String, T]] {
    def encode(x: ju.Map[String, T]) =
      if(!x.isEmpty) JObject(x.asScala.mapValues(JsonEncode[T].encode))
      else JObject.canonicalEmpty
  }

  // either is right-biased; if decoding as Right fails it tries Left;
  // if Left fails the whole thing fails.
  implicit def eitherEncode[L: JsonEncode, R: JsonEncode] = new JsonEncode[Either[L, R]] {
    def encode(x: Either[L,R]) = x match {
      case Left(left) => JsonEncode[L].encode(left)
      case Right(right) => JsonEncode[R].encode(right)
    }
  }

  implicit def jlEnumEncode[T <: java.lang.Enum[T]] = new JsonEncode[T] {
    def encode(x: T) = JString(x.name)
  }

  implicit object UnitEncode extends JsonEncode[Unit] {
    private val empty = JArray.canonicalEmpty
    def encode(x: Unit) = empty
  }
}
