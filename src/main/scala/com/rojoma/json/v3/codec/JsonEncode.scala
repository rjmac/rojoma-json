package com.rojoma.json.v3
package codec

import scala.language.higherKinds
import scala.{collection => sc}
import sc.JavaConverters._
import scala.reflect.ClassTag
import java.{util => ju}
import java.{net => jn}

import ast._
import util.{WrapperJsonEncode, Strategy, JsonEnumStrategy}

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
        JArray.empty
    }
  }

  implicit def arrayEncode[T: JsonEncode: ClassTag] = new JsonEncode[Array[T]] {
    def encode(x: Array[T]): JValue =
      if(x.length > 0)
        JArray(x.view.map(JsonEncode[T].encode))
      else
        JArray.empty
  }

  implicit def setEncode[T, S[X] <: sc.Set[X]](implicit tEncode: JsonEncode[T]) = new JsonEncode[S[T]] {
    def encode(x: S[T]): JValue = {
      if(x.nonEmpty)
        JArray(x.toSeq.view.map(tEncode.encode))
      else
        JArray.empty
    }
  }

  implicit def juListEncode[T: JsonEncode] = new JsonEncode[ju.List[T]] {
    def encode(x: ju.List[T]): JValue = {
      if(!x.isEmpty)
        JArray(x.asScala.view.map(JsonEncode[T].encode))
      else
        JArray.empty
    }
  }

  implicit def juSetEncode[T: JsonEncode] = new JsonEncode[ju.Set[T]] {
    def encode(x: ju.Set[T]): JValue = {
      if(!x.isEmpty)
        JArray(x.asScala.toSeq.view.map(JsonEncode[T].encode))
      else
        JArray.empty
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

  implicit object jbooleanEncode extends JsonEncode[java.lang.Boolean] {
    private val jtrue = JBoolean.canonicalTrue
    private val jfalse = JBoolean.canonicalFalse
    def encode(x: java.lang.Boolean) = if(x) jtrue else jfalse
  }

  implicit object byteEncode extends JsonEncode[Byte] {
    def encode(x: Byte) = JNumber(x)
  }

  implicit object jbyteEncode extends JsonEncode[java.lang.Byte] {
    def encode(x: java.lang.Byte) = JNumber(x)
  }

  implicit object shortEncode extends JsonEncode[Short] {
    def encode(x: Short) = JNumber(x)
  }

  implicit object jshortEncode extends JsonEncode[java.lang.Short] {
    def encode(x: java.lang.Short) = JNumber(x)
  }

  implicit object intEncode extends JsonEncode[Int] {
    def encode(x: Int) = JNumber(x)
  }

  implicit object jintegerEncode extends JsonEncode[java.lang.Integer] {
    def encode(x: java.lang.Integer) = JNumber(x)
  }

  implicit object longEncode extends JsonEncode[Long] {
    def encode(x: Long) = JNumber(x)
  }

  implicit object jlongEncode extends JsonEncode[java.lang.Long] {
    def encode(x: java.lang.Long) = JNumber(x)
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

  implicit object jfloatEncode extends JsonEncode[java.lang.Float] {
    def encode(x: java.lang.Float) = JNumber(x)
  }

  implicit object doubleEncode extends JsonEncode[Double] {
    def encode(x: Double) = JNumber(x)
  }

  implicit object jdoubleEncode extends JsonEncode[java.lang.Double] {
    def encode(x: java.lang.Double) = JNumber(x)
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

  implicit def fieldMapEncode[T, U, M[A, B] <: sc.Map[A, B]](implicit tEncode: FieldEncode[T], uEncode: JsonEncode[U]): JsonEncode[M[T,U]] = {
    if(tEncode eq FieldEncode.stringEncode) {
      // common enough to have its own impl, since it can be done more cheaply
      // We're taking advantage of the fact that modifying the thing that was
      // encoded has undefined effects on the encodee up to ".forced".
      new JsonEncode[M[String, U]] {
        def encode(x: M[String, U]) =
          if(x.nonEmpty) JObject(x.mapValues(uEncode.encode))
          else JObject.empty
      }.asInstanceOf[JsonEncode[M[T, U]]]
    } else {
      new JsonEncode[M[T, U]] {
        def encode(x: M[T, U]) =
          if(x.nonEmpty) {
            // In keeping with the general philosophy of compound encoders,
            // we'll encode as lazily as possible.
            val keysConverted: sc.Map[String, U] = x.map { case (k, v) => tEncode.encode(k) -> v }
            JObject(keysConverted.mapValues(uEncode.encode))
          } else JObject.empty
      }
    }
  }

  implicit def fieldJuMapEncode[T, U](implicit tEncode: FieldEncode[T], uEncode: JsonEncode[U]): JsonEncode[ju.Map[T,U]] = new JsonEncode[ju.Map[T, U]] {
    val scalaCodec = fieldMapEncode[T, U, sc.Map]
    def encode(x: ju.Map[T, U]) = scalaCodec.encode(x.asScala)
  }

  @deprecated(message = "Use fieldMapEncode instead", since="3.2.0")
  def mapEncode[T, M[U, V] <: sc.Map[U, V]](implicit tEncode: JsonEncode[T]) = fieldMapEncode[String, T, M]

  @deprecated(message = "Use fieldJuMapEncode instead", since="3.2.0")
  def juMapEncode[T: JsonEncode] = fieldJuMapEncode[String, T]

  // either is right-biased; if decoding as Right fails it tries Left;
  // if Left fails the whole thing fails.
  implicit def eitherEncode[L: JsonEncode, R: JsonEncode] = new JsonEncode[Either[L, R]] {
    def encode(x: Either[L,R]) = x match {
      case Left(left) => JsonEncode[L].encode(left)
      case Right(right) => JsonEncode[R].encode(right)
    }
  }

  @deprecated(message = "Use jlEnumEncode2 instead", since = "3.14.0")
  def jlEnumEncode[T <: java.lang.Enum[T]] = new JsonEncode[T] {
    @volatile private var trueEncode: JsonEncode[T] = null

    def encode(x: T) = {
      var e = trueEncode
      if(e == null) {
        e = new EnumCodec[T](x.getClass)
        trueEncode = e
      }
      e.encode(x)
    }
  }

  private class EnumCodec[T <: java.lang.Enum[T]](cls: Class[_]) extends JsonEncode[T] {
    private val converter: T => String = locally {
      val ann = cls.getAnnotation(classOf[JsonEnumStrategy])
      if(ann == null) {
        _.name
      } else ann.value match {
        case Strategy.Identity =>
          _.name
        case Strategy.Underscore =>
          val converted =
            cls.getMethod("values").invoke(null).asInstanceOf[Array[T]].map { item =>
              `-impl`.util.CamelSplit(item.name).map(_.toLowerCase).mkString("_").intern()
            }
          t => converted(t.ordinal)
      }
    }

    def encode(x: T) = {
      JString(converter(x))
    }
  }

  implicit def jlEnumEncode2[T <: java.lang.Enum[T]](implicit ev: ClassTag[T]): JsonEncode[T] =
    new EnumCodec[T](ev.runtimeClass)

  implicit object UnitEncode extends JsonEncode[Unit] {
    private val empty = JArray.empty
    def encode(x: Unit) = empty
  }

  implicit val uuidEncode = WrapperJsonEncode[ju.UUID](_.toString)
  implicit val uriEncode = WrapperJsonEncode[jn.URI](_.toString)

  def scalaEnumEncode[T <: Enumeration](enum: T): JsonEncode[enum.Value] =
    JsonCodec.scalaEnumCodec(enum)
}
