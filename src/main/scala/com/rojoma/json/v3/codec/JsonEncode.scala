package com.rojoma.json.v3
package codec

import scala.{collection => sc}
import scala.jdk.CollectionConverters._
import scala.reflect.ClassTag
import java.{util => ju}
import java.{net => jn}

import `-impl`.{MappedViewSeq, MappedViewMap}
import ast._
import util.WrapperJsonEncode

trait JsonEncode[T] {
  def encode(x: T): JValue
}

/** Generally-useful json implicits. */
object JsonEncode extends com.rojoma.json.v3.`-impl`.codec.TupleEncode {
  def apply[T](using a: JsonEncode[T]): a.type = a
  def toJValue[T : JsonEncode](x: T): JValue = JsonEncode[T].encode(x)

  given seqEncode[T, S[X] <: sc.Seq[X]](using tEncode: JsonEncode[T]): JsonEncode[S[T]] with {
    def encode(x: S[T]): JValue = {
      if(x.nonEmpty)
        JArray(x.view.map(tEncode.encode).toVector)
      else
        JArray.empty
    }
  }

  given arrayEncode[T: JsonEncode: ClassTag]: JsonEncode[Array[T]] with {
    def encode(x: Array[T]): JValue =
      if(x.length > 0)
        JArray(x.view.map(JsonEncode[T].encode).toVector)
      else
        JArray.empty
  }

  given setEncode[T, S[X] <: sc.Set[X]](using tEncode: JsonEncode[T]): JsonEncode[S[T]] with {
    def encode(x: S[T]): JValue = {
      if(x.nonEmpty)
        JArray(x.iterator.map(tEncode.encode).toVector)
      else
        JArray.empty
    }
  }

  given juListEncode[T: JsonEncode]: JsonEncode[ju.List[T]] with {
    def encode(x: ju.List[T]): JValue = {
      if(!x.isEmpty)
        JArray(x.asScala.view.map(JsonEncode[T].encode).toVector)
      else
        JArray.empty
    }
  }

  given juSetEncode[T](using tEncode: JsonEncode[T]): JsonEncode[ju.Set[T]] with {
    def encode(x: ju.Set[T]): JValue = {
      if(!x.isEmpty)
        JArray(new MappedViewSeq(x.asScala.toSeq, tEncode.encode))
      else
        JArray.empty
    }
  }

  given stringEncode: JsonEncode[String] with {
    def encode(x: String) = JString(x)
  }

  given boolEncode: JsonEncode[Boolean] with {
    private val jtrue = JBoolean.canonicalTrue
    private val jfalse = JBoolean.canonicalFalse
    def encode(x: Boolean) = if(x) jtrue else jfalse
  }

  given jbooleanEncode: JsonEncode[java.lang.Boolean] with {
    private val jtrue = JBoolean.canonicalTrue
    private val jfalse = JBoolean.canonicalFalse
    def encode(x: java.lang.Boolean) = if(x) jtrue else jfalse
  }

  given byteEncode: JsonEncode[Byte] with {
    def encode(x: Byte) = JNumber(x)
  }

  given jbyteEncode: JsonEncode[java.lang.Byte] with {
    def encode(x: java.lang.Byte) = JNumber(x)
  }

  given shortEncode: JsonEncode[Short] with {
    def encode(x: Short) = JNumber(x)
  }

  given jshortEncode: JsonEncode[java.lang.Short] with {
    def encode(x: java.lang.Short) = JNumber(x)
  }

  given intEncode: JsonEncode[Int] with {
    def encode(x: Int) = JNumber(x)
  }

  given jintegerEncode: JsonEncode[java.lang.Integer] with {
    def encode(x: java.lang.Integer) = JNumber(x)
  }

  given longEncode: JsonEncode[Long] with {
    def encode(x: Long) = JNumber(x)
  }

  given jlongEncode: JsonEncode[java.lang.Long] with {
    def encode(x: java.lang.Long) = JNumber(x)
  }

  given bigintEncode: JsonEncode[BigInt] with {
    def encode(x: BigInt) = JNumber(x)
  }

  given bigintegerEncode: JsonEncode[java.math.BigInteger] with {
    def encode(x: java.math.BigInteger) = JNumber(new BigInt(x))
  }

  given floatEncode: JsonEncode[Float] with {
    def encode(x: Float) = JNumber(x)
  }

  given jfloatEncode: JsonEncode[java.lang.Float] with {
    def encode(x: java.lang.Float) = JNumber(x)
  }

  given doubleEncode: JsonEncode[Double] with {
    def encode(x: Double) = JNumber(x)
  }

  given jdoubleEncode: JsonEncode[java.lang.Double] with {
    def encode(x: java.lang.Double) = JNumber(x)
  }

  given bigdecimalEncode: JsonEncode[BigDecimal] with {
    def encode(x: BigDecimal) = JNumber(x)
  }

  given jbigdecimalEncode: JsonEncode[java.math.BigDecimal] with {
    def encode(x: java.math.BigDecimal) = JNumber(BigDecimal(x))
  }

  given jvalueEncode[T <: JValue]: JsonEncode[T] with {
    def encode(x: T) = x
  }

  given fieldMapEncode[T, U, M[A, B] <: sc.Map[A, B]](using tEncode: FieldEncode[T], uEncode: JsonEncode[U]): JsonEncode[M[T,U]] = {
    if(tEncode eq FieldEncode.stringEncode) {
      // common enough to have its own impl, since it can be done more cheaply
      // We're taking advantage of the fact that modifying the thing that was
      // encoded has undefined effects on the encodee up to ".forced".
      new JsonEncode[M[String, U]] {
        def encode(x: M[String, U]) =
          if(x.nonEmpty) JObject(new MappedViewMap(x, uEncode.encode))
          else JObject.empty
      }.asInstanceOf[JsonEncode[M[T, U]]]
    } else {
      new JsonEncode[M[T, U]] {
        def encode(x: M[T, U]) =
          if(x.nonEmpty) {
            // In keeping with the general philosophy of compound encoders,
            // we'll encode as lazily as possible.
            val keysConverted: sc.Map[String, U] = x.map { case (k, v) => tEncode.encode(k) -> v }
            JObject(new MappedViewMap(keysConverted, uEncode.encode))
          } else JObject.empty
      }
    }
  }

  given fieldJuMapEncode[T, U](using tEncode: FieldEncode[T], uEncode: JsonEncode[U]): JsonEncode[ju.Map[T,U]] with {
    private val scalaCodec = fieldMapEncode[T, U, sc.Map]
    def encode(x: ju.Map[T, U]) = scalaCodec.encode(x.asScala)
  }

  // either is right-biased; if decoding as Right fails it tries Left;
  // if Left fails the whole thing fails.
  given eitherEncode[L: JsonEncode, R: JsonEncode]: JsonEncode[Either[L, R]] with {
    def encode(x: Either[L,R]) = x match {
      case Left(left) => JsonEncode[L].encode(left)
      case Right(right) => JsonEncode[R].encode(right)
    }
  }

  given jlEnumEncode[T <: java.lang.Enum[T]]: JsonEncode[T] with {
    def encode(x: T) = JString(x.name)
  }

  given UnitEncode: JsonEncode[Unit] with {
    private val empty = JArray.empty
    def encode(x: Unit) = empty
  }

  given uuidEncode: JsonEncode[ju.UUID] = WrapperJsonEncode[ju.UUID](_.toString)
  given uriEncode: JsonEncode[jn.URI] = WrapperJsonEncode[jn.URI](_.toString)

  def scalaEnumEncode[T <: Enumeration](e: T): JsonEncode[e.Value] =
    JsonCodec.scalaEnumCodec(e)
}
