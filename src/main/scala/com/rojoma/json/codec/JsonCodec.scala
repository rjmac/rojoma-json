package com.rojoma.json
package codec

import scala.language.higherKinds
import scala.{collection => sc}
import sc.JavaConversions._
import sc.{mutable => scm}
import scala.reflect.ClassTag
import java.{util => ju}

import ast._

trait JsonCodec[T] {
  def encode(x: T): JValue
  def decode(x: JValue): Option[T]
}

private[codec] object CBHolder {
  type CB[A, B] = sc.generic.CanBuild[A, B]
}

/** Generally-useful json implicits. */
object JsonCodec extends com.rojoma.`json-impl`.codec.TupleCodecs {
  import CBHolder._

  def apply[T](implicit a: JsonCodec[T]) = a
  def toJValue[T : JsonCodec](x: T) = JsonCodec[T].encode(x)
  def fromJValue[T : JsonCodec](x: JValue) = JsonCodec[T].decode(x)

  // I would like all of these codecs' encode methods to use view, but unfortunately in scala 2.8.1,
  // empty views and non-views are not symmetrically comparable (Nil.view == Nil, but Nil != Nil.view).  This
  // is fixed in 2.9.

  implicit def seqCodec[T, S[X] <: sc.Seq[X]](implicit tCodec: JsonCodec[T], buildFactory: CB[T, S[T]]) = new JsonCodec[S[T]] {
    def encode(x: S[T]): JValue = {
      if(x.nonEmpty)
        JArray(x.view.map(tCodec.encode))
      else
        JArray(Nil)
    }

    def decode(xs: JValue): Option[S[T]] = xs match {
      case JArray(jElems) =>
        val builder = buildFactory()
        for (jElem <- jElems) {
          tCodec.decode(jElem) match {
            case Some(elem) =>
              builder += elem
            case None =>
              return None
          }
        }
        Some(builder.result())
      case _ =>
        None
    }
  }

  implicit def arrayCodec[T: JsonCodec: ClassTag] = new JsonCodec[Array[T]] {
    def encode(x: Array[T]): JValue =
      if(x.length > 0)
        JArray(x.view.map(JsonCodec[T].encode))
      else
        JArray(Nil)

    def decode(xs: JValue): Option[Array[T]] = xs match {
      case JArray(jElems) =>
        val builder = scm.ArrayBuilder.make[T]()
        val subCodec = JsonCodec[T]
        for(jElem <- jElems) {
          subCodec.decode(jElem) match {
            case Some(elem) =>
              builder += elem
            case None =>
              return None
          }
        }
        Some(builder.result())
      case _ =>
        None
    }
  }

  implicit def juListCodec[T: JsonCodec] = new JsonCodec[ju.List[T]] {
    def encode(x: ju.List[T]): JValue = {
      if(!x.isEmpty)
        JArray(x.view.map(JsonCodec[T].encode))
      else
        JArray(Nil)
    }

    def decode(xs: JValue): Option[ju.List[T]] = xs match {
      case JArray(jElems) =>
        val result = new ju.ArrayList[T]
        val subCodec = JsonCodec[T]
        for(jElem <- jElems) {
          subCodec.decode(jElem) match {
            case Some(elem) =>
              result.add(elem)
            case None =>
              return None
          }
        }
        Some(result)
      case _ =>
        None
    }
  }

  implicit object stringCodec extends JsonCodec[String] {
    def encode(x: String) = JString(x)
    def decode(x: JValue) = x match {
      case JString(s) => Some(s)
      case _ => None
    }
  }

  implicit object boolCodec extends JsonCodec[Boolean] {
    def encode(x: Boolean) = JBoolean(x)
    def decode(x: JValue) = x match {
      case JBoolean(b) => Some(b)
      case _ => None
    }
  }

  implicit object byteCodec extends JsonCodec[Byte] {
    def encode(x: Byte) = JNumber(x.toLong)
    def decode(x: JValue) = x match {
      case JNumber(num) => Some(num.toByte)
      case _ => None
    }
  }

  implicit object shortCodec extends JsonCodec[Short] {
    def encode(x: Short) = JNumber(x.toLong)
    def decode(x: JValue) = x match {
      case JNumber(num) => Some(num.toShort)
      case _ => None
    }
  }

  implicit object intCodec extends JsonCodec[Int] {
    def encode(x: Int) = JNumber(x.toLong)
    def decode(x: JValue) = x match {
      case JNumber(num) => Some(num.toInt)
      case _ => None
    }
  }

  implicit object longCodec extends JsonCodec[Long] {
    def encode(x: Long) = JNumber(x)
    def decode(x: JValue) = x match {
      case JNumber(num) => Some(num.toLong)
      case _ => None
    }
  }

  implicit object bigintCodec extends JsonCodec[BigInt] {
    def encode(x: BigInt) = JNumber(x)
    def decode(x: JValue) = x match {
      case JNumber(num) => Some(num.toBigInt)
      case _ => None
    }
  }

  implicit object bigintegerCodec extends JsonCodec[java.math.BigInteger] {
    def encode(x: java.math.BigInteger) = JNumber(new BigInt(x))
    def decode(x: JValue) = x match {
      case JNumber(num) => Some(num.toBigInt.underlying)
      case _ => None
    }
  }

  implicit object floatCodec extends JsonCodec[Float] {
    def encode(x: Float) = JNumber(x)
    def decode(x: JValue) = x match {
      case JNumber(num) => Some(num.toFloat)
      case _ => None
    }
  }

  implicit object doubleCodec extends JsonCodec[Double] {
    def encode(x: Double) = JNumber(x)
    def decode(x: JValue) = x match {
      case JNumber(num) => Some(num.toDouble)
      case _ => None
    }
  }

  implicit object bigdecimalCodec extends JsonCodec[BigDecimal] {
    def encode(x: BigDecimal) = JNumber(x)
    def decode(x: JValue) = x match {
      case JNumber(num) => Some(num)
      case _ => None
    }
  }

  implicit object jbigdecimalCodec extends JsonCodec[java.math.BigDecimal] {
    def encode(x: java.math.BigDecimal) = JNumber(BigDecimal(x))
    def decode(x: JValue) = x match {
      case JNumber(num) => Some(num.underlying)
      case _ => None
    }
  }

  implicit def jvalueCodec[T <: JValue : ClassTag] = new JsonCodec[T] {
    def encode(x: T) = x
    def decode(x: JValue) = x.cast[T]
  }

  implicit def mapCodec[T, M[U, V] <: sc.Map[U, V]](implicit tCodec: JsonCodec[T], buildFactory: CB[(String, T), M[String, T]]) = new JsonCodec[M[String, T]] {
    def encode(x: M[String, T]) =
      JObject(x.mapValues(tCodec.encode))

    def decode(x: JValue): Option[M[String, T]] = x match {
      case JObject(fields) =>
        val builder = buildFactory()
        for((k, jv) <- fields) {
          tCodec.decode(jv) match {
            case Some(v) => builder += (k -> v)
            case None => return None
          }
        }
        Some(builder.result())
      case _ =>
        None
    }
  }

  implicit def juMapCodec[T: JsonCodec] = new JsonCodec[ju.Map[String, T]] {
    def encode(x: ju.Map[String, T]) =
      JObject(x.mapValues(JsonCodec[T].encode))

    def decode(x: JValue): Option[ju.Map[String, T]] = x match {
      case JObject(fields) =>
        val result = new ju.LinkedHashMap[String, T]
        val subCodec = JsonCodec[T]
        for((k, jv) <- fields) {
          subCodec.decode(jv) match {
            case Some(v) => result.put(k, v)
            case None => return None
          }
        }
        Some(result)
      case _ =>
        None
    }
  }

  // either is right-biased; if decoding as Right fails it tries Left;
  // if Left fails the whole thing fails.
  implicit def eitherCodec[L: JsonCodec, R: JsonCodec] = new JsonCodec[Either[L, R]] {
    def encode(x: Either[L,R]) = x match {
      case Left(left) => JsonCodec[L].encode(left)
      case Right(right) => JsonCodec[R].encode(right)
    }

    def decode(x: JValue) = 
      JsonCodec[R].decode(x) match {
        case Some(right) => Some(Right(right))
        case None =>
          JsonCodec[L].decode(x) match {
            case Some(left) => Some(Left(left))
            case None => None
          }
      }
  }

  implicit def jlEnumCodec[T <: java.lang.Enum[T]](implicit tag: ClassTag[T]) = new JsonCodec[T] {
    def encode(x: T) = JString(x.name)
    def decode(x: JValue) = x match {
      case JString(s) =>
        try {
          Some(java.lang.Enum.valueOf[T](tag.runtimeClass.asInstanceOf[Class[T]], s))
        } catch {
          case _: IllegalArgumentException =>
            None
        }
      case _ =>
        None
    }
  }

  implicit object UnitCodec extends JsonCodec[Unit] {
    val empty = io.JsonReader("[]").read()
    def encode(x: Unit) = empty
    def decode(x: JValue) = x match {
      case JArray(Seq()) => Some(())
      case _ => None
    }
  }
}
