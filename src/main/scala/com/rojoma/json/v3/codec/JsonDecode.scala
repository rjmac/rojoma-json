package com.rojoma.json.v3
package codec

import scala.language.higherKinds
import scala.{collection => sc}
import scala.reflect.ClassTag
import java.{util => ju}
import java.{net => jn}

import ast._
import util.{WrapperJsonDecode, JsonCaseInsensitiveEnum, JsonEnumStrategy, Strategy}

trait JsonDecode[T] {
  def decode(x: JValue): JsonDecode.DecodeResult[T]
}

/** Generally-useful json implicits. */
object JsonDecode  extends com.rojoma.json.v3.`-impl`.codec.TupleDecode {
  private type CB[A, B] = sc.generic.CanBuild[A, B]
  type DecodeResult[T] = Either[DecodeError, T]

  def apply[T](implicit a: JsonDecode[T]): a.type = a
  def fromJValue[T : JsonDecode](x: JValue) = JsonDecode[T].decode(x)

  private class IterableDecode[T, S](tDecode: JsonDecode[T], buildFactory: CB[T, S]) extends JsonDecode[S] {
    def decode(xs: JValue): DecodeResult[S] = xs match {
      case JArray(jElems) =>
        val builder = buildFactory()
        for ((jElem, idx) <- jElems.iterator.zipWithIndex) {
          tDecode.decode(jElem) match {
            case Right(elem) =>
              builder += elem
            case Left(err) =>
              return Left(err.prefix(idx))
          }
        }
        Right(builder.result())
      case other =>
        Left(DecodeError.InvalidType(JArray, other.jsonType))
    }
  }

  implicit def seqDecode[T, S[X] <: sc.Seq[X]](implicit tDecode: JsonDecode[T], buildFactory: CB[T, S[T]]): JsonDecode[S[T]] =
    new IterableDecode(tDecode, buildFactory)

  implicit def arrayDecode[T](implicit tDecode: JsonDecode[T], ct: ClassTag[T]): JsonDecode[Array[T]] =
    new IterableDecode(tDecode, implicitly[CB[T, Array[T]]])

  implicit def setDecode[T, S[U] <: sc.Set[U]](implicit tCodec: JsonDecode[T], buildFactory: CB[T, S[T]]): JsonDecode[S[T]] =
    new IterableDecode(tCodec, buildFactory)

  implicit def juListDecode[T: JsonDecode] = new JsonDecode[ju.List[T]] {
    def decode(xs: JValue): DecodeResult[ju.List[T]] = xs match {
      case JArray(jElems) =>
        val result = new ju.ArrayList[T](jElems.length)
        val subDecode = JsonDecode[T]
        for((jElem, idx) <- jElems.iterator.zipWithIndex) {
          subDecode.decode(jElem) match {
            case Right(elem) =>
              result.add(elem)
            case Left(err) =>
              return Left(err.prefix(idx))
          }
        }
        Right(result)
      case other =>
        Left(DecodeError.InvalidType(JArray, other.jsonType))
    }
  }

  implicit def juSetDecode[T : JsonDecode] = new JsonDecode[ju.Set[T]] {
    def decode(xs: JValue): DecodeResult[ju.Set[T]] = xs match {
      case JArray(jElems) =>
        val result = new ju.LinkedHashSet[T]
        val subDecode = JsonDecode[T]
        for((jElem, idx) <- jElems.iterator.zipWithIndex) {
          subDecode.decode(jElem) match {
            case Right(elem) =>
              result.add(elem)
            case Left(err) =>
              return Left(err.prefix(idx))
          }
        }
        Right(result)
      case other =>
        Left(DecodeError.InvalidType(JArray, other.jsonType))
    }
  }

  implicit object stringDecode extends JsonDecode[String] {
    def decode(x: JValue) = x match {
      case JString(s) => Right(s)
      case other => Left(DecodeError.InvalidType(JString, other.jsonType))
    }
  }

  implicit object boolDecode extends JsonDecode[Boolean] {
    def decode(x: JValue) = x match {
      case JBoolean(b) => Right(b)
      case other => Left(DecodeError.InvalidType(JBoolean, other.jsonType))
    }
  }

  implicit object jbooleanDecode extends JsonDecode[java.lang.Boolean] {
    def decode(x: JValue) = x match {
      case JBoolean(b) => Right(b)
      case other => Left(DecodeError.InvalidType(JBoolean, other.jsonType))
    }
  }

  implicit object byteDecode extends JsonDecode[Byte] {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toByte)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  implicit object jbyteDecode extends JsonDecode[java.lang.Byte] {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toByte)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  implicit object shortDecode extends JsonDecode[Short] {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toShort)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  implicit object jshortDecode extends JsonDecode[java.lang.Short] {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toShort)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  implicit object intDecode extends JsonDecode[Int] {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toInt)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  implicit object jintegerDecode extends JsonDecode[java.lang.Integer] {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toInt)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  implicit object longDecode extends JsonDecode[Long] {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toLong)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  implicit object jlongDecode extends JsonDecode[java.lang.Long] {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toLong)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  implicit object bigintDecode extends JsonDecode[BigInt] {
    def encode(x: BigInt) = JNumber(x)
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toBigInt)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  implicit object bigintegerDecode extends JsonDecode[java.math.BigInteger] {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toBigInt.underlying)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  implicit object floatDecode extends JsonDecode[Float] {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toFloat)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  implicit object jfloatDecode extends JsonDecode[java.lang.Float] {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toFloat)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  implicit object doubleDecode extends JsonDecode[Double] {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toDouble)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  implicit object jdoubleDecode extends JsonDecode[java.lang.Double] {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toDouble)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  implicit object bigdecimalDecode extends JsonDecode[BigDecimal] {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toBigDecimal)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  implicit object jbigdecimalDecode extends JsonDecode[java.math.BigDecimal] {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toBigDecimal.underlying)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  implicit def jvalueDecode[T <: JValue : Json] = new JsonDecode[T] {
    def decode(x: JValue) = x.cast[T] match {
      case Some(j) =>
        Right(j)
      case None =>
        val choices = implicitly[Json[T]].jsonTypes
        Left(DecodeError.join(choices.map(DecodeError.InvalidType(_, x.jsonType))))
    }
  }

  implicit def fieldMapDecode[T, U, M[A, B] <: sc.Map[A, B]](implicit tDecode: FieldDecode[T], uDecode: JsonDecode[U], buildFactory: CB[(T, U), M[T, U]]) = new JsonDecode[M[T, U]] {
    def decode(x: JValue): DecodeResult[M[T, U]] = x match {
      case JObject(fields) =>
        val builder = buildFactory()
        for((kv, jv) <- fields) {
          tDecode.decode(kv) match {
            case Right(k) =>
              uDecode.decode(jv) match {
                case Right(v) => builder += (k -> v)
                case Left(err) => return Left(err.prefix(kv))
              }
            case Left(err) =>
              return Left(err)
          }
        }
        Right(builder.result())
      case other =>
        Left(DecodeError.InvalidType(JObject, other.jsonType))
    }
  }

  implicit def fieldJuMapDecode[T, U](implicit tDecode: FieldDecode[T], uDecode: JsonDecode[U]) = new JsonDecode[ju.Map[T, U]] {
    def decode(x: JValue): DecodeResult[ju.Map[T, U]] = x match {
      case JObject(fields) =>
        val result = new ju.LinkedHashMap[T, U]
        for((kv, jv) <- fields) {
          tDecode.decode(kv) match {
            case Right(k) =>
              uDecode.decode(jv) match {
                case Right(v) => result.put(k, v)
                case Left(err) => return Left(err.prefix(kv))
              }
            case Left(err) =>
              return Left(err)
          }
        }
        Right(result)
      case other =>
        Left(DecodeError.InvalidType(JObject, other.jsonType))
    }
  }

  @deprecated(message = "Use fieldMapEncode instead", since="3.2.0")
  def mapDecode[T, M[U, V] <: sc.Map[U, V]](implicit tDecode: JsonDecode[T], buildFactory: CB[(String, T), M[String, T]]) =
    fieldMapDecode[String, T, M]

  @deprecated(message = "Use fieldJuMapEncode instead", since="3.2.0")
  def juMapDecode[T: JsonDecode] = fieldJuMapDecode[String, T]

  // either is right-biased; if decoding as Right fails it tries Left;
  // if Left fails the whole thing fails.
  implicit def eitherDecode[L: JsonDecode, R: JsonDecode] = new JsonDecode[Either[L, R]] {
    def decode(x: JValue) =
      JsonDecode[R].decode(x) match {
        case Right(right) => Right(Right(right))
        case Left(err1) =>
          JsonDecode[L].decode(x) match {
            case Right(left) => Right(Left(left))
            case Left(err2) =>
              Left(DecodeError.join(Seq(err1, err2)))
          }
      }
  }

  implicit def jlEnumDecode[T <: java.lang.Enum[T]](implicit tag: ClassTag[T]) = {
    val isUnderscoreized = Option(tag.runtimeClass.getAnnotation(classOf[JsonEnumStrategy])).
      fold(false) { ann =>
        ann.value() match {
          case Strategy.Underscore =>
            true
          case Strategy.Identity =>
            false
        }
      }
    val isCaseInsensitive = tag.runtimeClass.isAnnotationPresent(classOf[JsonCaseInsensitiveEnum])
    if(isUnderscoreized || isCaseInsensitive) {
      val nameMap =
        tag.
          runtimeClass.
          asInstanceOf[Class[T]].
          getMethod("values").
          invoke(null).
          asInstanceOf[Array[T]].
          iterator.
          map { e =>
            val name =
              if(isUnderscoreized) `-impl`.util.CamelSplit(e.name).mkString("_").toLowerCase.intern()
              else if(isCaseInsensitive) e.name.toLowerCase.intern()
              else e.name
            name -> e
          }.
          toMap

      if(isCaseInsensitive) {
        new JsonDecode[T] {
          def decode(x: JValue) = x match {
            case str@JString(s) =>
              nameMap.get(s.toLowerCase) match {
                case Some(e) ⇒ Right(e)
                case None ⇒ Left(DecodeError.InvalidValue(str))
              }
            case other =>
              Left(DecodeError.InvalidType(JString, other.jsonType))
          }
        }
      } else {
        new JsonDecode[T] {
          def decode(x: JValue) = x match {
            case str@JString(s) =>
              nameMap.get(s) match {
                case Some(e) ⇒ Right(e)
                case None ⇒ Left(DecodeError.InvalidValue(str))
              }
            case other =>
              Left(DecodeError.InvalidType(JString, other.jsonType))
          }
        }
      }
    } else {
      new JsonDecode[T] {
        def decode(x: JValue) = x match {
          case str@JString(s) =>
            try {
              Right(java.lang.Enum.valueOf[T](tag.runtimeClass.asInstanceOf[Class[T]], s))
            } catch {
              case _: IllegalArgumentException =>
                Left(DecodeError.InvalidValue(str))
            }
          case other =>
            Left(DecodeError.InvalidType(JString, other.jsonType))
        }
      }
    }
  }

  implicit object UnitDecode extends JsonDecode[Unit] {
    def decode(x: JValue) = x match {
      case JArray(xs) if xs.isEmpty => Right(())
      case nonEmpty: JArray => Left(DecodeError.InvalidLength(0, nonEmpty.length))
      case other => Left(DecodeError.InvalidType(JArray, other.jsonType))
    }
  }

  implicit val uuidDecode = WrapperJsonDecode[ju.UUID](ju.UUID.fromString)
  implicit val uriDecode = WrapperJsonDecode[jn.URI](jn.URI.create)

  def scalaEnumDecode[T <: Enumeration](enum: T): JsonDecode[enum.Value] =
    JsonCodec.scalaEnumCodec(enum)
}
