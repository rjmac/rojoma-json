package com.rojoma.json.v3
package codec

import scala.{collection => sc}
import scala.reflect.ClassTag
import java.{util => ju}
import java.{net => jn}

import ast._
import util.{WrapperJsonDecode, JsonCaseInsensitiveEnum}

trait JsonDecode[T] {
  def decode(x: JValue): JsonDecode.DecodeResult[T]
}

/** Generally-useful json implicits. */
object JsonDecode extends com.rojoma.json.v3.`-impl`.codec.TupleDecode {
  private type CB[A, B] = sc.Factory[A, B]
  type DecodeResult[T] = Either[DecodeError, T]

  def apply[T](using a: JsonDecode[T]): a.type = a
  def fromJValue[T : JsonDecode](x: JValue) = JsonDecode[T].decode(x)

  private class IterableDecode[T, S](tDecode: JsonDecode[T], buildFactory: CB[T, S]) extends JsonDecode[S] {
    def decode(xs: JValue): DecodeResult[S] = xs match {
      case JArray(jElems) =>
        val builder = buildFactory.newBuilder
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

  given seqDecode[T, S[X] <: sc.Seq[X]](using tDecode: JsonDecode[T], buildFactory: CB[T, S[T]]): JsonDecode[S[T]] =
    new IterableDecode(tDecode, buildFactory)

  given arrayDecode[T](using tDecode: JsonDecode[T], ct: ClassTag[T]): JsonDecode[Array[T]] =
    new IterableDecode(tDecode, implicitly[CB[T, Array[T]]])

  given setDecode[T, S[U] <: sc.Set[U]](using tCodec: JsonDecode[T], buildFactory: CB[T, S[T]]): JsonDecode[S[T]] =
    new IterableDecode(tCodec, buildFactory)

  given juListDecode[T: JsonDecode]: JsonDecode[ju.List[T]] with {
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

  given juSetDecode[T : JsonDecode]: JsonDecode[ju.Set[T]] with {
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

  given stringDecode: JsonDecode[String] with {
    def decode(x: JValue) = x match {
      case JString(s) => Right(s)
      case other => Left(DecodeError.InvalidType(JString, other.jsonType))
    }
  }

  given boolDecode: JsonDecode[Boolean] with {
    def decode(x: JValue) = x match {
      case JBoolean(b) => Right(b)
      case other => Left(DecodeError.InvalidType(JBoolean, other.jsonType))
    }
  }

  given jbooleanDecode: JsonDecode[java.lang.Boolean] with {
    def decode(x: JValue) = x match {
      case JBoolean(b) => Right(b)
      case other => Left(DecodeError.InvalidType(JBoolean, other.jsonType))
    }
  }

  given byteDecode: JsonDecode[Byte] with {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toByte)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  given jbyteDecode: JsonDecode[java.lang.Byte] with {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toByte)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  given shortDecode: JsonDecode[Short] with {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toShort)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  given jshortDecode: JsonDecode[java.lang.Short] with {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toShort)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  given intDecode: JsonDecode[Int] with {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toInt)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  given jintegerDecode: JsonDecode[java.lang.Integer] with {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toInt)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  given longDecode: JsonDecode[Long] with {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toLong)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  given jlongDecode: JsonDecode[java.lang.Long] with {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toLong)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  given bigintDecode: JsonDecode[BigInt] with {
    def encode(x: BigInt) = JNumber(x)
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toBigInt)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  given bigintegerDecode: JsonDecode[java.math.BigInteger] with {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toBigInt.underlying)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  given floatDecode: JsonDecode[Float] with {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toFloat)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  given jfloatDecode: JsonDecode[java.lang.Float] with {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toFloat)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  given doubleDecode: JsonDecode[Double] with {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toDouble)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  given jdoubleDecode: JsonDecode[java.lang.Double] with {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toDouble)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  given bigdecimalDecode: JsonDecode[BigDecimal] with {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toBigDecimal)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  given jbigdecimalDecode: JsonDecode[java.math.BigDecimal] with {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toBigDecimal.underlying)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType))
    }
  }

  given jvalueDecode[T <: JValue : Json]: JsonDecode[T] with {
    def decode(x: JValue) = x.cast[T] match {
      case Some(j) =>
        Right(j)
      case None =>
        val choices = implicitly[Json[T]].jsonTypes
        Left(DecodeError.join(choices.map(DecodeError.InvalidType(_, x.jsonType))))
    }
  }

  given fieldMapDecode[T, U, M[A, B] <: sc.Map[A, B]](using tDecode: FieldDecode[T], uDecode: JsonDecode[U], buildFactory: CB[(T, U), M[T, U]]): JsonDecode[M[T, U]] with {
    def decode(x: JValue): DecodeResult[M[T, U]] = x match {
      case JObject(fields) =>
        val builder = buildFactory.newBuilder
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

  given fieldJuMapDecode[T, U](using tDecode: FieldDecode[T], uDecode: JsonDecode[U]): JsonDecode[ju.Map[T, U]] with {
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

  // either is right-biased; if decoding as Right fails it tries Left;
  // if Left fails the whole thing fails.
  given eitherDecode[L: JsonDecode, R: JsonDecode]: JsonDecode[Either[L, R]] with {
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

  given jlEnumDecode[T <: java.lang.Enum[T]](using tag: ClassTag[T]): JsonDecode[T] =
    if(tag.runtimeClass.isAnnotationPresent(classOf[JsonCaseInsensitiveEnum])) {
      new JsonDecode[T] {
        val nameMap =
          tag.
            runtimeClass.
            asInstanceOf[Class[T]].
            getMethod("values").
            invoke(null).
            asInstanceOf[Array[T]].
            iterator.
            map { e => e.name.toLowerCase -> e }.
            toMap

        def decode(x: JValue) = x match {
          case str@JString(s) =>
            nameMap.get(s.toLowerCase) match {
              case Some(e) => Right(e)
              case None => Left(DecodeError.InvalidValue(str))
            }
          case other =>
            Left(DecodeError.InvalidType(JString, other.jsonType))
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

  given UnitDecode: JsonDecode[Unit] with {
    def decode(x: JValue) = x match {
      case JArray(xs) if xs.isEmpty => Right(())
      case nonEmpty: JArray => Left(DecodeError.InvalidLength(0, nonEmpty.length))
      case other => Left(DecodeError.InvalidType(JArray, other.jsonType))
    }
  }

  given uuidDecode: JsonDecode[ju.UUID] = WrapperJsonDecode[ju.UUID](ju.UUID.fromString)
  given uriDecode: JsonDecode[jn.URI] = WrapperJsonDecode[jn.URI](jn.URI.create)

  def scalaEnumDecode[T <: Enumeration](e: T): JsonDecode[e.Value] =
    JsonCodec.scalaEnumCodec(e)
}
