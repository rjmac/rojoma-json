package com.rojoma.json.v3
package codec

import scala.language.higherKinds
import scala.{collection => sc}
import sc.JavaConversions._
import sc.{mutable => scm}
import scala.reflect.ClassTag
import java.{util => ju}

import ast._

trait JsonDecode[T] {
  def decode(x: JValue): JsonDecode.DecodeResult[T]
}

private[codec] object CBHolder {
  type CB[A, B] = sc.generic.CanBuild[A, B]
}

sealed trait DecodeError {
  def path: Path
  def augment(parent: Path.Entry): DecodeError
}

object DecodeError {
  case class Multiple(choices: Iterable[DecodeError], path: Path) extends DecodeError {
    def augment(parent: Path.Entry) = copy(path = path.prepend(parent))
  }
  case class InvalidType(expected: JsonType, got: JsonType, path: Path) extends DecodeError {
    def augment(parent: Path.Entry) = copy(path = path.prepend(parent))
  }
  case class InvalidValue(got: JValue, path: Path) extends DecodeError {
    def augment(parent: Path.Entry) = copy(path = path.prepend(parent))
  }
}

/** Generally-useful json implicits. */
object JsonDecode  extends com.rojoma.json.v3.`-impl`.codec.TupleDecode {
  import CBHolder._

  type DecodeResult[T] = Either[DecodeError, T]

  def apply[T](implicit a: JsonDecode[T]): a.type = a
  def fromJValue[T : JsonDecode](x: JValue) = JsonDecode[T].decode(x)

  implicit def seqDecode[T, S[X] <: sc.Seq[X]](implicit tDecode: JsonDecode[T], buildFactory: CB[T, S[T]]) = new JsonDecode[S[T]] {
    def decode(xs: JValue): DecodeResult[S[T]] = xs match {
      case JArray(jElems) =>
        val builder = buildFactory()
        for ((jElem, idx) <- jElems.iterator.zipWithIndex) {
          tDecode.decode(jElem) match {
            case Right(elem) =>
              builder += elem
            case Left(err) =>
              return Left(err.augment(Path.Index(idx)))
          }
        }
        Right(builder.result())
      case other =>
        Left(DecodeError.InvalidType(JArray, other.jsonType, Path.empty))
    }
  }

  implicit def arrayDecode[T: JsonDecode: ClassTag] = new JsonDecode[Array[T]] {
    def decode(xs: JValue): DecodeResult[Array[T]] = xs match {
      case JArray(jElems) =>
        val builder = scm.ArrayBuilder.make[T]()
        val subDecode = JsonDecode[T]
        for((jElem, idx) <- jElems.iterator.zipWithIndex) {
          subDecode.decode(jElem) match {
            case Right(elem) =>
              builder += elem
            case Left(err) =>
              return Left(err.augment(Path.Index(idx)))
          }
        }
        Right(builder.result())
      case other =>
        Left(DecodeError.InvalidType(JArray, other.jsonType, Path.empty))
    }
  }

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
              return Left(err.augment(Path.Index(idx)))
          }
        }
        Right(result)
      case other =>
        Left(DecodeError.InvalidType(JArray, other.jsonType, Path.empty))
    }
  }

  implicit object stringDecode extends JsonDecode[String] {
    def decode(x: JValue) = x match {
      case JString(s) => Right(s)
      case other => Left(DecodeError.InvalidType(JString, other.jsonType, Path.empty))
    }
  }

  implicit object boolDecode extends JsonDecode[Boolean] {
    def decode(x: JValue) = x match {
      case JBoolean(b) => Right(b)
      case other => Left(DecodeError.InvalidType(JBoolean, other.jsonType, Path.empty))
    }
  }

  implicit object byteDecode extends JsonDecode[Byte] {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toByte)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType, Path.empty))
    }
  }

  implicit object shortDecode extends JsonDecode[Short] {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toShort)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType, Path.empty))
    }
  }

  implicit object intDecode extends JsonDecode[Int] {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toInt)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType, Path.empty))
    }
  }

  implicit object longDecode extends JsonDecode[Long] {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toLong)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType, Path.empty))
    }
  }

  implicit object bigintDecode extends JsonDecode[BigInt] {
    def encode(x: BigInt) = JNumber(x)
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toBigInt)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType, Path.empty))
    }
  }

  implicit object bigintegerDecode extends JsonDecode[java.math.BigInteger] {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toBigInt.underlying)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType, Path.empty))
    }
  }

  implicit object floatDecode extends JsonDecode[Float] {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toFloat)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType, Path.empty))
    }
  }

  implicit object doubleDecode extends JsonDecode[Double] {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toDouble)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType, Path.empty))
    }
  }

  implicit object bigdecimalDecode extends JsonDecode[BigDecimal] {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toBigDecimal)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType, Path.empty))
    }
  }

  implicit object jbigdecimalDecode extends JsonDecode[java.math.BigDecimal] {
    def decode(x: JValue) = x match {
      case num: JNumber => Right(num.toBigDecimal.underlying)
      case other => Left(DecodeError.InvalidType(JNumber, other.jsonType, Path.empty))
    }
  }

  implicit def jvalueDecode[T <: JValue : Json] = new JsonDecode[T] {
    def decode(x: JValue) = x.cast[T] match {
      case Some(j) =>
        Right(j)
      case None =>
        val choices = implicitly[Json[T]].jsonTypes
        Left(DecodeError.Multiple(choices.toSeq.map(DecodeError.InvalidType(_, x.jsonType, Path.empty)), Path.empty))
    }
  }

  implicit def mapDecode[T, M[U, V] <: sc.Map[U, V]](implicit tDecode: JsonDecode[T], buildFactory: CB[(String, T), M[String, T]]) = new JsonDecode[M[String, T]] {
    def decode(x: JValue): DecodeResult[M[String, T]] = x match {
      case JObject(fields) =>
        val builder = buildFactory()
        for((k, jv) <- fields) {
          tDecode.decode(jv) match {
            case Right(v) => builder += (k -> v)
            case Left(err) => return Left(err.augment(Path.Field(k)))
          }
        }
        Right(builder.result())
      case other =>
        Left(DecodeError.InvalidType(JObject, other.jsonType, Path.empty))
    }
  }

  implicit def juMapDecode[T: JsonDecode] = new JsonDecode[ju.Map[String, T]] {
    def decode(x: JValue): DecodeResult[ju.Map[String, T]] = x match {
      case JObject(fields) =>
        val result = new ju.LinkedHashMap[String, T]
        val subDecode = JsonDecode[T]
        for((k, jv) <- fields) {
          subDecode.decode(jv) match {
            case Right(v) => result.put(k, v)
            case Left(err) => return Left(err.augment(Path.Field(k)))
          }
        }
        Right(result)
      case other =>
        Left(DecodeError.InvalidType(JObject, other.jsonType, Path.empty))
    }
  }

  // either is right-biased; if decoding as Right fails it tries Left;
  // if Left fails the whole thing fails.
  implicit def eitherDecode[L: JsonDecode, R: JsonDecode] = new JsonDecode[Either[L, R]] {
    def decode(x: JValue) = 
      JsonDecode[R].decode(x) match {
        case Right(right) => Right(Right(right))
        case Left(err1) =>
          JsonDecode[L].decode(x) match {
            case Right(left) => Right(Left(left))
            case Left(err2) => Left(DecodeError.Multiple(Seq(err1, err2), Path.empty))
          }
      }
  }

  implicit def jlEnumDecode[T <: java.lang.Enum[T]](implicit tag: ClassTag[T]) = new JsonDecode[T] {
    def decode(x: JValue) = x match {
      case JString(s) =>
        try {
          Right(java.lang.Enum.valueOf[T](tag.runtimeClass.asInstanceOf[Class[T]], s))
        } catch {
          case _: IllegalArgumentException =>
            Left(DecodeError.InvalidValue(x, Path.empty))
        }
      case other =>
        Left(DecodeError.InvalidType(JString, other.jsonType, Path.empty))
    }
  }

  implicit object UnitDecode extends JsonDecode[Unit] {
    def decode(x: JValue) = x match {
      case JArray(xs) if xs.isEmpty => Right(())
      case nonEmpty: JArray => Left(DecodeError.InvalidValue(nonEmpty, Path.empty))
      case other => Left(DecodeError.InvalidType(JArray, other.jsonType, Path.empty))
    }
  }
}
