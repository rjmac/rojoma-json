package com.rojoma.json.v3
package codec

import scala.reflect.ClassTag
import java.{util => ju}
import java.{net => jn}

import ast._
import util.WrapperFieldDecode

trait FieldDecode[T] {
  def decode(x: String): Either[DecodeError.InvalidField, T]
}

object FieldDecode {
  def apply[T](using ev: FieldDecode[T]) = ev
  def toString[T](x: String)(using ev: FieldDecode[T]) = ev.decode(x)

  given stringDecode: FieldDecode[String] with {
    def decode(x: String) = Right(x)
  }

  given boolDecode: FieldDecode[Boolean] with {
    def decode(x: String) = x match {
      case "true" => Right(true)
      case "false" => Right(false)
      case other => Left(DecodeError.InvalidField(other))
    }
  }

  given byteDecode: FieldDecode[Byte] = WrapperFieldDecode[Byte](_.toByte)
  given shortDecode: FieldDecode[Short] = WrapperFieldDecode[Short](_.toShort)
  given intDecode: FieldDecode[Int] = WrapperFieldDecode[Int](_.toInt)
  given longDecode: FieldDecode[Long] = WrapperFieldDecode[Long](_.toLong)
  given floatDecode: FieldDecode[Float] = WrapperFieldDecode[Float](_.toFloat)
  given doubleDecode: FieldDecode[Double] = WrapperFieldDecode[Double](_.toDouble)
  given bgintDecode: FieldDecode[BigInt] = WrapperFieldDecode[BigInt](BigInt(_))
  given bigintegerDecode: FieldDecode[java.math.BigInteger] = WrapperFieldDecode[java.math.BigInteger](new java.math.BigInteger(_))
  given bigdecimalDecode: FieldDecode[BigDecimal] = WrapperFieldDecode[BigDecimal](BigDecimal(_, java.math.MathContext.UNLIMITED))
  given jbigdecimalDecode: FieldDecode[java.math.BigDecimal] = WrapperFieldDecode[java.math.BigDecimal](new java.math.BigDecimal(_, java.math.MathContext.UNLIMITED))

  given jstringDecode: FieldDecode[JString] with {
    def decode(x: String) = Right(JString(x))
  }

  given jlEnumDecode[T <: java.lang.Enum[T]](using tag: ClassTag[T]): FieldDecode[T] with {
    def decode(x: String) =
      try {
        Right(java.lang.Enum.valueOf[T](tag.runtimeClass.asInstanceOf[Class[T]], x))
      } catch {
        case _: IllegalArgumentException =>
          Left(DecodeError.InvalidField(x))
      }
  }

  given uuidDecode: FieldDecode[ju.UUID] = WrapperFieldDecode[ju.UUID](ju.UUID.fromString)
  given uriDecode: FieldDecode[jn.URI] = WrapperFieldDecode[jn.URI](jn.URI.create)

  def scalaEnumDecode[T <: Enumeration](e: T): FieldDecode[e.Value] =
    FieldCodec.scalaEnumCodec(e)
}
