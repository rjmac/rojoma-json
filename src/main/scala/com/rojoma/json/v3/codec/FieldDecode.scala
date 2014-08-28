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
  def apply[T](implicit ev: FieldDecode[T]) = ev
  def toString[T](x: String)(implicit ev: FieldDecode[T]) = ev.decode(x)

  implicit object stringDecode extends FieldDecode[String] {
    def decode(x: String) = Right(x)
  }

  implicit object boolDecode extends FieldDecode[Boolean] {
    def decode(x: String) = x match {
      case "true" => Right(true)
      case "false" => Right(false)
      case other => Left(DecodeError.InvalidField(other))
    }
  }

  implicit val byteDecode = WrapperFieldDecode[Byte](_.toByte)
  implicit val shortDecode = WrapperFieldDecode[Short](_.toShort)
  implicit val intDecode = WrapperFieldDecode[Int](_.toInt)
  implicit val longDecode = WrapperFieldDecode[Long](_.toLong)
  implicit val floatDecode = WrapperFieldDecode[Float](_.toFloat)
  implicit val doubleDecode = WrapperFieldDecode[Double](_.toDouble)
  implicit val bgintDecode = WrapperFieldDecode[BigInt](BigInt(_))
  implicit val bigintegerDecode = WrapperFieldDecode[java.math.BigInteger](new java.math.BigInteger(_))
  implicit val bigdecimalDecode = WrapperFieldDecode[BigDecimal](BigDecimal(_, java.math.MathContext.UNLIMITED))
  implicit val jbigdecimalDecode = WrapperFieldDecode[java.math.BigDecimal](new java.math.BigDecimal(_, java.math.MathContext.UNLIMITED))

  implicit object jstringDecode extends FieldDecode[JString] {
    def decode(x: String) = Right(JString(x))
  }

  implicit def jlEnumDecode[T <: java.lang.Enum[T]](implicit tag: ClassTag[T]) = new FieldDecode[T] {
    def decode(x: String) = 
      try {
        Right(java.lang.Enum.valueOf[T](tag.runtimeClass.asInstanceOf[Class[T]], x))
      } catch {
        case _: IllegalArgumentException =>
          Left(DecodeError.InvalidField(x))
      }
  }

  implicit val uuidDecode = WrapperFieldDecode[ju.UUID](ju.UUID.fromString)
  implicit val uriDecode = WrapperFieldDecode[jn.URI](jn.URI.create)
}
