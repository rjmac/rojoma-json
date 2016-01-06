package com.rojoma.json.v3
package codec

import java.{util => ju}
import java.{net => jn}

import ast._
import util.WrapperFieldEncode

trait FieldEncode[T] {
  def encode(x: T): String
}

object FieldEncode {
  def apply[T](implicit ev: FieldEncode[T]) = ev
  def toField[T](x: T)(implicit ev: FieldEncode[T]) = ev.encode(x)

  implicit object stringEncode extends FieldEncode[String] {
    def encode(x: String) = x
  }

  implicit object boolEncode extends FieldEncode[Boolean] {
    def encode(x: Boolean) = if(x) "true" else "false"
  }

  implicit object byteEncode extends FieldEncode[Byte] {
    def encode(x: Byte) = x.toString
  }

  implicit object shortEncode extends FieldEncode[Short] {
    def encode(x: Short) = x.toString
  }

  implicit object intEncode extends FieldEncode[Int] {
    def encode(x: Int) = x.toString
  }

  implicit object longEncode extends FieldEncode[Long] {
    def encode(x: Long) = x.toString
  }

  implicit object floatEncode extends FieldEncode[Float] {
    def encode(x: Float) = x.toString
  }

  implicit object doubleEncode extends FieldEncode[Double] {
    def encode(x: Double) = x.toString
  }

  implicit object bgintEncode extends FieldEncode[BigInt] {
    def encode(x: BigInt) = x.toString
  }

  implicit object bigintegerEncode extends FieldEncode[java.math.BigInteger] {
    def encode(x: java.math.BigInteger) = x.toString
  }

  implicit object bigdecimalEncode extends FieldEncode[BigDecimal] {
    def encode(x: BigDecimal) = x.toString
  }

  implicit object jbigdecimalEncode extends FieldEncode[java.math.BigDecimal] {
    def encode(x: java.math.BigDecimal) = x.toString
  }

  implicit object jstringEncode extends FieldEncode[JString] {
    def encode(x: JString) = x.toString
  }

  implicit def jlEnumEncode[T <: java.lang.Enum[T]] = new FieldEncode[T] {
    def encode(x: T) = x.name
  }

  implicit object uuidEncode extends FieldEncode[ju.UUID] {
    def encode(x: ju.UUID) = x.toString
  }

  implicit object uriEncode extends FieldEncode[jn.URI] {
    def encode(x: jn.URI) = x.toString
  }

  def scalaEnumEncode[T <: Enumeration](enum: T): FieldEncode[enum.Value] =
    FieldCodec.scalaEnumCodec(enum)
}
