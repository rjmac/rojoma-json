package com.rojoma.json.v3
package codec

import java.{util => ju}
import java.{net => jn}

import ast._

trait FieldEncode[T] {
  def encode(x: T): String
}

object FieldEncode {
  def apply[T](using ev: FieldEncode[T]) = ev
  def toField[T](x: T)(using ev: FieldEncode[T]) = ev.encode(x)

  given stringEncode: FieldEncode[String] with {
    def encode(x: String) = x
  }

  given boolEncode: FieldEncode[Boolean] with {
    def encode(x: Boolean) = if(x) "true" else "false"
  }

  given byteEncode: FieldEncode[Byte] with {
    def encode(x: Byte) = x.toString
  }

  given shortEncode: FieldEncode[Short] with {
    def encode(x: Short) = x.toString
  }

  given intEncode: FieldEncode[Int] with {
    def encode(x: Int) = x.toString
  }

  given longEncode: FieldEncode[Long] with {
    def encode(x: Long) = x.toString
  }

  given floatEncode: FieldEncode[Float] with {
    def encode(x: Float) = x.toString
  }

  given doubleEncode: FieldEncode[Double] with {
    def encode(x: Double) = x.toString
  }

  given bgintEncode: FieldEncode[BigInt] with {
    def encode(x: BigInt) = x.toString
  }

  given bigintegerEncode: FieldEncode[java.math.BigInteger] with {
    def encode(x: java.math.BigInteger) = x.toString
  }

  given bigdecimalEncode: FieldEncode[BigDecimal] with {
    def encode(x: BigDecimal) = x.toString
  }

  given jbigdecimalEncode: FieldEncode[java.math.BigDecimal] with {
    def encode(x: java.math.BigDecimal) = x.toString
  }

  given jstringEncode: FieldEncode[JString] with {
    def encode(x: JString) = x.string
  }

  given jlEnumEncode[T <: java.lang.Enum[T]]: FieldEncode[T] with {
    def encode(x: T) = x.name
  }

  given uuidEncode: FieldEncode[ju.UUID] with {
    def encode(x: ju.UUID) = x.toString
  }

  given uriEncode: FieldEncode[jn.URI] with {
    def encode(x: jn.URI) = x.toString
  }

  def scalaEnumEncode[T <: Enumeration](e: T): FieldEncode[e.Value] =
    FieldCodec.scalaEnumCodec(e)
}
