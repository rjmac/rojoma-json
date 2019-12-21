package com.rojoma.json.v3
package `-impl`.util

import ast._
import codec._

private[util] class WrapperDecode[U, T: JsonDecode](wrap: T => U) extends JsonDecode[U] {
  private def wrapHandler(j: JValue, x: T): Either[DecodeError, U] = {
    try {
      Right(wrap(x))
    } catch {
      case e: IllegalArgumentException =>
        Left(DecodeError.InvalidValue(j))
    }
  }

  def decode(x: JValue) =
    JsonDecode[T].decode(x) match {
      case Right(v) => wrapHandler(x, v)
      case Left(err) => Left(err)
    }
}

class WrapperJsonCodecImpl[U] {
  def apply[T : JsonEncode : JsonDecode](wrap: T => U, unwrap: U => T): JsonEncode[U] with JsonDecode[U] =
    new WrapperDecode(wrap) with JsonEncode[U] {
      def encode(x: U) = JsonEncode[T].encode(unwrap(x))
    }
}

class WrapperJsonEncodeImpl[U] {
  def apply[T : JsonEncode](unwrap: U => T): JsonEncode[U] =
    new JsonEncode[U] {
      def encode(x: U) = JsonEncode[T].encode(unwrap(x))
    }
}

class WrapperJsonDecodeImpl[U] {
  def apply[T : JsonDecode](wrap: T => U): JsonDecode[U] =
    new WrapperDecode(wrap)
}
