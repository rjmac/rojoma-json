package com.rojoma.json.v3
package util

import ast.JValue
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

/** Creates a combined [[com.rojoma.json.v3.codec.JsonEncode]]
  * and [[com.rojoma.json.v3.codec.JsonDecode]] for a simple wrapper type.
  * The `wrap` function may throw `IllegalArgumentException`; this
  * is translated to a [[com.rojoma.json.v3.codec.DecodeError.InvalidValue]].
  */
object WrapperJsonCodec {
  def apply[U, T : JsonEncode : JsonDecode](wrap: T => U, unwrap: U => T): JsonEncode[U] with JsonDecode[U] =
    new WrapperDecode(wrap) with JsonEncode[U] {
      def encode(x: U) = JsonEncode[T].encode(unwrap(x))
    }
}

/** Creates a [[com.rojoma.json.v3.codec.JsonEncode]] for a simple wrapper type.
  */
object WrapperJsonEncode {
  def apply[U, T : JsonEncode](unwrap: U => T): JsonEncode[U] =
    new JsonEncode[U] {
      def encode(x: U) = JsonEncode[T].encode(unwrap(x))
    }
}

/** Creates a [[com.rojoma.json.v3.codec.JsonDecode]] for a simple wrapper type.
  * The `wrap` function may throw `IllegalArgumentException`; this
  * is translated to a [[com.rojoma.json.v3.codec.DecodeError.InvalidValue]].
  */
object WrapperJsonDecode {
  def apply[U, T : JsonDecode](wrap: T => U): JsonDecode[U] =
    new WrapperDecode(wrap)
}
